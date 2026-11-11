// blockr.script prose block: markdown text with inline R, edited as it reads.
//
// One JS controller owns the markdown (inline R as `r expr`, Quarto's own
// form) and drives a Milkdown editor, with the raw markdown in a collapsed
// section under it. Sync is client-side and guarded against echo loops.
//
// Two clocks:
//   - The inline EXPRESSIONS go to R eagerly on every edit (input
//     <exprs-id>); R evaluates each on its own and pushes the values back
//     (`prose-values`), which are painted into the chips. Never touches the
//     DAG.
//   - The TEXT commits on blur or Ctrl/Cmd+Enter to input <input-id>, never
//     on a keystroke.
//
// R -> JS push (`prose-set`) handles external (AI) writes.
//
// A host that shows several texts as one document (blockr.page) treats the
// edges of each as passages to the next: at an edge, the arrow keys,
// Backspace and Delete raise `prose-edge` on the element, and a host that
// handles it cancels the event. A "/" typed on an empty line raises
// `prose-slash` the same way, for the host's insert menu, and an "@" at the
// start of a word `prose-at`, for a value to put in. `el.blockrProse` is the
// controller, for focusAt(), join(), splitHere() and insertChip().

import { Editor, rootCtx, defaultValueCtx, editorViewCtx, serializerCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { history } from "@milkdown/kit/plugin/history";
import { replaceAll, getMarkdown } from "@milkdown/kit/utils";
import { Selection, TextSelection } from "@milkdown/kit/prose/state";
import { joinBackward } from "@milkdown/kit/prose/commands";
import { inlineRNode, inlineRRemark, inlineRPlugin, collectExprs } from "./inline-r.js";
import { xrefNode, xrefRemark } from "./xref.js";
import { columnInsert, linkHref, isPastedLink } from "./util.js";

const instances = new Map(); // el.id -> ProseBlock

// The last message of each kind for an element that has no instance yet. The
// server sends `prose-columns` when it starts, which can be before the
// element is on the page. The constructor applies what waits here.
const pending = new Map(); // el.id -> { type: msg }

// How long a raw markdown edit waits before it goes to the editor and the
// server, so that half-typed code is not evaluated at every keystroke.
const RAW_EDIT_MS = 300;

// An element that left the page keeps its editor this long, in case it was
// only moved (a dock panel that changes place).
const GONE_MS = 1000;

let fieldSeq = 0;

const strip = (md) => String(md || "").replace(/\n+$/, "");

// What a host calls the figures and tables a text refers to: label -> name
// ("fig-mdl" -> "Figure 2"), one map for every text on the page.
let REFS = {};

// A few calls that come up in sentences, offered under the inputs.
const CHANGED_MS = 8000;

const COMMON = ["nrow", "round", "mean", "median", "min", "max", "sum", "format", "scales::percent"];

function setShinyInput(id, value) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue(id, value, { priority: "event" });
  }
}

const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;");

class ProseBlock {
  constructor(el) {
    this.el = el;
    this.inputId = el.dataset.inputId;
    this.exprsId = el.dataset.exprsId;
    this.markdown = el.dataset.initial || "";
    this.committed = this.markdown;
    this.inputs = {}; // input name -> [columns]
    this.offer = {}; // object a host offers -> { name, cols } (blockr.page: the blocks above)
    this.values = {}; // expr -> {ok, value}
    this._sentExprs = "";
    // The editor's own markdown for the last text written into it by code
    // (not typed). The editor reports every change on a debounce, also this
    // one; the report that equals `_applied` is that echo and is skipped.
    this._applied = null;
    this._rawTimer = null;
    // Counts the server's writes. A commit carries it, so the server can
    // drop a commit that was made before its last write arrived.
    this.version = 0;
    this.destroyed = false;
    this.editor = null;
    this.field = null; // the open code field
    el.blockrProse = this;

    // An element that was off the page and came back: the state of the
    // instance that was destroyed then.
    const kept = el._blockrProseKept;
    if (kept) {
      el._blockrProseKept = null;
      this.markdown = this.committed = kept.markdown;
      this.inputs = kept.inputs;
      this.offer = kept.offer;
      this.values = kept.values;
      this.version = kept.version;
    }

    this._buildDom();
    const waiting = pending.get(el.id);
    if (waiting) {
      pending.delete(el.id);
      Object.keys(waiting).forEach((type) => HANDLERS[type](this, waiting[type]));
    }
    this._initEditor().catch((e) => console.error("[prose] init failed", e));
    this._bindCommit();
  }

  // The element left the page for good: the editor, its timers and its
  // floating parts go. What it held stays on the element, so an element that
  // comes back (a dock panel shown again) starts where it was.
  destroy() {
    if (this.destroyed) return;
    this._commit();
    this.destroyed = true;
    clearTimeout(this._rawTimer);
    clearTimeout(this._appliedTimer);
    clearTimeout(this._unpaint);
    this.field = null;
    this._hidePopup();
    if (this.popup) { this.popup.remove(); this.popup = null; }
    this._closeLink();
    this.el._blockrProseKept = {
      markdown: this.markdown,
      inputs: this.inputs,
      offer: this.offer,
      values: this.values,
      version: this.version,
    };
    if (this.editor) {
      try { this.editor.destroy(); } catch (e) { /* already gone */ }
      this.editor = null;
    }
    this.editorHost.remove();
    this.details.remove();
    if (this.el.blockrProse === this) this.el.blockrProse = null;
    if (instances.get(this.el.id) === this) instances.delete(this.el.id);
  }

  // ---- DOM -----------------------------------------------------------------

  _buildDom() {
    this.el.classList.add("blockr-prose");

    this.editorHost = document.createElement("div");
    this.editorHost.className = "blockr-prose-editor";

    // The raw markdown, collapsed under the text.
    this.details = document.createElement("details");
    this.details.className = "blockr-prose-source";
    const summary = document.createElement("summary");
    summary.textContent = "Markdown";
    this.textarea = document.createElement("textarea");
    this.textarea.className = "blockr-prose-source-ta";
    this.textarea.rows = 6;
    this.textarea.spellcheck = false;
    this.textarea.value = this.markdown;
    this.textarea.addEventListener("input", () => this._onRawEdit());
    this.details.appendChild(summary);
    this.details.appendChild(this.textarea);

    this.el.appendChild(this.editorHost);
    this.el.appendChild(this.details);
  }

  async _initEditor() {
    const self = this;
    this.editor = await Editor.make()
      .config((ctx) => {
        ctx.set(rootCtx, this.editorHost);
        ctx.set(defaultValueCtx, this.markdown);
        ctx.get(listenerCtx).markdownUpdated((_ctx, md) => self._onWysiwygUpdate(md));
      })
      .use(commonmark)
      .use(gfm)
      .use(listener)
      .use(history)
      .use(inlineRRemark)
      .use(inlineRNode)
      .use(xrefRemark)
      .use(xrefNode)
      .use(inlineRPlugin((view, pos, isNew) => self._openField(view, pos, isNew)))
      .create();

    if (this.destroyed) {
      try { this.editor.destroy(); } catch (e) { /* already gone */ }
      this.editor = null;
      return;
    }
    // From here on the text is compared in the editor's own markdown. Text
    // that the editor writes differently (`* item`, `__bold__`) would
    // otherwise count as a change on the first blur, although nobody typed.
    if (this.markdown === this.committed) {
      this.markdown = this.committed = this._editorMarkdown();
    }
    this._sendExprs();
    this._paint();
  }

  _editorMarkdown() {
    try { return strip(this.editor.action(getMarkdown())); } catch (e) { return this.markdown; }
  }

  _bindCommit() {
    // Leaving the block commits; the code field and its suggestions are part
    // of it. The listeners sit on the block's own nodes and go with them.
    this.el.addEventListener("focusout", (ev) => {
      if (this.destroyed) return;
      const to = ev.relatedTarget;
      if (to && (this.el.contains(to) || (this.popup && this.popup.contains(to)))) return;
      if (this.field) return;
      this._commit();
    });
    this.el.addEventListener("keydown", (ev) => {
      if (this.destroyed) return;
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        this._commit();
      }
    });
    // before the editor's own keys
    this.editorHost.addEventListener("keydown", (ev) => this._edge(ev), true);
    // a link pasted over words links them
    this.editorHost.addEventListener("paste", (ev) => {
      const view = this._view();
      const url = ((ev.clipboardData && ev.clipboardData.getData("text/plain")) || "").trim();
      if (!view || view.state.selection.empty || !isPastedLink(url)) return;
      ev.preventDefault();
      ev.stopPropagation();
      const { from, to } = view.state.selection;
      const link = view.state.schema.marks.link;
      view.dispatch(view.state.tr.addMark(from, to, link.create({ href: url })));
    }, true);
  }

  // ---- edges: the host's passage to the next text ----------------------------

  _view() {
    if (!this.editor) return null;
    try { return this.editor.action((ctx) => ctx.get(editorViewCtx)); } catch (e) { return null; }
  }

  _edge(ev) {
    if ((ev.metaKey || ev.ctrlKey) && !ev.shiftKey && !ev.altKey && ev.key.toLowerCase() === "k") {
      ev.preventDefault();
      ev.stopPropagation();
      return this._linkField();
    }
    if (ev.key === "/") return this._slash(ev);
    if (ev.key === "@") return this._at(ev);
    if (this.field || ev.defaultPrevented || ev.shiftKey || ev.altKey || ev.metaKey || ev.ctrlKey || ev.isComposing) return;
    const view = this._view();
    if (!view) return;
    const st = view.state, sel = st.selection, doc = st.doc;
    if (!sel.empty) return;
    const atStart = sel.head <= Selection.atStart(doc).head;
    const atEnd = sel.head >= Selection.atEnd(doc).head;
    const para = (n) => n && n.type.name === "paragraph";
    let dir = null;
    if (ev.key === "ArrowUp" && sel.$head.index(0) === 0 && view.endOfTextblock("up")) dir = "up";
    else if (ev.key === "ArrowDown" && sel.$head.index(0) === doc.childCount - 1 && view.endOfTextblock("down")) dir = "down";
    else if (ev.key === "ArrowLeft" && atStart) dir = "left";
    else if (ev.key === "ArrowRight" && atEnd) dir = "right";
    // Backspace at the start of a heading or a list lifts it, as it should
    else if (ev.key === "Backspace" && atStart && para(doc.firstChild)) dir = "back";
    else if (ev.key === "Delete" && atEnd && para(doc.lastChild)) dir = "forward";
    if (!dir) return;
    const caret = view.coordsAtPos(sel.head);
    const e = new CustomEvent("prose-edge", {
      bubbles: true,
      cancelable: true,
      detail: { dir: dir, x: caret.left, empty: this.isEmpty() }
    });
    if (!this.el.dispatchEvent(e)) {
      ev.preventDefault();
      ev.stopPropagation();
    }
  }

  // ---- find and replace, for a host ------------------------------------------
  //
  // find() gives each match of `q` (any case) as editor positions, in order;
  // a match does not run across a value. rangeOf() is a match as a DOM range,
  // to highlight; replaceRange() puts text in its place and commits.

  find(q) {
    const view = this._view();
    if (!view || !q) return [];
    const needle = q.toLowerCase(), out = [];
    view.state.doc.descendants((node, pos) => {
      if (!node.isTextblock) return true;
      let text = "";
      const map = [];
      node.forEach((child, off) => {
        if (child.isText) {
          for (let i = 0; i < child.text.length; i++) map.push(pos + 1 + off + i);
          text += child.text;
        } else {
          map.push(-1);
          text += "\ufffc";
        }
      });
      const hay = text.toLowerCase();
      for (let i = hay.indexOf(needle); i >= 0; i = hay.indexOf(needle, i + needle.length)) {
        const a = map[i], b = map[i + needle.length - 1];
        if (a >= 0 && b >= 0 && b - a === needle.length - 1) out.push({ from: a, to: b + 1 });
      }
      return false;
    });
    return out;
  }

  rangeOf(m) {
    const view = this._view();
    if (!view) return null;
    try {
      const s = view.domAtPos(m.from), e = view.domAtPos(m.to);
      const r = document.createRange();
      r.setStart(s.node, s.offset);
      r.setEnd(e.node, e.offset);
      return r;
    } catch (err) { return null; }
  }

  replaceRange(m, text) {
    const view = this._view();
    if (!view) return;
    view.dispatch(view.state.tr.insertText(text, m.from, m.to));
    this._commit();
  }

  // ---- links: Cmd/Ctrl+K -------------------------------------------------------
  //
  // On selected words, a small field for the address; in a link, the same
  // field with its address, and an empty address takes the link off.

  _linkRange(st) {
    const link = st.schema.marks.link;
    let { from, to } = st.selection;
    if (from !== to) return { from, to, href: (link.isInSet(st.doc.resolve(from + 1).marks()) || {}).attrs?.href || "" };
    const $p = st.doc.resolve(from), start = $p.start();
    let range = null;
    $p.parent.forEach((node, offset) => {
      const a = start + offset, b = a + node.nodeSize, m = link.isInSet(node.marks);
      if (m && a <= from && from <= b) {
        if (!range) range = { from: a, to: b, href: m.attrs.href };
        else if (a === range.to) range.to = b;
      }
    });
    return range;
  }

  _linkField() {
    const view = this._view();
    if (!view) return;
    const range = this._linkRange(view.state);
    if (!range) return;
    this._closeLink();
    const box = document.createElement("div");
    box.className = "blockr-prose-link";
    box.innerHTML = '<span class="blockr-input"><input type="url" class="blockr-input__field" placeholder="Paste a link" aria-label="Link"></span>';
    const input = box.querySelector("input");
    input.value = range.href || "";
    document.body.appendChild(box);
    const r = view.coordsAtPos(range.from);
    box.style.left = Math.max(8, r.left) + "px";
    box.style.top = (r.bottom + 6) + "px";
    this._link = box;
    const done = (apply) => {
      if (this._link !== box) return;
      this._closeLink();
      if (apply) {
        // an empty address takes the link off; so does a scheme that is not
        // a web or mail address
        const href = linkHref(input.value);
        const link = view.state.schema.marks.link;
        let tr = view.state.tr.removeMark(range.from, range.to, link);
        if (href) tr = tr.addMark(range.from, range.to, link.create({ href: href }));
        view.dispatch(tr);
      }
      view.focus();
    };
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      if (e.key === "Enter") { e.preventDefault(); done(true); }
      if (e.key === "Escape") { e.preventDefault(); done(false); }
    });
    input.addEventListener("blur", () => setTimeout(() => done(false), 0));
    input.focus();
    input.select();
  }

  _closeLink() {
    if (this._link) { const b = this._link; this._link = null; b.remove(); }
  }

  // "/" on an empty line of its own (not in a list or a quote), for what
  // goes between the lines; or where a word starts in running text
  // (`inline`), for what goes in the sentence. "and/or" stays text.
  _slash(ev) {
    if (this.field || ev.defaultPrevented || ev.altKey || ev.metaKey || ev.ctrlKey || ev.isComposing) return;
    const view = this._view();
    if (!view) return;
    const sel = view.state.selection, $h = sel.$head;
    if (!sel.empty || !$h.parent.isTextblock || $h.parent.type.spec.code) return;
    const line = $h.depth === 1 && $h.parent.type.name === "paragraph" && !$h.parent.content.size;
    if (!line) {
      const prev = $h.parent.textBetween(Math.max(0, $h.parentOffset - 1), $h.parentOffset, null, "\ufffc");
      if (prev && !/\s|[(\[]/.test(prev)) return;
    }
    const r = view.coordsAtPos(sel.head);
    const e = new CustomEvent("prose-slash", {
      bubbles: true,
      cancelable: true,
      detail: { left: r.left, top: r.top, bottom: r.bottom, inline: !line }
    });
    if (!this.el.dispatchEvent(e)) {
      ev.preventDefault();
      ev.stopPropagation();
    }
  }

  // "@" where a word starts
  _at(ev) {
    if (this.field || ev.defaultPrevented || ev.altKey || ev.metaKey || ev.ctrlKey || ev.isComposing) return;
    const view = this._view();
    if (!view) return;
    const sel = view.state.selection, $h = sel.$head;
    if (!sel.empty || !$h.parent.isTextblock || $h.parent.type.spec.code) return;
    const prev = $h.parent.textBetween(Math.max(0, $h.parentOffset - 1), $h.parentOffset, null, "\ufffc");
    if (prev && !/\s|[(\[]/.test(prev)) return;
    const r = view.coordsAtPos(sel.head);
    const e = new CustomEvent("prose-at", {
      bubbles: true,
      cancelable: true,
      detail: { left: r.left, top: r.top, bottom: r.bottom }
    });
    if (!this.el.dispatchEvent(e)) {
      ev.preventDefault();
      ev.stopPropagation();
    }
  }

  // A value at the cursor: a chip with this code. `open` opens its code
  // field, for code of your own.
  insertChip(expr, open) {
    const view = this._view();
    if (!view) return;
    const type = view.state.schema.nodes.inline_r;
    const pos = view.state.selection.from;
    view.dispatch(view.state.tr.replaceSelectionWith(type.create({ expr: expr })));
    view.focus();
    if (open) {
      this._openField(view, pos, false);
    } else {
      this.text();
    }
  }

  // The text around the cursor's line, as markdown: what comes before the
  // line and what comes after it. The line itself is in neither.
  splitHere() {
    const view = this._view();
    if (!view) return null;
    const st = view.state, $h = st.selection.$head, doc = st.doc;
    if ($h.depth < 1) return null;
    const ser = this.editor.action((ctx) => ctx.get(serializerCtx));
    const md = (frag) => frag.size ? ser(doc.type.create(doc.attrs, frag)).replace(/\n+$/, "") : "";
    return { before: md(doc.content.cut(0, $h.before(1))), after: md(doc.content.cut($h.after(1))) };
  }

  // The cursor's line out of the text, unless it is the only one.
  dropLine() {
    const view = this._view();
    if (!view) return;
    const $h = view.state.selection.$head;
    if ($h.depth < 1 || view.state.doc.childCount < 2) return;
    view.dispatch(view.state.tr.delete($h.before(1), $h.after(1)));
  }

  // Text typed in for the host: at the cursor, as if typed.
  typeHere(text) {
    const view = this._view();
    if (!view) return;
    view.dispatch(view.state.tr.insertText(text));
    view.focus();
  }

  // The cursor into the text: "start", "end", or on its first or last line
  // ("first", "last") as close to `x` as the line allows; "here" keeps it
  // where it was.
  focusAt(where, x) {
    const view = this._view();
    if (!view) return false;
    if (where === "here") { view.focus(); return true; }
    const doc = view.state.doc;
    let sel = where === "start" || where === "first" ? Selection.atStart(doc) : Selection.atEnd(doc);
    if ((where === "first" || where === "last") && x != null) {
      const r = view.dom.getBoundingClientRect();
      const y = where === "first" ? r.top + 6 : r.bottom - 6;
      const hit = view.posAtCoords({ left: x, top: y });
      if (hit) sel = TextSelection.near(doc.resolve(hit.pos));
    }
    view.dispatch(view.state.tr.setSelection(sel).scrollIntoView());
    view.focus();
    return true;
  }

  // Another text joined onto the end of this one, as Backspace joins two
  // paragraphs: its first paragraph runs on from this one's last, the cursor
  // at the seam. Commits.
  join(md) {
    const view = this._view();
    if (!view) return;
    md = (md || "").replace(/\n+$/, "");
    if (!md.trim()) { this.focusAt("end"); return; }
    const seam = Selection.atEnd(view.state.doc).head;
    const mine = this.text();
    const both = mine.trim() ? mine + "\n\n" + md : md;
    this.markdown = both;
    this.textarea.value = both;
    this._applyToWysiwyg(both);
    const v = this._view();
    if (this.markdown !== md) {
      // the cursor at the start of the joined text, then join backward
      const $p = v.state.doc.resolve(Math.min(seam + 2, v.state.doc.content.size));
      v.dispatch(v.state.tr.setSelection(TextSelection.near($p)));
      joinBackward(v.state, v.dispatch);
    }
    v.focus();
    this._afterChange();
    this._commit();
  }

  // ---- sync ------------------------------------------------------------------

  _onWysiwygUpdate(md) {
    if (this.destroyed) return;
    md = strip(md);
    if (this._applied !== null && md === this._applied) {
      // the echo of a text written by code, not an edit
      this._applied = null;
      return;
    }
    this._applied = null;
    this.markdown = md;
    this._syncTextarea();
    this._afterChange();
  }

  // The raw markdown follows the editor, but never while it is being typed
  // in: that would rewrite half-typed syntax and move the cursor.
  _syncTextarea() {
    if (document.activeElement === this.textarea) return;
    if (this.textarea.value !== this.markdown) this.textarea.value = this.markdown;
  }

  // Typing in the raw markdown reaches the editor and the server after a
  // pause, not at every keystroke.
  _onRawEdit() {
    clearTimeout(this._rawTimer);
    this._rawTimer = setTimeout(() => this._applyRaw(), RAW_EDIT_MS);
  }

  _applyRaw() {
    clearTimeout(this._rawTimer);
    this._rawTimer = null;
    this.markdown = this.textarea.value;
    this._applyToWysiwyg(this.markdown);
    this._afterChange();
  }

  // Text written into the editor by code. `_applied` remembers how the
  // editor writes it, for the report that follows. A text that changes
  // nothing gives no report, so the mark is dropped after a while.
  _applyToWysiwyg(md) {
    if (!this.editor) return;
    this.editor.action(replaceAll(md));
    this._applied = this._editorMarkdown();
    clearTimeout(this._appliedTimer);
    this._appliedTimer = setTimeout(() => { this._applied = null; }, 1000);
  }

  _afterChange() {
    this._sendExprs();
    this._paint();
  }

  // The editor reports its markdown on a debounce; this reads it now, so a
  // commit or a host never sees text that is a few keystrokes old.
  text() {
    if (this._rawTimer) this._applyRaw();
    if (this.editor) {
      const md = this._editorMarkdown();
      if (md !== this.markdown) {
        this.markdown = md;
        this._syncTextarea();
        this._afterChange();
      }
    }
    return this.markdown;
  }

  isEmpty() {
    const view = this._view();
    if (!view) return !this.markdown.trim();
    const doc = view.state.doc;
    return doc.childCount === 1 && doc.firstChild.isTextblock && doc.firstChild.content.size === 0;
  }

  _commit() {
    if (this.destroyed) return;
    this.text();
    if (this.markdown === this.committed) return;
    this.committed = this.markdown;
    // The version travels in the same message as the text.
    setShinyInput(this.inputId + "_version", this.version);
    setShinyInput(this.inputId, this.markdown);
  }

  // External / AI write: update both views, do NOT commit back to R.
  // `version` is the server's count of such writes.
  setMarkdown(md, version) {
    if (version != null) this.version = version;
    // Nothing to do when the editor already holds this text: the same
    // string, or the string of the last write, which the editor may have
    // rewritten in its own markdown.
    const same = md === this.markdown ||
      (md === this._lastSet && this.markdown === this._lastSetMd);
    if (same && this.markdown === this.committed) return;
    clearTimeout(this._rawTimer);
    this._rawTimer = null;
    this.markdown = md;
    this.committed = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._applyToWysiwyg(md);
    // compared in the editor's own markdown from here on, see _initEditor()
    if (this.editor) this.markdown = this.committed = this._editorMarkdown();
    this._lastSet = md;
    this._lastSetMd = this.markdown;
    this._sendExprs();
    Promise.resolve().then(() => this._paint());
  }

  // ---- values ----------------------------------------------------------------

  _sendExprs() {
    if (!this.exprsId) return;
    const exprs = collectExprs(this.markdown);
    const key = JSON.stringify(exprs);
    if (key === this._sentExprs) return;
    this._sentExprs = key;
    setShinyInput(this.exprsId, exprs);
  }

  // A value that changed since it was last shown is tinted for a while, so
  // a block that moves the numbers in a sentence does not go unnoticed.
  setValues(values) {
    const now = Date.now();
    this._changed = this._changed || {};
    Object.entries(values || {}).forEach(([expr, rec]) => {
      const old = this.values[expr];
      if (old && old.ok === true && rec && rec.ok === true && old.value !== rec.value) {
        this._changed[expr] = now + CHANGED_MS;
      }
    });
    this.values = values || {};
    this._paint();
    clearTimeout(this._unpaint);
    this._unpaint = setTimeout(() => this._paint(), CHANGED_MS + 50);
  }

  // Every chip from the value store: its value when the server sent one, its
  // code while it has not (no data yet), its code in red when it failed.
  _paint() {
    this._paintRefs();
    this.editorHost.querySelectorAll(".blockr-r-chip").forEach((chip) => {
      if (chip.classList.contains("is-editing")) return;
      const expr = chip.getAttribute("data-r") || "";
      const rec = this.values[expr];
      const ok = rec && rec.ok === true;
      chip.classList.toggle("is-err", !!rec && rec.ok === false);
      chip.classList.toggle("is-dormant", !ok && !(rec && rec.ok === false));
      chip.classList.toggle("is-changed", ok && (this._changed || {})[expr] > Date.now());
      const text = ok ? rec.value : expr;
      if (chip.textContent !== text) chip.textContent = text;
      // pointing at a value shows its code, in the design system's tooltip
      chip.setAttribute("data-blockr-tooltip",
        "r " + expr + (rec && rec.ok === false ? "  \u00b7  " + rec.value : ""));
    });
  }

  // The names of the figures and tables, for every text: what a reference
  // shows. One it does not know shows its label.
  // Kept on the instance for hosts that call it there; the names are one map
  // for the page.
  setRefs(refs) {
    setRefs(refs);
  }

  _paintRefs() {
    this.editorHost.querySelectorAll(".blockr-xref").forEach((x) => {
      const key = x.getAttribute("data-xref") || "";
      const text = REFS[key] || "@" + key;
      if (x.textContent !== text) x.textContent = text;
      x.classList.toggle("is-unknown", !REFS[key]);
    });
  }

  // A reference at the cursor.
  insertRef(key) {
    const view = this._view();
    if (!view) return;
    const type = view.state.schema.nodes.xref;
    view.dispatch(view.state.tr.replaceSelectionWith(type.create({ key: key })));
    view.focus();
    this.text();
  }

  setInputs(inputs) {
    this.inputs = inputs || {};
  }

  setOffer(offer) {
    this.offer = offer || {};
  }

  // What the code can name: the inputs, and what a host offers besides.
  _objects() {
    const out = {};
    Object.entries(this.offer).forEach(([k, o]) => { out[k] = { cols: (o && o.cols) || [], meta: (o && o.name) || "block" }; });
    Object.entries(this.inputs).forEach(([k, cols]) => { if (k) out[k] = { cols: cols || [], meta: (out[k] && out[k].meta) || "input" }; });
    return out;
  }

  // ---- the code field --------------------------------------------------------
  //
  // A chip opens in place as a small code field, a Blockr.Input field: the
  // line reflows around it, and its completions drop down in the field
  // dropdown of the design system (portalled with Blockr.place, a layer with
  // Blockr.layer). Enter or a closing backtick computes it, Escape leaves it
  // as it was (and drops a new one), Tab or Enter takes a completion.

  _openField(view, pos, isNew) {
    this._closeField(true);
    const node = view.state.doc.nodeAt(pos);
    const chip = view.nodeDOM(pos);
    if (!node || !chip) return;
    const expr = node.attrs.expr || "";
    chip.removeAttribute("data-blockr-tooltip");

    chip.classList.add("is-editing");
    chip.textContent = "";
    const box = document.createElement("span");
    box.className = "blockr-input blockr-r-field";
    const input = document.createElement("input");
    input.type = "text";
    input.className = "blockr-input__field";
    input.setAttribute("autocomplete", "off");
    input.setAttribute("aria-label", "R expression");
    input.setAttribute("role", "combobox");
    input.setAttribute("aria-autocomplete", "list");
    input.setAttribute("aria-expanded", "false");
    input.spellcheck = false;
    input.value = expr;
    box.appendChild(input);
    chip.appendChild(box);
    // ch of the code face, plus the field's padding and border
    const grow = () => { input.style.width = "calc(" + Math.max(3, input.value.length + 1) + "ch + 14px)"; };
    grow();
    input.focus();
    input.setSelectionRange(input.value.length, input.value.length);

    this.field = { view, pos, chip, input, expr, isNew, items: [], cur: -1 };

    input.addEventListener("input", () => { grow(); this._suggest(); });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      const f = this.field;
      if (!f) return;
      const open = this._popupOpen();
      if (e.key === "ArrowDown" && open) { e.preventDefault(); f.cur = Math.min(f.items.length - 1, f.cur + 1); this._mark(); }
      else if (e.key === "ArrowUp" && open) { e.preventDefault(); f.cur = Math.max(0, f.cur - 1); this._mark(); }
      else if ((e.key === "Tab" || e.key === "Enter") && open && f.cur >= 0 &&
               f.items[f.cur].insert !== f.items[f.cur].token) { e.preventDefault(); this._take(f.items[f.cur]); }
      else if (e.key === "Enter") { e.preventDefault(); this._closeField(true); }
      else if (e.key === "`") { e.preventDefault(); this._closeField(true); }
      else if (e.key === "Escape") {
        // the layer closes an open list first; Escape on a closed list leaves
        e.preventDefault();
        if (!f.listJustClosed) this._closeField(false);
      }
    });
    input.addEventListener("mousedown", (e) => e.stopPropagation());
    input.addEventListener("blur", () => setTimeout(() => {
      if (this.field && this.field.input === input && document.activeElement !== input) this._closeField(true);
    }, 0));
    this._suggest();
  }

  _closeField(keep) {
    const f = this.field;
    if (!f) return;
    this.field = null;
    this._hidePopup();
    const next = f.input.value.trim();
    f.chip.classList.remove("is-editing");
    const view = f.view;
    const node = view.state.doc.nodeAt(f.pos);
    const still = node && node.type.name === "inline_r";
    if (still && (keep ? !next : f.isNew)) {
      // an empty field, or a new one left with Escape, goes
      view.dispatch(view.state.tr.delete(f.pos, f.pos + node.nodeSize));
    } else if (still && keep && next !== f.expr) {
      // a new expression rebuilds the chip, which sends it for a value
      view.dispatch(view.state.tr.setNodeMarkup(f.pos, undefined, { expr: next }));
    } else {
      f.chip.textContent = f.expr;
      this._paint();
    }
    view.focus();
  }

  // ---- completions: the inputs and a few common calls, as Blockr.Input
  // lists columns and functions; after `name$`, the columns of that input.

  _popupOpen() {
    return !!(this.popup && this.popup.style.display === "block" && this.field && this.field.items.length);
  }

  _hidePopup() {
    if (this.placement) { this.placement.stop(); this.placement = null; }
    if (this.layer) { this.layer.remove(); this.layer = null; }
    if (this.popup) { this.popup.style.display = ""; this.popup.innerHTML = ""; }
    if (this.field) {
      this.field.input.setAttribute("aria-expanded", "false");
      this.field.input.removeAttribute("aria-activedescendant");
    }
  }

  _suggest() {
    const f = this.field;
    if (!f) return;
    const v = f.input.value.slice(0, f.input.selectionStart == null ? f.input.value.length : f.input.selectionStart);
    const word = /[A-Za-z0-9_.:]*$/.exec(v)[0];
    const items = [];
    const dollar = /([A-Za-z.][A-Za-z0-9_.]*)\$([A-Za-z0-9_.]*)$/.exec(v);
    const objs = this._objects();
    if (dollar && objs[dollar[1]]) {
      const part = dollar[2].toLowerCase();
      (objs[dollar[1]].cols || [])
        .filter((c) => String(c).toLowerCase().startsWith(part))
        .forEach((c) => {
          const ins = columnInsert(c, dollar[2]);
          if (ins) items.push({ text: String(c), insert: ins.insert, token: ins.token, meta: "column" });
        });
    } else if (word) {
      Object.keys(objs)
        .filter((n) => n.toLowerCase().startsWith(word.toLowerCase()))
        .forEach((n) => items.push({ text: n, insert: n, token: word, meta: objs[n].meta }));
      COMMON
        .filter((c) => c.toLowerCase().startsWith(word.toLowerCase()))
        .forEach((c) => items.push({ text: c, insert: c, token: word, meta: "often used", fn: true }));
    } else if (!f.input.value.trim()) {
      // an empty field offers the inputs
      Object.keys(objs)
        .forEach((n) => items.push({ text: n, insert: n, token: "", meta: objs[n].meta }));
    }
    f.items = items;
    f.cur = items.length ? 0 : -1;
    if (!items.length) { this._hidePopup(); return; }

    if (!this.popup) {
      this.popup = document.createElement("div");
      this.popup.className = "blockr-input__popup blockr-r-popup";
      this.popup.id = "blockr-r-popup-" + (++fieldSeq);
      this.popup.setAttribute("role", "listbox");
      this.popup.addEventListener("mousedown", (e) => {
        e.preventDefault();
        const row = e.target.closest("[data-i]");
        if (row && this.field) this._take(this.field.items[+row.dataset.i]);
      });
    }
    this.popup.innerHTML = items.map((it, i) =>
      `<div class="blockr-input__item" role="option" id="${this.popup.id}-${i}" data-i="${i}">` +
      `<span class="blockr-input__item-text">${esc(it.text)}</span>` +
      (it.fn ? '<span class="blockr-input__item-parens">()</span>' : "") +
      `<span class="blockr-input__item-meta">${esc(it.meta)}</span></div>`).join("");
    if (this.popup.style.display !== "block") {
      document.body.appendChild(this.popup);
      this.popup.style.display = "block";
      if (window.Blockr && Blockr.place) {
        this.placement = Blockr.place(this.popup, f.chip, { gap: 2, width: { min: 220, max: 320 } });
      }
      if (window.Blockr && Blockr.layer) {
        this.layer = Blockr.layer(this.popup, {
          from: f.chip,
          escape: () => {
            const g = this.field;
            this._hidePopup();
            if (g) { g.listJustClosed = true; setTimeout(() => { g.listJustClosed = false; }, 0); }
          },
          outside: () => this._hidePopup(),
        });
      }
    }
    f.input.setAttribute("aria-controls", this.popup.id);
    f.input.setAttribute("aria-expanded", "true");
    this._mark();
  }

  _mark() {
    if (!this.popup || !this.field) return;
    const cur = this.field.cur;
    this.popup.querySelectorAll("[data-i]").forEach((row) => {
      const on = +row.dataset.i === cur;
      row.classList.toggle("blockr-input__item--highlighted", on);
      row.setAttribute("aria-selected", on ? "true" : "false");
      if (on && row.scrollIntoView) row.scrollIntoView({ block: "nearest" });
    });
    if (cur >= 0) this.field.input.setAttribute("aria-activedescendant", this.popup.id + "-" + cur);
    else this.field.input.removeAttribute("aria-activedescendant");
  }

  _take(it) {
    const f = this.field;
    if (!f || !it) return;
    const inp = f.input;
    const at = inp.selectionStart == null ? inp.value.length : inp.selectionStart;
    const start = at - it.token.length;
    const ins = it.fn ? it.insert + "()" : it.insert;
    inp.value = inp.value.slice(0, start) + ins + inp.value.slice(at);
    // a function puts the cursor between its parentheses
    const caret = start + (it.fn ? ins.length - 1 : ins.length);
    inp.setSelectionRange(caret, caret);
    inp.dispatchEvent(new Event("input"));
    inp.focus();
    // a finished name needs no list
    if (!it.fn) this._hidePopup();
  }
}

// ---- init & message handlers --------------------------------------------------

// The names of the figures and tables, for every text on the page.
function setRefs(refs) {
  REFS = refs || {};
  instances.forEach((inst) => inst._paintRefs());
}

// What each message from the server does to an instance.
const HANDLERS = {
  "prose-set": (inst, msg) => inst.setMarkdown(msg.markdown || "", msg.version),
  "prose-columns": (inst, msg) => inst.setInputs(msg.inputs || {}),
  // A host (blockr.page) offers objects the text may name besides the
  // inputs: naming one is how the text comes to read from it.
  "prose-offer": (inst, msg) => inst.setOffer(msg.objects || {}),
  "prose-values": (inst, msg) => inst.setValues(msg.values || {}),
};

// A message goes to its instance, or waits for it.
function deliver(type, msg) {
  if (!msg || !msg.id) return;
  const inst = instances.get(msg.id);
  if (inst && !inst.destroyed) {
    HANDLERS[type](inst, msg);
    return;
  }
  const waiting = pending.get(msg.id) || {};
  waiting[type] = msg;
  pending.set(msg.id, waiting);
}

function initEl(el) {
  if (!el || !el.id) return;
  // a block removed and put back has the same id: the old one is gone
  const old = instances.get(el.id);
  if (old && !old.destroyed && (old.el === el || old.el.isConnected)) return;
  if (!el.classList.contains("blockr-prose") && !el.dataset.inputId) return;
  if (old) old.destroy();
  instances.set(el.id, new ProseBlock(el));
}

function scan(root) {
  (root || document).querySelectorAll(".blockr-prose[data-input-id]").forEach(initEl);
}

// Instances whose element is no longer on the page are destroyed, after a
// pause: a panel that is moved leaves the page and comes straight back.
let sweepTimer = null;
function sweep() {
  sweepTimer = null;
  instances.forEach((inst) => { if (!inst.el.isConnected) inst.destroy(); });
}

function register() {
  scan(document);

  const mo = new MutationObserver((muts) => {
    let removed = false;
    for (const m of muts) {
      if (m.removedNodes.length) removed = true;
      m.addedNodes.forEach((n) => {
        if (n.nodeType !== 1) return;
        if (n.matches && n.matches(".blockr-prose[data-input-id]")) initEl(n);
        if (n.querySelectorAll) scan(n);
      });
    }
    if (removed && instances.size && !sweepTimer) sweepTimer = setTimeout(sweep, GONE_MS);
  });
  mo.observe(document.body, { childList: true, subtree: true });

  // A session (re)connect re-announces every instance's expressions: the
  // first send can predate Shiny.setInputValue, and a reconnect wipes the
  // server's inputs. shiny:connected is a jQuery event.
  if (window.jQuery) {
    window.jQuery(document).on("shiny:connected", () => {
      instances.forEach((inst) => {
        inst._sentExprs = "";
        inst._sendExprs();
      });
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Object.keys(HANDLERS).forEach((type) => {
      Shiny.addCustomMessageHandler(type, (msg) => deliver(type, msg));
    });
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
