/**
 * Blockr.Script — a CodeMirror 6 code editor as a Shiny input binding.
 *
 * Follows the blockr JS-driven-block pattern (see blockr.docs
 * patterns/js-driven-blocks.md, blockr.dplyr/inst/js/filter-block.js).
 *
 * Execution model (see blockr.design/open/function-block-editor):
 *   - The editor reports its document as input$<id> (debounced) — this is the
 *     "editor text", compared server-side against the executed text to decide
 *     whether the dirty-state "Run" button shows.
 *   - Manual typing is PENDING: it only reports the value (→ Run appears).
 *   - Discrete actions on complete code run IMMEDIATELY (no Run):
 *       * AI / external write of `fn`  (R pushes `blockr-script-set` w/ original)
 *       * Reject a hunk (userEvent "revert") → run the reduced doc now
 *       * Ctrl/Cmd-Enter → run the current doc now
 *     Accept a hunk (userEvent "accept") changes nothing to run — the AI code
 *     is already live — so it's a no-op for execution.
 *   - DIFF: a `blockr-script-set` carrying an `original` enters a unifiedMergeView
 *     (inline diff, per-hunk Accept/Reject). Accept-all just closes it; reject-
 *     all restores the merge view's own original and runs it.
 *
 * Bundled to inst/js/blockr-script.js via esbuild (see package.json).
 */
import {
  Compartment, EditorState, StateEffect, StateField, RangeSetBuilder
} from "@codemirror/state";
import {
  EditorView, keymap, lineNumbers, drawSelection, highlightActiveLine,
  Decoration, gutter, GutterMarker
} from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentMore, indentLess } from "@codemirror/commands";
import { StreamLanguage, syntaxHighlighting, defaultHighlightStyle, indentUnit, indentService, getIndentUnit } from "@codemirror/language";
import { r } from "@codemirror/legacy-modes/mode/r";
import { autocompletion, completionKeymap } from "@codemirror/autocomplete";
import { unifiedMergeView, getChunks, getOriginalDoc } from "@codemirror/merge";
import { search, searchKeymap } from "@codemirror/search";

(() => {
  "use strict";

  const DEBOUNCE_MS = 250;

  // An editor that left the page keeps its view this long, in case it was
  // only moved.
  const GONE_MS = 1000;

  // Tab indents, so a keyboard user needs a way out of the editor. Escape
  // followed by Tab (or Shift-Tab) moves the focus on, as CodeMirror
  // recommends. The first Escape is used up here, so the gear tray around the
  // editor stays open; a second Escape reaches the tray and closes it.
  const LEAVE_HINT = "R script. Press Escape, then Tab, to leave the editor.";
  const tabKeys = () => {
    let leave = false;
    const tab = (indent) => (view) => {
      if (leave) { leave = false; return false; }
      return indent(view);
    };
    return [
      keymap.of([
        { key: "Escape", run: () => { if (leave) return false; leave = true; return true; } },
        { key: "Tab", run: tab(indentMore), shift: tab(indentLess) }
      ]),
      EditorView.updateListener.of((u) => {
        if (u.docChanged || (u.focusChanged && !u.view.hasFocus)) leave = false;
      }),
      EditorView.contentAttributes.of({ "aria-label": LEAVE_HINT })
    ];
  };

  // Plain block indentation. Each unclosed `(`, `[` or `{` adds exactly
  // one indent unit (2 spaces) regardless of where the bracket sits on
  // its line. This replaces the legacy R mode's "align continuation
  // lines to the column just after the bracket" behaviour, which pushed
  // the body of `function(...) { expr` far to the right. CodeMirror tries
  // every indentService before its syntax-tree fallback, so returning a
  // number here fully overrides the legacy mode's indent.
  const blockIndent = indentService.of((context, pos) => {
    const unit = getIndentUnit(context.state);
    // `lineAt(pos, -1)` returns only `{text, from}` (no `.to`) when a line
    // break is simulated — i.e. the Enter case — so derive the end from
    // the text length. Using `.to` here counted brackets past the cursor
    // (e.g. a closing `}` below), cancelling the open brace to depth 0.
    const prev = context.lineAt(pos, -1);
    const upto = context.state.sliceDoc(0, prev.from + prev.text.length);
    let depth = 0, inStr = null;
    for (let i = 0; i < upto.length; i++) {
      const c = upto[i];
      if (inStr) {
        if (c === "\\") { i++; continue; }
        if (c === inStr) inStr = null;
        continue;
      }
      if (c === '"' || c === "'" || c === "`") inStr = c;
      else if (c === "#") {
        const nl = upto.indexOf("\n", i);
        if (nl === -1) break;
        i = nl;
      } else if (c === "{" || c === "(" || c === "[") depth++;
      else if (c === "}" || c === ")" || c === "]") depth = Math.max(0, depth - 1);
    }
    // A line that itself starts with a closer dedents one level.
    const after = (context.textAfterPos(pos) || "").replace(/^\s*/, "");
    if (/^[}\])]/.test(after)) depth = Math.max(0, depth - 1);
    return depth * unit;
  });

  // Static completion vocabulary — kept small on purpose (the function body is
  // short and usually AI-written). Column names from the upstream data are
  // added dynamically and boosted above these.
  const R_KEYWORDS = [
    "function", "if", "else", "for", "while", "repeat", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN"
  ];
  const R_BASE = [
    "c", "mean", "sum", "median", "min", "max", "range", "round", "abs", "sqrt",
    "length", "nrow", "ncol", "names", "colnames", "rownames", "seq", "seq_len",
    "seq_along", "rep", "paste", "paste0", "sprintf", "format", "factor",
    "as.numeric", "as.character", "as.factor", "as.integer", "as.logical",
    "is.na", "is.null", "ifelse", "sapply", "lapply", "vapply", "mapply",
    "do.call", "Reduce", "Filter", "Map", "unique", "duplicated", "sort",
    "order", "rev", "which", "table", "tapply", "aggregate", "data.frame",
    "list", "matrix", "rbind", "cbind", "apply", "head", "tail", "subset",
    "transform", "merge", "cut", "quantile", "cumsum"
  ];
  const R_TIDY = [
    "filter", "mutate", "select", "summarise", "summarize", "group_by",
    "ungroup", "arrange", "desc", "rename", "relocate", "distinct", "slice",
    "slice_head", "slice_tail", "slice_max", "slice_min", "pull", "count",
    "tally", "across", "where", "everything", "starts_with", "ends_with",
    "contains", "matches", "num_range", "any_of", "all_of", "left_join",
    "inner_join", "right_join", "full_join", "semi_join", "anti_join",
    "bind_rows", "bind_cols", "case_when", "if_else", "coalesce", "n",
    "n_distinct", "row_number", "lag", "lead", "first", "last", "pivot_longer",
    "pivot_wider", "separate", "unite", "nest", "unnest", "fill", "replace_na",
    "ggplot", "aes", "geom_point", "geom_line", "geom_col", "geom_bar",
    "geom_boxplot", "geom_histogram", "facet_wrap", "facet_grid", "labs",
    "scale_x_continuous", "scale_y_continuous", "theme_minimal", "theme_bw"
  ];

  // R-aware completion source. Closes over the host element so it can read the
  // live column list (pushed from R into el._cmCompletions).
  const makeRSource = (el) => (context) => {
    const word = context.matchBefore(/[A-Za-z0-9._]+/);
    if (!word || (word.from === word.to && !context.explicit)) return null;
    const options = [];
    const cols = el._cmCompletions || [];
    // An entry is a name, or {label, detail} saying what it is: an input
    // and its size, or a column with its type, input and label.
    for (const col of cols) {
      if (col && typeof col === "object") {
        options.push({
          label: String(col.label), type: "variable",
          detail: col.detail || "", boost: 3
        });
      } else {
        options.push({ label: col, type: "variable", detail: "column", boost: 3 });
      }
    }
    for (const fn of R_TIDY) {
      options.push({ label: fn, type: "function", apply: fn + "(", boost: 1 });
    }
    for (const fn of R_BASE) {
      options.push({ label: fn, type: "function", apply: fn + "(", boost: 0 });
    }
    for (const kw of R_KEYWORDS) {
      options.push({ label: kw, type: "keyword", boost: -1 });
    }
    return { from: word.from, options };
  };

  // ---- input marks -------------------------------------------------------
  // R classifies each top-level line of the script and pushes the ones that
  // became controls (`blockr-script-inputs`). The editor paints them: a tinted
  // band on the line plus a gutter glyph naming the widget. This is what tells
  // the user which lines are special -- there is no fence comment in the
  // script, deliberately. Because the marks are pushed on every debounced
  // keystroke, a glyph appears the moment a line becomes a declaration and
  // disappears the moment it stops being one, which teaches the rule faster
  // than any documentation.
  const setInputMarks = StateEffect.define();

  const inputMarks = StateField.define({
    create: () => [],
    update(value, tr) {
      for (const e of tr.effects) {
        if (e.is(setInputMarks)) return e.value;
      }
      return value;
    }
  });

  const inputLineDeco = Decoration.line({ class: "cm-input-line" });

  const inputDecorations = EditorView.decorations.compute(
    [inputMarks, "doc"],
    (state) => {
      const marks = state.field(inputMarks, false) || [];
      const lines = [];
      for (const m of marks) {
        const n = Number(m && m.line);
        if (n >= 1 && n <= state.doc.lines && lines.indexOf(n) === -1) {
          lines.push(n);
        }
      }
      // RangeSetBuilder requires ascending positions.
      lines.sort((a, b) => a - b);
      const builder = new RangeSetBuilder();
      for (const n of lines) {
        builder.add(state.doc.line(n).from, state.doc.line(n).from, inputLineDeco);
      }
      return builder.finish();
    }
  );

  class InputGlyph extends GutterMarker {
    constructor(text, title, kind) {
      super(); this.text = text; this.title = title; this.kind = kind;
    }
    eq(other) {
      return other.text === this.text && other.title === this.title &&
        other.kind === this.kind;
    }
    toDOM() {
      const span = document.createElement("span");
      span.className = "cm-input-glyph";
      // The kind drives the CSS: the select caret is drawn with borders
      // rather than typed, because the triangle codepoints are missing from
      // enough font stacks to render as nothing at gutter size.
      if (this.kind) span.dataset.kind = this.kind;
      span.textContent = this.text || "";
      if (this.title) span.title = this.title;
      return span;
    }
  }

  const inputGutter = gutter({
    class: "cm-input-gutter",
    lineMarker(view, line) {
      const marks = view.state.field(inputMarks, false) || [];
      const n = view.state.doc.lineAt(line.from).number;
      for (const m of marks) {
        if (Number(m.line) === n) return new InputGlyph(m.glyph, m.title, m.kind);
      }
      return null;
    },
    lineMarkerChange: (update) =>
      update.transactions.some((tr) =>
        tr.effects.some((e) => e.is(setInputMarks)))
  });

  // ---- names that are no input -----------------------------------------
  // R pushes `blockr-script-warnings`: names the script reads that are not an
  // input but a data frame from a package (`cars`). Each is underlined, with
  // the note as its tooltip. Positions are line and 1-based columns.
  const setWarnMarks = StateEffect.define();

  const warnMarks = StateField.define({
    create: () => [],
    update(value, tr) {
      for (const e of tr.effects) {
        if (e.is(setWarnMarks)) return e.value;
      }
      return value;
    }
  });

  const warnDecorations = EditorView.decorations.compute(
    [warnMarks, "doc"],
    (state) => {
      const marks = (state.field(warnMarks, false) || []).slice();
      const ranges = [];
      for (const m of marks) {
        const n = Number(m && m.line);
        if (!(n >= 1 && n <= state.doc.lines)) continue;
        const line = state.doc.line(n);
        const from = line.from + Number(m.from) - 1;
        const to = line.from + Number(m.to) - 1;
        if (from < line.from || to > line.to || to <= from) continue;
        ranges.push({ from, to, title: m.title || "" });
      }
      ranges.sort((a, b) => a.from - b.from);
      const builder = new RangeSetBuilder();
      for (const r of ranges) {
        builder.add(r.from, r.to, Decoration.mark({
          class: "cm-not-input", attributes: { title: r.title }
        }));
      }
      return builder.finish();
    }
  );

  // Marks may arrive before the editor exists (a block in a dock panel that
  // has never been opened), so they are remembered on the element and applied
  // when the view is built. The initial set also rides in on `data-input-marks`
  // so a deferred mount is painted correctly on first reveal, without waiting
  // for a round trip.
  const readMarks = (el) => {
    if (el._cmMarks) return el._cmMarks;
    const raw = el.getAttribute("data-input-marks");
    if (!raw) return [];
    try {
      const parsed = JSON.parse(raw);
      return Array.isArray(parsed) ? parsed : [];
    } catch (e) {
      return [];
    }
  };

  const makeView = (el) => {
    let view;

    // Debounced report of the editor text (the "pending"/dirty path).
    const reportValue = () => {
      if (!el._cmCallback) return;
      clearTimeout(el._cmTimer);
      el._cmTimer = setTimeout(() => el._cmCallback && el._cmCallback(true), DEBOUNCE_MS);
    };

    // Execute the current document immediately: sync the editor value AND fire
    // the run-input so the server commits + runs it now (no Run click needed).
    const runNow = () => {
      if (!view) return;
      const code = view.state.doc.toString();
      Shiny.setInputValue(el.id, code, { priority: "event" });
      if (el.dataset.runInput) {
        Shiny.setInputValue(el.dataset.runInput, code, { priority: "event" });
      }
    };
    el._cmRunNow = runNow;

    // Report the live hunk count to the server (drives the "N edits" label).
    const reportCount = () => {
      if (!el.dataset.countInput || !view) return;
      const c = getChunks(view.state);
      Shiny.setInputValue(el.dataset.countInput, c ? c.chunks.length : 0,
        { priority: "event" });
    };
    el._cmReportCount = reportCount;

    // Tell the server the AI review is finished so it can drop the review
    // footer (Accept-all / Reject-all).
    const reviewDone = () => {
      el._cmMergeActive = false;
      if (el.dataset.reviewDone) {
        el._cmReviewSeq = (el._cmReviewSeq || 0) + 1;
        Shiny.setInputValue(el.dataset.reviewDone, el._cmReviewSeq, { priority: "event" });
      }
    };
    // After a hunk is accepted/rejected: end the review if none remain, else
    // refresh the count.
    const afterChunkChange = () => {
      if (!el._cmMergeActive) return;
      const c = getChunks(view.state);
      if (!c || c.chunks.length === 0) reviewDone(); else reportCount();
    };

    const updateListener = EditorView.updateListener.of((u) => {
      // Classify the change by CodeMirror-merge's user events.
      let kind = null;
      for (const tr of u.transactions) {
        if (tr.isUserEvent("revert")) kind = "revert";
        else if (tr.isUserEvent("accept")) kind = "accept";
      }
      if (kind === "revert") {        // reject hunk → run the reduced doc now
        runNow();
        afterChunkChange();
        return;
      }
      if (kind === "accept") {        // accept hunk → already live; nothing to run
        afterChunkChange();
        return;
      }
      // Programmatic set (AI / initial / reject-all) is handled by its caller.
      if (u.docChanged && !el._cmSelfWrite) {
        // The first manual keystroke ENDS an AI review: the diff was there to
        // review the AI's change, not to mark up your own typing. Clear it and
        // fall back to the normal pending/Run model.
        if (el._cmMergeActive) {
          clearMerge(el);
          reviewDone();
        }
        reportValue();
      }
    });

    const mergeCompartment = new Compartment();
    el._cmMerge = mergeCompartment;

    const state = EditorState.create({
      doc: el.getAttribute("data-value") || "",
      extensions: [
        lineNumbers(),
        history(),
        drawSelection(),
        highlightActiveLine(),
        indentUnit.of("  "),
        blockIndent,
        syntaxHighlighting(defaultHighlightStyle),
        StreamLanguage.define(r),
        autocompletion({ override: [makeRSource(el)], activateOnTyping: true }),
        inputMarks,
        inputDecorations,
        inputGutter,
        warnMarks,
        warnDecorations,
        mergeCompartment.of([]),
        // Mod-f inside the editor opens CodeMirror's own search bar: the
        // browser's find only sees the lines CodeMirror has drawn.
        search({ top: true }),
        keymap.of([
          { key: "Mod-Enter", run: () => { runNow(); return true; } },
          ...defaultKeymap, ...historyKeymap, ...completionKeymap,
          ...searchKeymap
        ]),
        tabKeys(),
        updateListener,
        EditorView.lineWrapping
      ]
    });
    view = new EditorView({ state, parent: el });
    // An Escape the editor used (closing the search bar or a completion)
    // stops at the editor, so the gear tray around it stays open.
    view.dom.addEventListener("keydown", (e) => {
      if (e.key === "Escape" && e.defaultPrevented) e.stopPropagation();
    });
    const initial = readMarks(el);
    if (initial.length) {
      view.dispatch({ effects: setInputMarks.of(initial) });
    }
    if (el._cmWarn && el._cmWarn.length) {
      view.dispatch({ effects: setWarnMarks.of(el._cmWarn) });
    }
    return view;
  };

  // Replace the document. If `original` is a string, enter the inline diff;
  // if null, clear any active diff. Either way the server input is synced.
  const setCode = (el, code, original) => {
    const view = el._cmView;
    if (!view) return;
    el._cmSelfWrite = true;
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: code == null ? "" : code }
    });
    if (el._cmMerge) {
      const isMerge = typeof original === "string";
      view.dispatch({
        effects: el._cmMerge.reconfigure(
          isMerge ? unifiedMergeView({ original, mergeControls: true }) : []
        )
      });
      el._cmMergeActive = isMerge;
      if (isMerge && el._cmReportCount) el._cmReportCount();
    }
    el._cmSelfWrite = false;
    Shiny.setInputValue(el.id, view.state.doc.toString(), { priority: "event" });
  };

  const clearMerge = (el) => {
    if (el._cmView && el._cmMerge) {
      el._cmView.dispatch({ effects: el._cmMerge.reconfigure([]) });
    }
    el._cmMergeActive = false;
  };

  // Build the editor for a mount if it doesn't exist yet. Deferred mounts
  // (see initialize) come through here on reveal; the observer is one-shot.
  const ensureView = (el) => {
    if (!el._cmView) {
      if (el._cmObserver) { el._cmObserver.disconnect(); el._cmObserver = null; }
      el._cmView = makeView(el);
    }
    return el._cmView;
  };

  // Build the editor now when the mount is visible, or on its first reveal:
  // the gear toggle's Blockr.Script.refresh call, or the observer for any
  // other reveal path.
  const mountView = (el) => {
    if (el._cmView || el._cmObserver) return;
    if (el.getClientRects().length > 0) {
      ensureView(el);
      return;
    }
    if (typeof IntersectionObserver !== "undefined") {
      const obs = new IntersectionObserver((entries) => {
        if (entries.some((entry) => entry.isIntersecting)) {
          ensureView(el).requestMeasure();
        }
      });
      obs.observe(el);
      el._cmObserver = obs;
    }
  };

  // The view, its observer and its timer go when the mount has left the page.
  // The text stays on the mount, so a mount that comes back gets it again.
  const dropView = (el) => {
    clearTimeout(el._cmTimer);
    if (el._cmObserver) { el._cmObserver.disconnect(); el._cmObserver = null; }
    if (el._cmView) {
      el.setAttribute("data-value", el._cmView.state.doc.toString());
      el._cmView.destroy();
      el._cmView = null;
    }
    el._cmMergeActive = false;
    el._cmRunNow = null;
    el._cmReportCount = null;
  };

  // ---- Blockr.Script.create: the editor without Shiny --------------------------
  // For a block on the JS block protocol (code-editor-block.js), which owns what
  // happens to the text: the editor reports edits and Mod-Enter, the block
  // decides. Same language, indentation and completion; no merge view and no
  // input marks. Its host carries `blockr-script--block`, which the binding
  // below leaves alone.
  const createEditor = (parent, opts = {}) => {
    const host = { _cmCompletions: [] };
    let selfWrite = false;
    let view;
    view = new EditorView({
      parent,
      state: EditorState.create({
        doc: opts.value || "",
        extensions: [
          lineNumbers(),
          history(),
          drawSelection(),
          highlightActiveLine(),
          indentUnit.of("  "),
          blockIndent,
          syntaxHighlighting(defaultHighlightStyle),
          StreamLanguage.define(r),
          autocompletion({ override: [makeRSource(host)], activateOnTyping: true }),
          search({ top: true }),
          keymap.of([
            { key: "Mod-Enter", run: () => { if (opts.onRun) opts.onRun(view.state.doc.toString()); return true; } },
            ...defaultKeymap, ...historyKeymap, ...completionKeymap,
            ...searchKeymap
          ]),
          tabKeys(),
          EditorView.updateListener.of((u) => {
            if (u.docChanged && !selfWrite && opts.onChange) opts.onChange(view.state.doc.toString());
          }),
          EditorView.lineWrapping
        ]
      })
    });
    view.dom.addEventListener("keydown", (e) => {
      if (e.key === "Escape" && e.defaultPrevented) e.stopPropagation();
    });
    return {
      getValue: () => view.state.doc.toString(),
      setValue: (code) => {
        selfWrite = true;
        view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code == null ? "" : code } });
        selfWrite = false;
      },
      setCompletions: (cols) => { host._cmCompletions = Array.isArray(cols) ? cols : []; },
      refresh: () => view.requestMeasure(),
      destroy: () => view.destroy()
    };
  };

  // ---- the Shiny side: the input binding and the messages from R --------------
  // Blockr.Script.create above works without Shiny; the rest needs it.
  const registerShiny = () => {
    const binding = new Shiny.InputBinding();
    Object.assign(binding, {
      find: function (scope) {
        return $(scope).find(".blockr-script:not(.blockr-script--block)");
      },
      initialize: function (el) {
        if (el._cmView) return;
        el._cmCallback = null;
        el._cmSelfWrite = false;
        el._cmMergeActive = false;
        el._cmCompletions = [];
        // Report the initial value so the server input is populated from mount.
        Shiny.setInputValue(el.id, el.getAttribute("data-value") || "");
        // The editor usually mounts inside a closed (display:none) gear section
        // the user may never open, and a full CodeMirror instance per block is
        // the one real per-block startup cost. Build eagerly only when mounted
        // visible; otherwise defer to reveal. Until then the binding answers
        // from data-value, and a `blockr-script-set` (AI write) materializes
        // the editor itself.
        mountView(el);
      },
      getValue: function (el) {
        return el._cmView
          ? el._cmView.state.doc.toString()
          : (el.getAttribute("data-value") || "");
      },
      subscribe: function (el, callback) {
        el._cmCallback = callback;
        clearTimeout(el._cmGone);
        // bound again after its view was dropped
        mountView(el);
      },
      // Shiny unbinds an input before it removes it, and also around a move.
      // The view is dropped a moment later, and only when the mount is gone.
      unsubscribe: function (el) {
        el._cmCallback = null;
        clearTimeout(el._cmTimer);
        clearTimeout(el._cmGone);
        el._cmGone = setTimeout(() => {
          if (!el.isConnected) dropView(el);
        }, GONE_MS);
      },
      getType: function () {
        return false;
      }
    });
    Shiny.inputBindings.register(binding, "blockr.script.editor");

    // R -> JS: replace the document (+ optionally enter/exit the inline diff).
    // An external write may target a still-deferred mount (gear never opened) —
    // materialize it so the diff/run state machine has a real editor to act on.
    Shiny.addCustomMessageHandler("blockr-script-set", (msg) => {
      const el = document.getElementById(msg.id);
      if (!el) return;
      ensureView(el);
      setCode(el, msg.code, msg.original);
    });

    // R -> JS: Accept all — keep the AI code (already live), just close the diff.
    Shiny.addCustomMessageHandler("blockr-script-merge-clear", (msg) => {
      const el = document.getElementById(msg.id);
      if (el) clearMerge(el);
    });

    // R -> JS: Reject all — restore the diff's own original, close it, run it.
    Shiny.addCustomMessageHandler("blockr-script-reject-all", (msg) => {
      const el = document.getElementById(msg.id);
      const view = el && el._cmView;
      if (!view) return;
      // A keystroke ends the review. A "Reject all" that was clicked before the
      // footer went has no original left to restore.
      if (!el._cmMergeActive) return;
      const orig = getOriginalDoc(view.state).toString();
      el._cmSelfWrite = true;
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: orig } });
      clearMerge(el);
      el._cmSelfWrite = false;
      if (el._cmRunNow) el._cmRunNow();   // commit + run the restored original
    });

    // R -> JS: update the column-name completions for a given editor.
    Shiny.addCustomMessageHandler("blockr-script-completions", (msg) => {
      const el = document.getElementById(msg.id);
      if (!el) return;
      el._cmCompletions = Array.isArray(msg.columns)
        ? msg.columns
        : (msg.columns == null ? [] : [msg.columns]);
    });

    // R -> JS: which lines of the script became controls, and what each became.
    Shiny.addCustomMessageHandler("blockr-script-inputs", (msg) => {
      const el = document.getElementById(msg.id);
      if (!el) return;
      const marks = Array.isArray(msg.marks)
        ? msg.marks
        : (msg.marks == null ? [] : [msg.marks]);
      el._cmMarks = marks;
      if (el._cmView) {
        el._cmView.dispatch({ effects: setInputMarks.of(marks) });
      }
    });

    // R -> JS: names the script reads that are no input (see setWarnMarks).
    Shiny.addCustomMessageHandler("blockr-script-warnings", (msg) => {
      const el = document.getElementById(msg.id);
      if (!el) return;
      const marks = Array.isArray(msg.marks)
        ? msg.marks
        : (msg.marks == null ? [] : [msg.marks]);
      el._cmWarn = marks;
      if (el._cmView) {
        el._cmView.dispatch({ effects: setWarnMarks.of(marks) });
      }
    });
  };
  if (window.Shiny) registerShiny();

  // Insert text at the cursor (a column picked from an input chip).
  const insertText = (id, text) => {
    const el = document.getElementById(id);
    if (!el || !el.classList.contains("blockr-script")) return;
    const view = ensureView(el);
    view.dispatch(view.state.replaceSelection(text));
    view.focus();
  };

  // Insert a whole line above the cursor's line ("+ Control"), and leave the
  // cursor at its end. Before the editor has been focused the cursor is at the
  // top, which is where controls usually go.
  const insertLine = (id, text) => {
    const el = document.getElementById(id);
    if (!el || !el.classList.contains("blockr-script")) return;
    const view = ensureView(el);
    const line = view.state.doc.lineAt(view.state.selection.main.head);
    const insert = text + "\n";
    view.dispatch({
      changes: { from: line.from, insert },
      selection: { anchor: line.from + text.length }
    });
    view.focus();
  };

  // The text of an editor, also before its view is built.
  const getText = (id) => {
    const el = document.getElementById(id);
    if (!el || !el.classList.contains("blockr-script")) return "";
    return el._cmView ? el._cmView.state.doc.toString() : (el.getAttribute("data-value") || "");
  };

  // Replace the whole script ("Start from"), as one undo step. With
  // `run: true` the new script is committed and run at once. Returns whether
  // there was an editor to write to.
  const replaceText = (id, text, opts = {}) => {
    const el = document.getElementById(id);
    if (!el || !el.classList.contains("blockr-script")) return false;
    const view = ensureView(el);
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: text == null ? "" : text },
      selection: { anchor: 0 }
    });
    if (opts.run && el._cmRunNow) el._cmRunNow();
    view.focus();
    return true;
  };

  // `refresh` is the reveal hook (called by the gear toggle): it builds the
  // deferred editor on first open, and re-measures, since CodeMirror
  // mis-measures when created hidden.
  window.Blockr = window.Blockr || {};
  window.Blockr.Script = {
    create: createEditor,
    insert: insertText,
    insertLine: insertLine,
    replace: replaceText,
    getValue: getText,
    refresh: (id) => {
      const el = document.getElementById(id);
      if (el && el.classList.contains("blockr-script")) {
        ensureView(el).requestMeasure();
      }
    }
  };
})();
