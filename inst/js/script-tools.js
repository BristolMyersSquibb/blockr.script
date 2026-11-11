// The row above the script editor (sb_inputs_ui() in R/script-block.R).
//
//   input chip   opens a searchable list of what that input holds: columns,
//                a dm's tables, a list's elements, a model's coefficients. A
//                pick inserts the entry's `insert`, or its name; the first
//                entry inserts the input's own name
//   + Control    a menu of ready lines; a pick inserts the line above the
//                cursor's line, where it becomes a control. A note under the
//                menu says the rule.
//   Start from   a menu of whole scripts (table, chart, HTML page, external
//                page); a pick replaces the script and runs it, after a
//                question when the script is not empty. One undo step brings
//                the old script back while the editor lives.
//
// One popover at a time. The chips are re-rendered by R, so the listeners are
// delegated.
(function () {
  "use strict";

  var ITEM = ".blockr-script-pop__item";
  var open = null;
  var warned = false;

  function close(refocus) {
    if (!open) return;
    var was = open;
    open = null;
    if (was.layer) was.layer.remove();
    if (was.placement) was.placement.stop();
    if (was.watch) was.watch.disconnect();
    was.el.remove();
    was.anchor.setAttribute("aria-expanded", "false");
    // Focus goes back to the button, unless R has redrawn the row and the
    // button is no longer on the page.
    if (refocus && was.anchor.isConnected) was.anchor.focus();
  }

  // Without blockr.ui the popover is placed once, under its anchor.
  function placeOnce(pop, anchor) {
    var r = anchor.getBoundingClientRect();
    pop.style.top = (window.scrollY + r.bottom + 4) + "px";
    var left = window.scrollX + r.left;
    var maxLeft = window.scrollX + document.documentElement.clientWidth -
      pop.offsetWidth - 8;
    pop.style.left = Math.max(8, Math.min(left, maxLeft)) + "px";
  }

  // A popover is a layer on the design system's dismiss stack: Escape and a
  // click outside close it, and the gear tray under it stays open.
  function popover(anchor, cls, role) {
    close();
    var pop = document.createElement("div");
    pop.className = "blockr-script-pop " + (cls || "");
    pop.setAttribute("role", role);
    var label = anchor.getAttribute("aria-label") || anchor.textContent.trim();
    if (label) pop.setAttribute("aria-label", label);
    document.body.appendChild(pop);
    anchor.setAttribute("aria-expanded", "true");
    open = { el: pop, anchor: anchor, layer: null, placement: null, watch: null };

    var B = window.Blockr;
    if (B && B.layer) {
      open.layer = B.layer(pop, {
        from: anchor,
        escape: function () { close(true); },
        outside: function () { close(); }
      });
    } else {
      // No dismiss stack: Escape in the popover and a click outside close it.
      if (!warned) {
        warned = true;
        console.warn("[blockr.script] blockr.ui is not loaded; the menus of the script tools are placed once and do not follow the page.");
      }
      pop.addEventListener("keydown", function (e) {
        if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); close(true); }
      });
    }

    // R redraws the chip row when an input changes. A popover whose anchor
    // has left the page closes.
    if (typeof MutationObserver !== "undefined") {
      var mine = open;
      open.watch = new MutationObserver(function () {
        if (open === mine && !anchor.isConnected) close();
      });
      open.watch.observe(document.body, { childList: true, subtree: true });
    }

    // Tab from the last item would leave the page, because the popover is
    // the last child of <body>. Tab closes it and the focus goes on from the
    // button it belongs to.
    pop.addEventListener("keydown", function (e) {
      if (e.key === "Tab") close(true);
    });
    return pop;
  }

  // Called when the popover has its content: under the anchor, following it
  // on scroll and resize, above it when there is no room below.
  function place(pop, anchor) {
    var B = window.Blockr;
    if (B && B.place && open && open.el === pop) {
      open.placement = B.place(pop, anchor, { gap: 4, width: { min: 240, max: 380 } });
    } else {
      placeOnce(pop, anchor);
    }
  }

  function script() {
    return window.Blockr && window.Blockr.Script;
  }

  function insert(editor, text) {
    if (script()) script().insert(editor, text);
  }

  var RESERVED = [
    "if", "else", "repeat", "while", "function", "for", "next", "break", "in",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_character_", "NA_complex_"
  ];

  // A name as R code: as it is, or in backticks when it is not a plain name
  // or is a reserved word.
  function symbol(name) {
    var plain = /^[A-Za-z.][A-Za-z0-9._]*$/.test(name) &&
      !/^\.[0-9]/.test(name) && RESERVED.indexOf(name) === -1;
    return plain ? name : "`" + name + "`";
  }

  function item(parts, onPick, role) {
    var b = document.createElement("button");
    b.type = "button";
    b.className = "blockr-script-pop__item";
    b.setAttribute("role", role || "menuitem");
    parts.forEach(function (p) {
      var s = document.createElement("span");
      s.className = p[0];
      s.textContent = p[1];
      b.appendChild(s);
    });
    b.addEventListener("click", function () {
      close();
      onPick();
    });
    return b;
  }

  // Arrow keys, Home and End move through the items of `box`. ArrowUp on the
  // first item goes to `before` when there is one (the search field).
  function arrowKeys(box, before) {
    box.addEventListener("keydown", function (e) {
      var items = Array.prototype.slice.call(box.querySelectorAll(ITEM));
      if (!items.length) return;
      var at = items.indexOf(document.activeElement);
      var to = null;
      if (e.key === "ArrowDown") to = items[Math.min(items.length - 1, at + 1)];
      else if (e.key === "ArrowUp") to = at > 0 ? items[at - 1] : (before || items[0]);
      else if (e.key === "Home") to = items[0];
      else if (e.key === "End") to = items[items.length - 1];
      if (!to) return;
      e.preventDefault();
      to.focus();
    });
  }

  function readJson(el, attr) {
    try {
      var x = JSON.parse(el.getAttribute(attr) || "[]");
      return Array.isArray(x) ? x : [];
    } catch (e) {
      return [];
    }
  }

  // ---- input chip: search what the input holds ----------------------------

  function openColumns(chip) {
    var cols = readJson(chip, "data-cols");
    var editor = chip.getAttribute("data-editor");
    var name = chip.getAttribute("data-insert") || "";
    var pop = popover(chip, "blockr-script-pop--cols", "dialog");

    var search = document.createElement("input");
    search.type = "search";
    search.className = "blockr-script-pop__search";
    var what = chip.getAttribute("data-what") || "";
    search.placeholder = cols.length && what ? "Search " + what : "Nothing to search";
    search.setAttribute("aria-label", "Search " + (what || "the input"));
    pop.appendChild(search);

    var list = document.createElement("div");
    list.className = "blockr-script-pop__list";
    list.setAttribute("role", "listbox");
    pop.appendChild(list);

    function render() {
      var q = search.value.trim().toLowerCase();
      list.textContent = "";
      if (!q) {
        list.appendChild(item(
          [["blockr-script-pop__name", name], ["blockr-script-pop__hint", "insert the input"]],
          function () { insert(editor, name); },
          "option"
        ));
      }
      var shown = 0;
      cols.forEach(function (c) {
        var hay = (c.name + " " + (c.label || "")).toLowerCase();
        if (q && hay.indexOf(q) === -1) return;
        shown++;
        list.appendChild(item(
          [["blockr-script-pop__name", c.name],
           ["blockr-script-pop__type", c.type],
           ["blockr-script-pop__hint", c.label || ""]],
          function () { insert(editor, c.insert || symbol(c.name)); },
          "option"
        ));
      });
      if (q && !shown) {
        var none = document.createElement("div");
        none.className = "blockr-script-pop__empty";
        none.textContent = "No match";
        list.appendChild(none);
      }
    }

    search.addEventListener("input", render);
    search.addEventListener("keydown", function (e) {
      var first = list.querySelector(ITEM);
      if (!first) return;
      if (e.key === "Enter") { e.preventDefault(); first.click(); }
      else if (e.key === "ArrowDown") { e.preventDefault(); first.focus(); }
    });
    arrowKeys(list, search);

    render();
    place(pop, chip);
    search.focus();
  }

  // ---- + Control and Start from: a menu with a caption and a note -----------

  function openMenu(btn, caption, entries, row) {
    var pop = popover(btn, "blockr-script-pop--controls", "menu");
    var cap = document.createElement("div");
    cap.className = "blockr-script-pop__caption";
    cap.textContent = caption;
    pop.appendChild(cap);
    entries.forEach(function (x) { pop.appendChild(row(x)); });
    var note = btn.getAttribute("data-note");
    if (note) {
      var n = document.createElement("div");
      n.className = "blockr-script-pop__note";
      n.textContent = note;
      pop.appendChild(n);
    }
    arrowKeys(pop);
    place(pop, btn);
    var first = pop.querySelector(ITEM);
    if (first) first.focus();
  }

  function openControls(btn) {
    var editor = btn.getAttribute("data-editor");
    openMenu(btn, "Insert a line that becomes a control", readJson(btn, "data-templates"), function (t) {
      var b = item(
        [["blockr-script-pop__label", t.label], ["blockr-script-pop__hint", t.hint || ""]],
        function () { if (script()) script().insertLine(editor, t.line); }
      );
      b.title = t.line;
      return b;
    });
  }

  // A starter replaces the whole script and runs it. Undo only brings the old
  // script back while the same editor lives, so a script that is not empty
  // is replaced only after a yes.
  function replaceScript(editor, starter) {
    var S = script();
    if (!S || !S.replace) return;
    var current = S.getValue ? S.getValue(editor) : "";
    if (current.trim() && current.trim() !== String(starter.script).trim() &&
        !window.confirm("Replace the script with the \"" + starter.label + "\" example?")) {
      return;
    }
    S.replace(editor, starter.script, { run: true });
  }

  function openStarters(btn) {
    var editor = btn.getAttribute("data-editor");
    openMenu(btn, "Start from an example script", readJson(btn, "data-starters"), function (s) {
      return item(
        [["blockr-script-pop__label", s.label], ["blockr-script-pop__hint", s.hint || ""]],
        function () { replaceScript(editor, s); }
      );
    });
  }

  // ---- Run -------------------------------------------------------------------

  // The footer's Run button sends the text the editor shows now to the input
  // Cmd-Enter writes. Reading the debounced editor input on the server instead
  // could commit text one keystroke behind.
  function run(btn) {
    var id = btn.getAttribute("data-editor");
    var el = id && document.getElementById(id);
    if (!el || !window.Shiny || !window.Blockr || !window.Blockr.Script) return;
    var target = el.getAttribute("data-run-input");
    if (!target) return;
    window.Shiny.setInputValue(
      target, window.Blockr.Script.getValue(id), { priority: "event" }
    );
  }

  document.addEventListener("click", function (e) {
    var t = e.target;
    if (!t.closest) return;
    var runBtn = t.closest(".blockr-script-btn--run");
    if (runBtn) {
      if (!runBtn.disabled) run(runBtn);
      return;
    }
    var chip = t.closest(".blockr-script-chip");
    var add = t.closest(".blockr-script-add");
    var start = t.closest(".blockr-script-start");
    var target = chip || add || start;
    if (target) {
      e.preventDefault();
      if (open && open.anchor === target) { close(); return; }
      if (chip) openColumns(chip);
      else if (add) openControls(add);
      else openStarters(start);
    }
  });

  // Without Blockr.place nothing moves the popover, so a resize closes it.
  window.addEventListener("resize", function () {
    if (open && !open.placement) close();
  });
})();
