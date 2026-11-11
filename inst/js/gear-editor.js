// The code editor's gear (gear_editor_ui() in R/code-editor.R).
//
// The gear opens the editor in the design system's gear tray
// (Blockr.gearTray, blockr.ui). When the block is wide, the open tray moves
// beside the output: the nearest element holding both the tray and the
// output (`data-side-with`) becomes a two-column grid. What comes before the
// tray's branch keeps the full width, the tray's branch takes the right
// column, and what follows it stacks in the left column. In the dock that
// host is the card's accordion (inputs panel right, output panel left); a
// table block can mark its table container the same way (editor right,
// table left).
(function () {
  "use strict";

  // Host width (px) below which the tray stays above the output.
  var SIDE_MIN = 900;
  // The narrowest the editor and the output may get when the divider moves.
  var TRAY_MIN = 320;
  var MAIN_MIN = 280;
  // An element that left the page keeps its observers this long, in case it
  // was only moved.
  var GONE_MS = 1000;

  // The divider's width is kept per block id in localStorage. Nothing says
  // when a block is deleted for good, so the number of kept widths is capped
  // and the ones written longest ago go first.
  var SPLIT_PREFIX = "blockr-script-split:";
  var SPLIT_KEEP = 50;
  function trimStored(current) {
    var ls = window.localStorage;
    var keys = [];
    for (var i = 0; i < ls.length; i++) {
      var k = ls.key(i);
      if (k && k.indexOf(SPLIT_PREFIX) === 0 && k !== current) keys.push(k);
    }
    // localStorage keeps no order, so which of the old ones go is arbitrary.
    while (keys.length >= SPLIT_KEEP) ls.removeItem(keys.shift());
  }

  // An id inside an attribute selector.
  function esc(id) {
    return window.CSS && CSS.escape ? CSS.escape(id) : String(id).replace(/["\\]/g, "\\$&");
  }

  // The Run button's hint names the editor's Mod-Enter: Cmd on a Mac, Ctrl
  // elsewhere (code-block.css shows the one that applies).
  if (/Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent)) {
    document.documentElement.classList.add("blockr-script-mac");
  }

  /**
   * @param {HTMLElement} band
   * @returns {{host: HTMLElement, branch: HTMLElement} | null}
   */
  function sideHost(band) {
    var partner = document.getElementById(band.getAttribute("data-side-with") || "");
    if (!partner) return null;
    var branch = band;
    var host = band.parentElement;
    while (host && !host.contains(partner)) {
      branch = host;
      host = host.parentElement;
    }
    return host ? { host: host, branch: branch, partner: partner } : null;
  }

  // The nearest ancestor that scrolls: the dock panel a block sits in.
  function scroller(el) {
    for (var p = el.parentElement; p && p !== document.body; p = p.parentElement) {
      var o = getComputedStyle(p).overflowY;
      if (o === "auto" || o === "scroll") return p;
    }
    return null;
  }

  /**
   * @param {HTMLElement} band
   * @param {Array<{disconnect: function(): void}>} watchers
   */
  function followSide(band, watchers) {
    var found = sideHost(band);
    if (!found) return;
    var host = found.host;
    var branch = found.branch;
    var partner = found.partner;

    function apply() {
      var open = band.classList.contains("blockr-settings--open");
      var kids = Array.prototype.slice.call(host.children);
      var at = kids.indexOf(branch);
      var shows = function (k) { return getComputedStyle(k).display !== "none"; };
      var row = kids.slice(0, at).filter(shows).length + 1;
      var main = kids.slice(at + 1).filter(shows);
      // A closed output panel leaves nothing to sit beside. (The output
      // itself may draw no box, so look for a hidden element above it.)
      var shown = true;
      for (var el = partner; el && el !== host; el = el.parentElement) {
        if (getComputedStyle(el).display === "none") { shown = false; break; }
      }
      var on = open && shown && main.length > 0 &&
        host.clientWidth >= SIDE_MIN;
      host.classList.toggle("blockr-script-side", on);
      branch.classList.toggle("blockr-script-side__tray", on);
      // The tray's branch spans the rows of everything stacked beside it.
      branch.style.gridRow = on ? row + " / span " + main.length : "";
      kids.slice(at + 1).forEach(function (k) {
        k.classList.toggle("blockr-script-side__main", on);
      });
      divider(on);
      fillColumn(on, main);
    }

    // The divider between output and editor: drag to set the editor's width,
    // double-click (or Enter) to go back to the default, the arrow keys to
    // move it from the keyboard. The width is kept per block in this
    // browser, not in the board.
    var key = SPLIT_PREFIX + band.id;
    var handle = null;
    function stored() {
      try { return parseFloat(window.localStorage.getItem(key)) || 0; } catch (e) { return 0; }
    }
    function store(w) {
      try {
        if (w > 0) {
          window.localStorage.setItem(key, String(w));
          trimStored(key);
        } else {
          window.localStorage.removeItem(key);
        }
      } catch (err) { /* no storage */ }
    }
    // The widest and the narrowest the editor may be in this host.
    function bounds() {
      var w = host.getBoundingClientRect().width;
      return { min: TRAY_MIN, max: Math.max(TRAY_MIN, w - MAIN_MIN) };
    }
    function trayWidth() {
      return parseFloat(host.style.getPropertyValue("--blockr-script-tray-w")) ||
        branch.getBoundingClientRect().width;
    }
    function setWidth(w) {
      if (w > 0) {
        host.style.setProperty("--blockr-script-tray-w", Math.round(w) + "px");
      } else {
        host.style.removeProperty("--blockr-script-tray-w");
      }
      describe();
    }
    // What a screen reader hears: the editor's width in px, and its range.
    function describe() {
      if (!handle || !handle.parentElement) return;
      var b = bounds();
      handle.setAttribute("aria-valuemin", String(Math.round(b.min)));
      handle.setAttribute("aria-valuemax", String(Math.round(b.max)));
      handle.setAttribute("aria-valuenow", String(Math.round(trayWidth())));
    }
    function reset() {
      store(0);
      setWidth(0);
      window.requestAnimationFrame(apply);
    }
    function divider(on) {
      if (!on) {
        if (handle && handle.parentElement) handle.parentElement.removeChild(handle);
        setWidth(0);
        return;
      }
      if (!handle) {
        handle = document.createElement("div");
        handle.className = "blockr-script-splitter";
        handle.setAttribute("role", "separator");
        handle.setAttribute("aria-orientation", "vertical");
        handle.setAttribute("aria-label", "Width of the script editor");
        handle.tabIndex = 0;
        handle.title = "Drag to resize, double-click to reset";
        // Pointer events with capture: the handle keeps getting the moves and
        // the release when the pointer is over an iframe (an html_page in the
        // output), which would swallow mouse events. They cover touch too.
        handle.addEventListener("pointerdown", function (e) {
          if (e.button !== undefined && e.button !== 0) return;
          e.preventDefault();
          try { handle.setPointerCapture(e.pointerId); } catch (err) { /* old browser */ }
          handle.classList.add("is-dragging");
          document.documentElement.classList.add("blockr-script-resizing");
          function move(ev) {
            var r = host.getBoundingClientRect();
            var b = bounds();
            setWidth(Math.min(Math.max(r.right - ev.clientX, b.min), b.max));
          }
          function up() {
            handle.removeEventListener("pointermove", move);
            handle.removeEventListener("pointerup", up);
            handle.removeEventListener("pointercancel", up);
            handle.classList.remove("is-dragging");
            document.documentElement.classList.remove("blockr-script-resizing");
            store(parseFloat(host.style.getPropertyValue("--blockr-script-tray-w")) || 0);
            window.requestAnimationFrame(apply);
          }
          handle.addEventListener("pointermove", move);
          handle.addEventListener("pointerup", up);
          handle.addEventListener("pointercancel", up);
        });
        handle.addEventListener("dblclick", reset);
        // The editor is right of the divider: ArrowLeft makes it wider.
        handle.addEventListener("keydown", function (e) {
          var b = bounds();
          var by = e.shiftKey ? 64 : 16;
          var w = null;
          if (e.key === "ArrowLeft") w = trayWidth() + by;
          else if (e.key === "ArrowRight") w = trayWidth() - by;
          else if (e.key === "Home") w = b.max;
          else if (e.key === "End") w = b.min;
          else if (e.key === "Enter") { e.preventDefault(); reset(); return; }
          if (w === null) return;
          e.preventDefault();
          w = Math.min(Math.max(w, b.min), b.max);
          setWidth(w);
          store(w);
          window.requestAnimationFrame(apply);
        });
      }
      if (handle.parentElement !== branch) branch.appendChild(handle);
      var w = stored();
      if (w && !host.style.getPropertyValue("--blockr-script-tray-w")) setWidth(w);
      describe();
    }

    // Beside the output, the editor takes the full height: it grows until the
    // block reaches the bottom of the panel it scrolls in, or the bottom of
    // the output beside it if that is lower. A script longer than that sets
    // the height itself.
    function fillColumn(on, main) {
      var cm = band.querySelector(".cm-editor");
      if (!cm) return;
      var was = cm.style.minHeight;
      cm.style.minHeight = "";
      if (!on) return;
      var cont = band.closest(".blockr-script-container") || band;
      var right = cont.getBoundingClientRect().bottom;
      var left = 0;
      main.forEach(function (k) {
        left = Math.max(left, k.getBoundingClientRect().bottom);
      });
      var slack = left - right;
      var sc = scroller(host);
      if (sc) {
        var sr = sc.getBoundingClientRect();
        var pad = parseFloat(getComputedStyle(sc).paddingBottom) || 0;
        var content = 0;
        Array.prototype.forEach.call(sc.children, function (k) {
          content = Math.max(content, k.getBoundingClientRect().bottom);
        });
        var free = sr.top + sc.clientHeight - pad - content - 8;
        slack = Math.max(slack, free);
      }
      if (slack > 2) {
        cm.style.minHeight = Math.round(cm.getBoundingClientRect().height + slack) + "px";
      } else if (was && Math.abs(slack) <= 2) {
        cm.style.minHeight = was;
      }
    }

    // --open is on from the start of the slide-in to the end of the
    // slide-out: the tray moves aside as it opens and back once it is shut.
    var mo = new MutationObserver(apply);
    mo.observe(band, { attributes: true, attributeFilter: ["class"] });
    watchers.push(mo);
    // Width decides the mode; height changes when the output panel opens.
    // Deferred a frame: apply() changes the host's size itself.
    if (typeof ResizeObserver === "function") {
      var ro = new ResizeObserver(function () {
        // A removed host reports a size of zero: the block may be gone.
        if (!band.isConnected) { unwatchLater(band); return; }
        window.requestAnimationFrame(apply);
      });
      ro.observe(host);
      // The panel's own height decides how far the editor reaches.
      var sc = scroller(host);
      if (sc) ro.observe(sc);
      watchers.push(ro);
    }
  }

  // The observers of a band that has left the page are disconnected, after a
  // pause. A band that comes back is watched again (see init).
  function unwatchLater(band) {
    if (band._blockrScriptGone) return;
    band._blockrScriptGone = setTimeout(function () {
      band._blockrScriptGone = null;
      if (band.isConnected || !band._blockrScriptWatch) return;
      band._blockrScriptWatch.forEach(function (w) { w.disconnect(); });
      band._blockrScriptWatch = null;
    }, GONE_MS);
  }

  /**
   * @param {HTMLElement} band
   * @param {Array<{disconnect: function(): void}>} watchers
   */
  function refreshOnOpen(band, watchers) {
    var code = band.getAttribute("data-code");
    var opened = band.getAttribute("data-opened-input");
    var told = !!band._blockrScriptTold;
    var mo = new MutationObserver(function () {
      if (!band.classList.contains("blockr-settings--open")) return;
      // Tell R once that the tray has been opened: what the tray shows about
      // the inputs is only built from then on.
      if (!told && opened && window.Shiny && Shiny.setInputValue) {
        told = true;
        band._blockrScriptTold = true;
        Shiny.setInputValue(opened, true);
      }
      if (window.Blockr && window.Blockr.Script) {
        window.Blockr.Script.refresh(code);
      }
    });
    mo.observe(band, { attributes: true, attributeFilter: ["class"] });
    watchers.push(mo);
  }

  // While the tray is shut, the sentence sits in the gear's row and the
  // controls grid under it. Once the tray opens, both move to directly above
  // the output they act on (the left column when the block is wide, under
  // the tray when narrow), and back on close. The tray then holds only what
  // builds the block.
  /**
   * @param {HTMLElement} band
   * @param {Array<{disconnect: function(): void}>} watchers
   */
  function followFace(band, watchers) {
    var gear = document.querySelector('[data-tray="' + esc(band.id) + '"]');
    var header = gear && gear.parentElement;
    var output = document.getElementById(band.getAttribute("data-side-with") || "");
    var cont = band.closest(".blockr-script-container");
    if (!header || !output) return;
    var sentence = header.querySelector(".blockr-script-sentence-out");
    var params = cont && cont.querySelector(":scope > .blockr-script-params");
    function place() {
      var open = band.classList.contains("blockr-settings--open");
      var above = output.parentElement;
      if (open) {
        // Sentence first, then the controls, then the output.
        [sentence, params].forEach(function (el) {
          if (!el) return;
          above.insertBefore(el, output);
          el.classList.add("blockr-script-face--above");
        });
      } else {
        if (sentence && sentence.parentElement !== header) {
          header.insertBefore(sentence, gear);
          sentence.classList.remove("blockr-script-face--above");
        }
        if (params && params.parentElement !== cont) {
          cont.appendChild(params);
          params.classList.remove("blockr-script-face--above");
        }
      }
    }
    var mo = new MutationObserver(place);
    mo.observe(band, { attributes: true, attributeFilter: ["class"] });
    watchers.push(mo);
  }

  /** @param {HTMLElement} band */
  function init(band) {
    var gear = document.querySelector('[data-tray="' + esc(band.id) + '"]');
    if (!gear) return;
    if (!band._blockrTray) {
      band._blockrTray = window.Blockr.gearTray(band, gear, {
        label: band.getAttribute("aria-label") || "Settings"
      });
    }
    // The observers are kept on the band, so they can be disconnected when
    // the band leaves the page and set up again when it comes back.
    clearTimeout(band._blockrScriptGone);
    band._blockrScriptGone = null;
    if (band._blockrScriptWatch) return;
    var watchers = band._blockrScriptWatch = [];
    refreshOnOpen(band, watchers);
    followFace(band, watchers);
    followSide(band, watchers);
  }

  // The editor inside the tray is a Shiny input: once it is bound, the
  // tray, its gear and the output beside it are all in the document.
  $(document).on("shiny:bound", function (e) {
    var band = e.target.closest && e.target.closest(".blockr-script-tray");
    if (band) init(band);
  });
})();
