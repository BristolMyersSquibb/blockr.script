// The head row of a script block, as far as a result reaches into it.
//
// The head row (sentence, tools, gear) is drawn by the block's expression
// module, inside `.blockr-script-container[data-sb-block]`. The result is
// drawn by the block itself, in the output `<block>result`. `data-sb-block`
// holds the block's namespace prefix, which is how the two are paired here.
//
//   busy cue   R sends "blockr-script-busy" when it starts to evaluate the
//              script and again after the flush that follows, when the new
//              result or the error has gone out. When the end takes longer
//              than DELAY to arrive, the result dims and a line above it
//              counts the wait ("computing… 1.2 s"), as blockr.viz's outputs
//              do. A fast script does not flicker.
//   tools      what a result method put left of the gear (result_head() in
//              R) is hidden while the block has no result on screen.
(function () {
  "use strict";
  if (window.__blockrScriptHead) return;
  window.__blockrScriptHead = true;

  var DELAY = 300;
  var TICK = 100;
  var LABEL = "computing";
  var BUSY = "blockr-script-container--busy";
  var DIM = "blockr-script-result--busy";
  var LINE = "blockr-script-busy-line";
  var LINE_ON = "blockr-script-busy-line--on";
  var EMPTY = "blockr-script-container--no-result";
  var RESULT = "result";

  // Evaluations under way, by block: when R started, and the timers.
  var pending = {};

  function container(block) {
    var all = document.querySelectorAll(".blockr-script-container[data-sb-block]");
    for (var i = 0; i < all.length; i++) {
      if (all[i].getAttribute("data-sb-block") === block) return all[i];
    }
    return null;
  }

  // The line that counts the wait sits right above the result output, outside
  // it, so a new result does not replace it. A dock that builds the card
  // again drops it; it is made again on the next tick.
  function busyLine(out, make) {
    if (!out || !out.parentNode) return null;
    var prev = out.previousElementSibling;
    if (prev && prev.classList && prev.classList.contains(LINE)) return prev;
    if (!make) return null;
    var line = document.createElement("div");
    line.className = LINE;
    line.setAttribute("aria-live", "polite");
    out.parentNode.insertBefore(line, out);
    return line;
  }

  function setBusy(block, on, text) {
    var el = container(block);
    if (el) el.classList.toggle(BUSY, on);
    var out = document.getElementById(block + RESULT);
    if (out) {
      if (on) out.setAttribute("aria-busy", "true");
      else out.removeAttribute("aria-busy");
      if (out.classList) out.classList.toggle(DIM, on);
    }
    var line = busyLine(out, on);
    if (line) {
      line.classList.toggle(LINE_ON, on);
      if (on) line.textContent = text;
    }
  }

  function stop(block) {
    var p = pending[block];
    if (p) {
      if (p.timer) clearTimeout(p.timer);
      if (p.clock) clearInterval(p.clock);
      delete pending[block];
    }
  }

  // A second start while one is pending keeps the first clock: the wait the
  // reader sees began then.
  function busy(msg) {
    if (!msg || typeof msg.block !== "string") return;
    var block = msg.block;
    if (!msg.busy) {
      stop(block);
      setBusy(block, false);
      return;
    }
    if (pending[block]) return;
    var p = { t0: Date.now(), timer: null, clock: null };
    pending[block] = p;
    var tick = function () {
      if (pending[block] !== p) return;
      var secs = (Date.now() - p.t0) / 1000;
      setBusy(block, true, LABEL + "\u2026 " + secs.toFixed(1) + " s");
    };
    p.timer = setTimeout(function () {
      p.timer = null;
      tick();
      p.clock = setInterval(tick, TICK);
    }, DELAY);
  }

  // Is this the value of an output with nothing in it?
  function isEmpty(value) {
    if (value == null || value === "") return true;
    if (typeof value === "object" && "html" in value) {
      return value.html == null || String(value.html).trim() === "";
    }
    return false;
  }

  // The result output of a block got a value or an error.
  function resultChanged(id, empty) {
    if (typeof id !== "string") return;
    if (id.slice(-RESULT.length) !== RESULT) return;
    var el = container(id.slice(0, -RESULT.length));
    if (el) el.classList.toggle(EMPTY, empty);
  }

  if (window.Shiny && window.Shiny.addCustomMessageHandler) {
    window.Shiny.addCustomMessageHandler("blockr-script-busy", busy);
  }
  if (window.jQuery) {
    window.jQuery(document).on("shiny:value", function (e) {
      resultChanged(e.target && e.target.id, isEmpty(e.value));
    });
    window.jQuery(document).on("shiny:error", function (e) {
      resultChanged(e.target && e.target.id, true);
    });
    // A lost connection sends no end.
    window.jQuery(document).on("shiny:disconnected", function () {
      Object.keys(pending).forEach(function (block) {
        stop(block);
        setBusy(block, false);
      });
    });
  }

  // For the node test.
  window.__blockrScriptHeadTest = {
    busy: busy, isEmpty: isEmpty, resultChanged: resultChanged, DELAY: DELAY,
    TICK: TICK
  };
})();
