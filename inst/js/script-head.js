// The head row of a script block, as far as a result reaches into it.
//
// The head row (sentence, tools, gear) is drawn by the block's expression
// module, inside `.blockr-script-container[data-sb-block]`. The result is
// drawn by the block itself, in the output `<block>result`. `data-sb-block`
// holds the block's namespace prefix, which is how the two are paired here.
//
//   busy cue   R sends "blockr-script-busy" when it starts to evaluate the
//              script and again after the flush that follows, when the new
//              result or the error has gone out. The cue shows only when the
//              end takes longer than DELAY to arrive, so a fast script does
//              not flicker.
//   tools      what a result method put left of the gear (result_head() in
//              R) is hidden while the block has no result on screen.
(function () {
  "use strict";
  if (window.__blockrScriptHead) return;
  window.__blockrScriptHead = true;

  var DELAY = 300;
  var BUSY = "blockr-script-container--busy";
  var EMPTY = "blockr-script-container--no-result";
  var RESULT = "result";

  // Pending timers, by block.
  var timers = {};

  function container(block) {
    var all = document.querySelectorAll(".blockr-script-container[data-sb-block]");
    for (var i = 0; i < all.length; i++) {
      if (all[i].getAttribute("data-sb-block") === block) return all[i];
    }
    return null;
  }

  function setBusy(block, on) {
    var el = container(block);
    if (el) el.classList.toggle(BUSY, on);
    var out = document.getElementById(block + RESULT);
    if (out) {
      if (on) out.setAttribute("aria-busy", "true");
      else out.removeAttribute("aria-busy");
    }
  }

  function busy(msg) {
    if (!msg || typeof msg.block !== "string") return;
    var block = msg.block;
    if (timers[block]) {
      clearTimeout(timers[block]);
      delete timers[block];
    }
    if (msg.busy) {
      timers[block] = setTimeout(function () {
        delete timers[block];
        setBusy(block, true);
      }, DELAY);
    } else {
      setBusy(block, false);
    }
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
      Object.keys(timers).forEach(function (block) {
        clearTimeout(timers[block]);
        delete timers[block];
      });
      var on = document.querySelectorAll("." + BUSY);
      for (var i = 0; i < on.length; i++) on[i].classList.remove(BUSY);
    });
  }

  // For the node test.
  window.__blockrScriptHeadTest = {
    busy: busy, isEmpty: isEmpty, resultChanged: resultChanged, DELAY: DELAY
  };
})();
