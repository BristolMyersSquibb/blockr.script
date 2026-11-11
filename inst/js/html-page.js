// Board side of html_page().
//
// Height. The page is sandboxed without allow-same-origin, so the board cannot
// read its size. The page posts its height and this listener sizes the frame
// that sent the message. A frame can only resize itself.
//
// Document. On a script block the page's document does not travel inside the
// output's HTML. R sends it as the message "blockr-html-page-doc" and draws
// the frame empty, with `data-doc` naming the document it waits for. The two
// can arrive in either order, so the document is kept per frame id and put
// into the frame once both are there and the labels match.
(function () {
  if (window.__blockrHtmlPage) return;
  window.__blockrHtmlPage = true;

  var MAX = 20000;         // tallest frame, in px
  var SAME_STEP = 10;      // this many equal steps up in a row is a loop
  var MAX_GROWTH = 200;    // this many steps up in a row is a loop too
  var SELECTOR = "iframe.blockr-html-page[data-auto]";
  var WAITING = "iframe.blockr-html-page[data-doc]";

  // Last height per output, so a re-rendered frame starts where the old one
  // ended and the card does not jump.
  var lastHeight = {};
  // Growth bookkeeping per frame element.
  var growth = new WeakMap();

  function keyOf(frame) {
    var host = frame.closest ? frame.closest("[id]") : null;
    return host ? host.id : null;
  }

  // The height to set for a report of `h`, or null to leave the frame alone.
  // A page whose body is as tall as the frame plus a margin reports a little
  // more after every resize. That would grow the frame without end, so the
  // frame stops following after a run of equal steps up, or a long run of
  // steps up of any size.
  function nextHeight(state, h) {
    if (typeof h !== "number" || !isFinite(h) || h < 0) return null;
    if (state.frozen) return null;
    h = Math.min(Math.ceil(h), MAX);
    if (state.height != null && h > state.height) {
      var step = h - state.height;
      state.same = Math.abs(step - state.step) <= 1 ? state.same + 1 : 1;
      state.step = step;
      state.ups += 1;
      if (state.same >= SAME_STEP || state.ups >= MAX_GROWTH) {
        state.frozen = true;
        return null;
      }
    } else {
      state.same = 0;
      state.step = 0;
      state.ups = 0;
    }
    state.height = h;
    return h;
  }

  function stateOf(frame) {
    var s = growth.get(frame);
    if (!s) {
      s = { height: null, step: 0, same: 0, ups: 0, frozen: false };
      growth.set(frame, s);
    }
    return s;
  }

  window.addEventListener("message", function (e) {
    if (!e.data || !e.data.blockrHtmlPage) return;
    var frames = document.querySelectorAll(SELECTOR);
    for (var i = 0; i < frames.length; i++) {
      if (frames[i].contentWindow !== e.source) continue;
      var h = nextHeight(stateOf(frames[i]), e.data.height);
      if (h == null) continue;
      frames[i].style.height = h + "px";
      var key = keyOf(frames[i]);
      if (key) lastHeight[key] = h;
    }
  });

  // The latest document per frame id: { doc, html }.
  var docs = {};

  // Put the document into a frame that waits for it. Returns whether it did.
  function fill(frame) {
    var want = frame.getAttribute("data-doc");
    var have = docs[frame.id];
    if (!want || !have || have.doc !== want) return false;
    if (frame.getAttribute("data-doc-set") === want) return false;
    frame.setAttribute("data-doc-set", want);
    frame.srcdoc = have.html;
    return true;
  }

  function receive(msg) {
    if (!msg || !msg.id) return;
    docs[msg.id] = { doc: String(msg.doc), html: msg.html == null ? "" : msg.html };
    var frame = document.getElementById(msg.id);
    if (frame) fill(frame);
  }

  if (window.Shiny && window.Shiny.addCustomMessageHandler) {
    window.Shiny.addCustomMessageHandler("blockr-html-page-doc", receive);
  }

  // A new frame takes its document if that came first, and the last height of
  // its output before the first paint.
  function adopt(node) {
    if (!node || node.nodeType !== 1) return;
    var waiting = node.matches && node.matches(WAITING)
      ? [node]
      : node.querySelectorAll ? node.querySelectorAll(WAITING) : [];
    for (var k = 0; k < waiting.length; k++) fill(waiting[k]);
    var frames = node.matches && node.matches(SELECTOR)
      ? [node]
      : node.querySelectorAll ? node.querySelectorAll(SELECTOR) : [];
    for (var i = 0; i < frames.length; i++) {
      var key = keyOf(frames[i]);
      if (key && lastHeight[key]) {
        frames[i].style.height = lastHeight[key] + "px";
      }
    }
  }

  if (window.MutationObserver && document.documentElement) {
    new MutationObserver(function (records) {
      for (var i = 0; i < records.length; i++) {
        var added = records[i].addedNodes;
        for (var j = 0; j < added.length; j++) adopt(added[j]);
      }
    }).observe(document.documentElement, { childList: true, subtree: true });
  }

  // For the node test.
  window.__blockrHtmlPageTest = {
    nextHeight: nextHeight, MAX: MAX, receive: receive, fill: fill
  };
})();
