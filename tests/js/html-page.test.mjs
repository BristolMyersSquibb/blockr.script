import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

// Load inst/js/html-page.js against a small fake page: two auto frames in
// two outputs, and a window that records the message listener.
function load() {
  const frame = (id) => ({
    contentWindow: {},
    style: { height: "150px" },
    closest: () => ({ id }),
  });
  const frames = [frame("out-a"), frame("out-b")];
  let onMessage = null;
  const window = {
    addEventListener: (type, fn) => { if (type === "message") onMessage = fn; },
  };
  const document = { querySelectorAll: () => frames };
  const src = readFileSync(new URL("../../inst/js/html-page.js", import.meta.url), "utf8");
  vm.runInNewContext(src, { window, document, WeakMap, Math, isFinite });
  const post = (source, height) =>
    onMessage({ source, data: { blockrHtmlPage: true, height } });
  return { frames, post, onMessage, api: window.__blockrHtmlPageTest };
}

test("only the frame that sent the message is resized", () => {
  const { frames, post } = load();
  post(frames[1].contentWindow, 321.4);
  assert.equal(frames[0].style.height, "150px");
  assert.equal(frames[1].style.height, "322px");
  post({}, 999);
  assert.equal(frames[0].style.height, "150px");
  assert.equal(frames[1].style.height, "322px");
});

test("other messages and heights that are not finite numbers are ignored", () => {
  const { frames, post, onMessage } = load();
  const w = frames[0].contentWindow;
  onMessage({ source: w, data: { height: 500 } });
  onMessage({ source: w, data: null });
  for (const h of ["500", NaN, Infinity, -5, null, undefined, {}]) post(w, h);
  assert.equal(frames[0].style.height, "150px");
});

test("the height is clamped", () => {
  const { frames, post, api } = load();
  post(frames[0].contentWindow, 1e7);
  assert.equal(frames[0].style.height, api.MAX + "px");
});

test("a frame that grows by the same step every round stops following", () => {
  const { frames, post } = load();
  const w = frames[0].contentWindow;
  let h = 200;
  for (let i = 0; i < 50; i++) post(w, (h += 16));
  const stopped = parseInt(frames[0].style.height, 10);
  assert.ok(stopped < 200 + 16 * 12, "stopped after about ten rounds");
  post(w, 5000);
  assert.equal(parseInt(frames[0].style.height, 10), stopped);
});

test("growth in uneven steps and shrinking are followed", () => {
  const { frames, post } = load();
  const w = frames[0].contentWindow;
  let h = 100;
  for (let i = 0; i < 40; i++) post(w, (h += 10 + (i % 5) * 7));
  assert.equal(frames[0].style.height, h + "px");
  post(w, 120);
  assert.equal(frames[0].style.height, "120px");
});

// A frame that waits for a document, as html_page_tags(id =, doc =) draws it.
function waitingFrame(id, doc) {
  const attrs = { "data-doc": doc };
  return {
    id,
    srcdoc: null,
    getAttribute: (k) => (k in attrs ? attrs[k] : null),
    setAttribute: (k, v) => { attrs[k] = v; },
  };
}

function loadWith(byId) {
  const window = { addEventListener: () => {} };
  const document = {
    querySelectorAll: () => [],
    getElementById: (id) => byId[id] || null,
  };
  const src = readFileSync(new URL("../../inst/js/html-page.js", import.meta.url), "utf8");
  vm.runInNewContext(src, { window, document, WeakMap, Math, isFinite, String });
  return window.__blockrHtmlPageTest;
}

test("a document that arrives after its frame is put into it", () => {
  const frame = waitingFrame("f", "3");
  const api = loadWith({ f: frame });
  api.receive({ id: "f", doc: 3, html: "<p>three</p>" });
  assert.equal(frame.srcdoc, "<p>three</p>");
});

test("a frame that arrives after its document takes it", () => {
  const byId = {};
  const api = loadWith(byId);
  api.receive({ id: "f", doc: "4", html: "<p>four</p>" });
  const frame = waitingFrame("f", "4");
  assert.equal(api.fill(frame), true);
  assert.equal(frame.srcdoc, "<p>four</p>");
  // and only once
  frame.srcdoc = "kept";
  assert.equal(api.fill(frame), false);
  assert.equal(frame.srcdoc, "kept");
});

test("a frame does not take the document of another render", () => {
  const old = waitingFrame("f", "5");
  const api = loadWith({ f: old });
  api.receive({ id: "f", doc: "6", html: "<p>six</p>" });
  assert.equal(old.srcdoc, null);
  const next = waitingFrame("f", "6");
  assert.equal(api.fill(next), true);
  assert.equal(next.srcdoc, "<p>six</p>");
});
