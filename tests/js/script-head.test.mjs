import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

// Load inst/js/script-head.js against a fake page with one block: its head
// container and its result output. Timers are collected and run by hand.
function load() {
  const classes = new Set();
  const attrs = {};
  const head = {
    getAttribute: (name) => (name === "data-sb-block" ? "blk-" : null),
    classList: {
      toggle: (cls, on) => { if (on) classes.add(cls); else classes.delete(cls); },
      remove: (cls) => classes.delete(cls),
    },
  };
  const out = {
    setAttribute: (name, value) => { attrs[name] = value; },
    removeAttribute: (name) => { delete attrs[name]; },
  };
  const timers = new Map();
  let nextTimer = 0;
  const window = {};
  const document = {
    querySelectorAll: (sel) => (sel.startsWith(".blockr-script-container[") ? [head] : []),
    getElementById: (id) => (id === "blk-result" ? out : null),
  };
  const src = readFileSync(new URL("../../inst/js/script-head.js", import.meta.url), "utf8");
  vm.runInNewContext(src, {
    window, document, Object, String,
    setTimeout: (fn, ms) => { timers.set(++nextTimer, { fn, ms }); return nextTimer; },
    clearTimeout: (id) => timers.delete(id),
  });
  const runTimers = () => {
    for (const [id, t] of [...timers]) { timers.delete(id); t.fn(); }
  };
  return { api: window.__blockrScriptHeadTest, classes, attrs, timers, runTimers };
}

const BUSY = "blockr-script-container--busy";
const EMPTY = "blockr-script-container--no-result";

test("the cue shows only after the delay", () => {
  const { api, classes, attrs, timers, runTimers } = load();
  api.busy({ block: "blk-", busy: true });
  assert.equal(classes.has(BUSY), false);
  assert.equal(timers.size, 1);
  assert.equal([...timers.values()][0].ms, api.DELAY);
  runTimers();
  assert.equal(classes.has(BUSY), true);
  assert.equal(attrs["aria-busy"], "true");
  api.busy({ block: "blk-", busy: false });
  assert.equal(classes.has(BUSY), false);
  assert.equal("aria-busy" in attrs, false);
});

test("a fast evaluation never shows the cue", () => {
  const { api, classes, timers, runTimers } = load();
  api.busy({ block: "blk-", busy: true });
  api.busy({ block: "blk-", busy: false });
  assert.equal(timers.size, 0);
  runTimers();
  assert.equal(classes.has(BUSY), false);
});

test("a second start keeps one timer, and another block is left alone", () => {
  const { api, classes, timers, runTimers } = load();
  api.busy({ block: "blk-", busy: true });
  api.busy({ block: "blk-", busy: true });
  assert.equal(timers.size, 1);
  api.busy({ block: "other-", busy: true });
  runTimers();
  assert.equal(classes.has(BUSY), true);
  api.busy({ block: "other-", busy: false });
  assert.equal(classes.has(BUSY), true);
  api.busy(null);
  api.busy({ busy: true });
});

test("the tools are hidden while the result output is empty", () => {
  const { api, classes } = load();
  assert.equal(api.isEmpty(null), true);
  assert.equal(api.isEmpty(""), true);
  assert.equal(api.isEmpty({ html: "  ", deps: [] }), true);
  assert.equal(api.isEmpty({ html: "<p>x</p>", deps: [] }), false);
  assert.equal(api.isEmpty("text"), false);

  api.resultChanged("blk-result", true);
  assert.equal(classes.has(EMPTY), true);
  api.resultChanged("blk-result", false);
  assert.equal(classes.has(EMPTY), false);
  // an output of the same block that is not its result
  api.resultChanged("blk-expr-sentence", true);
  assert.equal(classes.has(EMPTY), false);
  api.resultChanged(undefined, true);
});
