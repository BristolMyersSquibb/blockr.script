import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

// Load inst/js/script-head.js against a fake page with one block: its head
// container and its result output, in a parent that takes the line counting
// the wait. Timers are collected and run by hand, and the clock is set by hand.
function fakeClasses() {
  const set = new Set();
  return {
    set,
    list: {
      toggle: (cls, on) => { if (on) set.add(cls); else set.delete(cls); },
      remove: (cls) => set.delete(cls),
      contains: (cls) => set.has(cls),
    },
  };
}

function load() {
  const head = fakeClasses();
  const classes = head.set;
  const attrs = {};
  const container = {
    getAttribute: (name) => (name === "data-sb-block" ? "blk-" : null),
    classList: head.list,
  };
  const outClasses = fakeClasses();
  const parent = { children: [] };
  const out = {
    parentNode: parent,
    classList: outClasses.list,
    setAttribute: (name, value) => { attrs[name] = value; },
    removeAttribute: (name) => { delete attrs[name]; },
    get previousElementSibling() {
      const i = parent.children.indexOf(out);
      return i > 0 ? parent.children[i - 1] : null;
    },
  };
  parent.children.push(out);
  parent.insertBefore = (el, before) => {
    parent.children.splice(parent.children.indexOf(before), 0, el);
  };
  const timers = new Map();
  let nextTimer = 0;
  const clock = { now: 1000 };
  const window = {};
  const document = {
    querySelectorAll: (sel) => (sel.startsWith(".blockr-script-container[") ? [container] : []),
    getElementById: (id) => (id === "blk-result" ? out : null),
    createElement: () => {
      const c = fakeClasses();
      const el = { classList: c.list, attrs: {}, textContent: "" };
      el.setAttribute = (name, value) => { el.attrs[name] = value; };
      Object.defineProperty(el, "className", {
        set: (v) => { c.set.clear(); v.split(" ").forEach((x) => c.set.add(x)); },
      });
      return el;
    },
  };
  const add = (fn, ms, every) => { timers.set(++nextTimer, { fn, ms, every }); return nextTimer; };
  const src = readFileSync(new URL("../../inst/js/script-head.js", import.meta.url), "utf8");
  vm.runInNewContext(src, {
    window, document, Object, String,
    Date: { now: () => clock.now },
    setTimeout: (fn, ms) => add(fn, ms, false),
    clearTimeout: (id) => timers.delete(id),
    setInterval: (fn, ms) => add(fn, ms, true),
    clearInterval: (id) => timers.delete(id),
  });
  // Runs the timers due now once; an interval stays.
  const runTimers = () => {
    for (const [id, t] of [...timers]) { if (!t.every) timers.delete(id); t.fn(); }
  };
  const line = () => (parent.children.length > 1 ? parent.children[0] : null);
  return {
    api: window.__blockrScriptHeadTest, classes, attrs, timers, runTimers,
    outClasses: outClasses.set, line, clock,
  };
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

test("a line above the result counts the wait, and the result dims", () => {
  const { api, timers, runTimers, line, outClasses, clock } = load();
  api.busy({ block: "blk-", busy: true });
  assert.equal(line(), null);
  clock.now += 400;
  runTimers();
  assert.equal(line().textContent, "computing\u2026 0.4 s");
  assert.equal(line().classList.contains("blockr-script-busy-line--on"), true);
  assert.equal(outClasses.has("blockr-script-result--busy"), true);
  // the clock ticks on
  const ticks = [...timers.values()].filter((t) => t.every);
  assert.equal(ticks.length, 1);
  assert.equal(ticks[0].ms, api.TICK);
  clock.now += 1300;
  runTimers();
  assert.equal(line().textContent, "computing\u2026 1.7 s");
  // the end stops the clock, closes the line and undims the result
  api.busy({ block: "blk-", busy: false });
  assert.equal(timers.size, 0);
  assert.equal(line().classList.contains("blockr-script-busy-line--on"), false);
  assert.equal(outClasses.has("blockr-script-result--busy"), false);
  // a second wait reuses the line
  api.busy({ block: "blk-", busy: true });
  runTimers();
  assert.equal(line().classList.contains("blockr-script-busy-line--on"), true);
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
