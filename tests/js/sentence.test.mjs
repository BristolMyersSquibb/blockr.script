import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import vm from "node:vm";

// Load inst/js/sentence.js against a fake page: one band at a time, with one
// word, and a Blockr.Select whose menu() records what it was given.

function makeBand(spec, arg = "show") {
  const marks = {};
  const band = {
    getAttribute: (name) => ({
      "data-sb-slots": JSON.stringify({ [arg]: spec }),
      "data-sb-input": "blk-expr-slot",
    }[name] ?? null),
    querySelector: (sel) => (sel.includes(`data-slot-arg="${arg}"`) && sel.startsWith(".blockr-slot[") ? word : null),
  };
  const word = {
    marks,
    isConnected: true,
    getAttribute: (name) => (name === "data-slot-arg" ? arg : marks[name] ?? null),
    setAttribute: (name, value) => { marks[name] = value; },
    closest: (sel) => (sel === "[data-sb-slots]" ? band : word),
    classList: { toggle: (cls, on) => { marks[cls] = on; } },
    focus: () => {},
  };
  return { band, word };
}

function load(select) {
  const handlers = {};
  const rootClasses = new Set();
  const errors = [];
  const sent = [];
  const page = { bands: [] };
  const menus = [];
  const Blockr = select === null ? undefined : {
    Select: Object.assign({
      menu: (anchor, config) => {
        const menu = { anchor, config, pushed: [], closed: false };
        menu.handle = {
          close: () => { menu.closed = true; },
          setOptions: (opts, sel) => { menu.pushed.push({ opts, sel }); },
        };
        menus.push(menu);
        return menu.handle;
      },
    }, select),
  };
  const console = { error: (msg) => errors.push(msg) };
  const window = { Blockr, console, addEventListener: () => {} };
  const document = {
    readyState: "complete",
    documentElement: {
      classList: { toggle: (cls, on) => { if (on) rootClasses.add(cls); else rootClasses.delete(cls); } },
    },
    addEventListener: (type, fn) => { handlers[type] = fn; },
    querySelectorAll: (sel) => (sel === "[data-sb-slots]" ? page.bands : []),
    contains: (el) => page.bands.some((b) => b.querySelector(
      '.blockr-slot[data-slot-arg="' + el.getAttribute("data-slot-arg") + '"]') === el),
  };
  const Shiny = { setInputValue: (id, value) => sent.push({ id, value }) };
  window.Shiny = Shiny;
  const src = readFileSync(new URL("../../inst/js/sentence.js", import.meta.url), "utf8");
  vm.runInNewContext(src, {
    window, document, Blockr, Shiny, JSON, Date, Array, Object, String, Number,
    console,
  });
  const click = (word) => handlers.click({ target: word, stopPropagation: () => {} });
  return { api: window.__blockrScriptSentenceTest, page, menus, click, rootClasses, errors, sent };
}

const CURRENT = { menuMulti: true, list: () => {} };

test("a blockr.ui without the list in place is reported as too old", () => {
  assert.match(load(null).api.menuProblem(), /menu\(\) is missing/);
  assert.match(load({}).api.menuProblem(), /no multi mode/);

  const old = load({ menuMulti: true });
  assert.match(old.api.menuProblem(), /Blockr\.Select\.list\(\) is missing: blockr\.ui is too old/);
  // said on the page and in the console, as for the other two
  assert.equal(old.rootClasses.has("blockr-script-no-menu"), true);
  assert.equal(old.errors.length, 1);
  // and a list word does not open the old list
  const { band, word } = makeBand({ kind: "multi", options: ["a"], selected: [] });
  old.page.bands = [band];
  assert.throws(() => old.click(word), /too old/);
  assert.equal(old.menus.length, 0);

  const now = load(CURRENT);
  assert.equal(now.api.menuProblem(), null);
  assert.equal(now.rootClasses.has("blockr-script-no-menu"), false);
});

test("a word opens the menu without options the list ignores", () => {
  const { page, menus, click } = load(CURRENT);
  const { band, word } = makeBand({ kind: "multi", options: ["a", "b"], selected: ["a"] });
  page.bands = [band];
  click(word);
  assert.equal(menus.length, 1);
  const config = menus[0].config;
  assert.equal(config.mode, "multi");
  assert.deepEqual(config.selected, ["a"]);
  assert.equal("maxTagChars" in config, false);
  assert.equal(word.marks["aria-expanded"], "true");
});

test("an open multi menu takes the choices of a redrawn word", () => {
  const { page, menus, click } = load(CURRENT);
  const first = makeBand({ kind: "multi", options: ["a", "b", "c"], selected: ["a", "c"] });
  page.bands = [first.band];
  click(first.word);
  const menu = menus[0];

  // R draws the band again: `c` is no longer a choice, but still a pick
  const next = makeBand({
    kind: "multi",
    options: ["a", { value: "b", label: "Bee" }, "d"],
    selected: ["a", "c"],
  });
  page.bands = [next.band];
  first.word.isConnected = false;

  assert.equal(menu.config.reanchor(), next.word);
  assert.equal(menu.pushed.length, 1);
  assert.deepEqual(menu.pushed[0].opts, ["a", { value: "b", label: "Bee" }, "d"]);
  assert.deepEqual(menu.pushed[0].sel, ["a"]);
  // open, and marked on the new word
  assert.equal(menu.closed, false);
  assert.equal(next.word.marks["aria-expanded"], "true");

  // asked again for the same word: nothing is pushed twice
  assert.equal(menu.config.reanchor(), next.word);
  assert.equal(menu.pushed.length, 1);

  // the word is gone: the menu stays where it is and keeps its list
  page.bands = [];
  assert.equal(menu.config.reanchor(), null);
  assert.equal(menu.pushed.length, 1);
});

test("an open single menu drops a pick that is no longer a choice", () => {
  const { page, menus, click, api } = load(CURRENT);
  const first = makeBand({ kind: "select", options: ["a", "b"], selected: "b" }, "by");
  page.bands = [first.band];
  click(first.word);
  const next = makeBand({ kind: "select", options: ["a", "c"], selected: "b" }, "by");
  page.bands = [next.band];
  menus[0].config.reanchor();
  assert.deepEqual(menus[0].pushed, [{ opts: ["a", "c"], sel: null }]);

  // a menu without setOptions (an older build) and a word that is no list
  assert.doesNotThrow(() => api.follow({}, { kind: "multi", options: [] }));
  const pushed = [];
  api.follow({ setOptions: (o, s) => pushed.push([o, s]) }, { kind: "number", value: 1 });
  assert.equal(pushed.length, 0);
});
