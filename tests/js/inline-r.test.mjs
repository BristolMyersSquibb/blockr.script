import { test } from "node:test";
import assert from "node:assert/strict";
import { collectExprs, inlineCodes, proseLines, remarkInlineR } from "../../tools/srcjs/prose-block/inline-r-md.js";

test("collectExprs finds inline R once each, outside fenced code", () => {
  const md = "Rows `r nrow(data)`, again `r nrow(data)`, mean `r mean(data$x)`.\n\n```\n`r nope`\n```\n\nCode `x` stays.";
  assert.deepEqual(collectExprs(md), ["nrow(data)", "mean(data$x)"]);
});

test("the remark plugin reads `r expr` into a node and writes it back", () => {
  const data = {};
  const proc = { data: () => data };
  const run = remarkInlineR.call(proc);
  const tree = { type: "root", children: [{ type: "paragraph", children: [
    { type: "text", value: "n = " },
    { type: "inlineCode", value: "r nrow(data)" },
    { type: "inlineCode", value: "plain" }
  ] }] };
  run(tree);
  const kids = tree.children[0].children;
  assert.deepEqual(kids[1], { type: "inlineR", value: "nrow(data)" });
  assert.equal(kids[2].type, "inlineCode");
  const handler = data.toMarkdownExtensions[0].handlers.inlineR;
  assert.equal(handler({ value: "nrow(data)" }), "`r nrow(data)`");
});

test("code between double backticks is shown, not sent", () => {
  assert.deepEqual(collectExprs("show `` `r nrow(data)` `` and `r 1 + 1`"), ["1 + 1"]);
  assert.deepEqual(inlineCodes("``r a`` ```r b``` `r c`"), ["c"]);
  // markdown pairs a lone backtick with the next one
  assert.deepEqual(inlineCodes("a ` lone and `r x`"), []);
  assert.deepEqual(inlineCodes("`plain` `python x` `rnorm(1)` `r `"), []);
});

test("a fence closes only on its own marker", () => {
  const four = "````\n```\n`r inner`\n```\n`r still`\n````\n`r out`";
  assert.deepEqual(collectExprs(four), ["out"]);
  assert.deepEqual(collectExprs("```\n~~~\n`r inside`\n```\n`r out`"), ["out"]);
  assert.deepEqual(collectExprs("```r\n```r\n`r inside`\n```\n`r out`"), ["out"]);
  assert.deepEqual(collectExprs("~~~\n`r never`"), []);
  assert.deepEqual(proseLines("a\n```\nb\n```\nc"), ["a", "c"]);
});
