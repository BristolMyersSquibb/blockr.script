import test from "node:test";
import assert from "node:assert/strict";
import { unified } from "unified";
import remarkParse from "remark-parse";
import remarkStringify from "remark-stringify";
import { remarkXref } from "../../tools/srcjs/prose-block/xref-md.js";

const run = (md) => unified().use(remarkParse).use(remarkXref).use(remarkStringify).processSync(md).toString().trim();

test("a reference reads in and writes back as it was", () => {
  assert.equal(run("As @fig-mdl shows, and @tbl-desc."), "As @fig-mdl shows, and @tbl-desc.");
});

test("a reference is its own node, the sentence end stays text", () => {
  const tree = unified().use(remarkParse).use(remarkXref).runSync(unified().use(remarkParse).parse("See @fig-a_1."));
  const kids = tree.children[0].children;
  assert.deepEqual(kids.map((k) => k.type), ["text", "xref", "text"]);
  assert.equal(kids[1].value, "fig-a_1");
  assert.equal(kids[2].value, ".");
});

test("an email or code is left alone", () => {
  const tree = unified().use(remarkParse).use(remarkXref).runSync(unified().use(remarkParse).parse("mail@fig-x.org"));
  assert.ok(!JSON.stringify(tree).includes('"xref"'));
  assert.equal(run("`@fig-x`"), "`@fig-x`");
});
