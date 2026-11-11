import { test } from "node:test";
import assert from "node:assert/strict";
import { columnInsert, isSyntactic, linkHref, isPastedLink } from "../../tools/srcjs/prose-block/util.js";

test("a plain column completes the word after the dollar", () => {
  assert.deepEqual(columnInsert("mpg", "m"), { token: "m", insert: "mpg" });
  assert.deepEqual(columnInsert("Sepal.Length", ""), { token: "", insert: "Sepal.Length" });
});

test("any other column is read with [[ ]], without a backtick", () => {
  assert.deepEqual(columnInsert("my col", "my"), { token: "$my", insert: '[["my col"]]' });
  assert.deepEqual(columnInsert('a"b', ""), { token: "$", insert: '[["a\\"b"]]' });
  assert.deepEqual(columnInsert("if", "i"), { token: "$i", insert: '[["if"]]' });
  assert.deepEqual(columnInsert("1st", ""), { token: "$", insert: '[["1st"]]' });
  assert.equal(columnInsert("a`b", ""), null);
  assert.ok(!isSyntactic("TRUE") && !isSyntactic(".1a") && isSyntactic(".a1"));
});

test("a link keeps http, https and mailto, and takes no other scheme", () => {
  assert.equal(linkHref("https://x.org/a"), "https://x.org/a");
  assert.equal(linkHref("HTTP://x.org"), "HTTP://x.org");
  assert.equal(linkHref("mailto:a@x.org"), "mailto:a@x.org");
  assert.equal(linkHref(" x.org/a "), "https://x.org/a");
  assert.equal(linkHref("localhost:3000/a"), "https://localhost:3000/a");
  assert.equal(linkHref("javascript:alert(1)"), null);
  assert.equal(linkHref("JavaScript:alert(1)"), null);
  assert.equal(linkHref("data:text/html,x"), null);
  assert.equal(linkHref("file:///etc/passwd"), null);
  assert.equal(linkHref(""), null);
});

test("only a web or mail address is pasted as a link", () => {
  assert.ok(isPastedLink("https://x.org"));
  assert.ok(isPastedLink("mailto:a@x.org"));
  assert.ok(!isPastedLink("javascript:alert(1)"));
  assert.ok(!isPastedLink("two words"));
});
