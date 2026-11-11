// Small rules of the prose editor, kept free of the editor so they can be
// tested in node.

const SYNTACTIC = /^[A-Za-z.][A-Za-z0-9_.]*$/;
const RESERVED = new Set([
  "if", "else", "repeat", "while", "function", "for", "next", "break",
  "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
  "NA_character_", "NA_complex_", "in",
]);

// Can `name` follow a `$` as it is?
export function isSyntactic(name) {
  return SYNTACTIC.test(name) && !RESERVED.has(name) && !/^\.[0-9]/.test(name);
}

// What picking a column writes after `data$<typed>`: `token` is the text it
// replaces and `insert` what goes there. A plain name completes the word.
// Any other name cannot be written with backticks, because a backtick ends
// the inline code in the stored markdown, so the `$` goes too and the column
// is read as data[["my col"]]. A name that holds a backtick cannot be
// written at all and gives null.
export function columnInsert(col, typed) {
  col = String(col);
  if (isSyntactic(col)) return { token: typed, insert: col };
  if (col.indexOf("`") !== -1) return null;
  return { token: "$" + typed, insert: "[[" + JSON.stringify(col) + "]]" };
}

// The address a link gets from what was typed or pasted: http, https and
// mailto as they are, a bare host with https:// in front, and null for any
// other scheme (javascript:, data:, ...). The link ends up in the markdown
// that the next block renders.
export function linkHref(text) {
  const href = String(text || "").trim();
  if (!href) return null;
  if (/^(https?:\/\/|mailto:)/i.test(href)) return href;
  // "host:8080/path" is a host with a port, not a scheme
  if (/^[a-z][a-z0-9+.-]*:(?!\d)/i.test(href)) return null;
  return "https://" + href;
}

// Is this pasted text a link to put on the selected words?
export function isPastedLink(text) {
  return /^(https?:\/\/|mailto:)\S+$/i.test(String(text || "").trim());
}
