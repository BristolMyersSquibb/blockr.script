// The markdown side of a reference to a figure or a table: Quarto's own
// `@fig-label` and `@tbl-label`. The remark plugin reads them out of the text
// into their own node and writes them back as they were.

const XREF = /(?<![A-Za-z0-9_.])@((?:fig|tbl)-[A-Za-z0-9_](?:[A-Za-z0-9_.-]*[A-Za-z0-9_])?)/g;

function split(text) {
  const out = [];
  let last = 0, m;
  XREF.lastIndex = 0;
  while ((m = XREF.exec(text))) {
    if (m.index > last) out.push({ type: "text", value: text.slice(last, m.index) });
    out.push({ type: "xref", value: m[1] });
    last = m.index + m[0].length;
  }
  if (!out.length) return null;
  if (last < text.length) out.push({ type: "text", value: text.slice(last) });
  return out;
}

function transform(parent) {
  if (!parent || !Array.isArray(parent.children)) return;
  const kids = [];
  parent.children.forEach((child) => {
    const parts = child.type === "text" && typeof child.value === "string" ? split(child.value) : null;
    if (parts) kids.push(...parts);
    else {
      if (child.type !== "inlineCode" && child.type !== "code") transform(child);
      kids.push(child);
    }
  });
  parent.children = kids;
}

export function remarkXref() {
  const data = this.data();
  const ext = data.toMarkdownExtensions || (data.toMarkdownExtensions = []);
  ext.push({ handlers: { xref: (node) => "@" + (node.value || "") } });
  return (tree) => {
    transform(tree);
    return tree;
  };
}
