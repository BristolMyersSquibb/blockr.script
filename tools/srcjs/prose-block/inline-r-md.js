// The markdown side of inline R, kept free of the editor so it can be tested
// in node: finding the expressions, and the remark plugin that reads
// `r expr` inline code into its own node and writes it back.

const R_CODE = /^r[ \t]+(\S[\s\S]*)$/;

// The lines of a markdown string that are outside fenced code. A fence closes
// on a line of the same character that is at least as long as the opening
// one and has nothing after it, so a longer fence can hold a shorter one and
// a backtick fence can hold a line of tildes. R/prose-block.R has the same
// rule (inline_r_lines).
export function proseLines(md) {
  const out = [];
  let open = "";
  for (const line of String(md || "").split("\n")) {
    const m = /^\s*(`{3,}|~{3,})(.*)$/.exec(line);
    if (!open) {
      if (m) open = m[1];
      else out.push(line);
    } else if (m && m[1][0] === open[0] && m[1].length >= open.length && !m[2].trim()) {
      open = "";
    }
  }
  return out;
}

// The code of each `r expr` on one line. Code spans are read as markdown
// reads them: a run of n backticks opens a span that the next run of n
// backticks closes. Only a span between single backticks whose content
// starts with "r " is inline R, so `` `r x` `` (the way to show the syntax)
// is left alone. R/prose-block.R has the same rule (inline_r_spans).
export function inlineCodes(line) {
  const runs = [];
  const re = /`+/g;
  let m;
  while ((m = re.exec(line))) runs.push({ at: m.index, len: m[0].length });
  const out = [];
  let i = 0;
  while (i < runs.length - 1) {
    let j = -1;
    for (let k = i + 1; k < runs.length; k++) {
      if (runs[k].len === runs[i].len) { j = k; break; }
    }
    if (j < 0) { i += 1; continue; }
    const inner = line.slice(runs[i].at + runs[i].len, runs[j].at);
    const code = /^r[ \t]+(\S[\s\S]*)$/.exec(inner);
    if (runs[i].len === 1 && code) out.push(code[1]);
    i = j + 1;
  }
  return out;
}

// The inline expressions of a markdown string, in order and without repeats,
// for the value preview.
export function collectExprs(md) {
  const out = [];
  for (const line of proseLines(md)) {
    for (const code of inlineCodes(line)) {
      const e = code.trim();
      if (e && out.indexOf(e) === -1) out.push(e);
    }
  }
  return out;
}

function transform(parent) {
  if (!parent || !Array.isArray(parent.children)) return;
  parent.children = parent.children.map((child) => {
    if (child.type === "inlineCode" && typeof child.value === "string") {
      const m = R_CODE.exec(child.value);
      if (m) return { type: "inlineR", value: m[1].trim() };
    }
    transform(child);
    return child;
  });
}

// A unified/remark plugin: parse inline R into its own node, write it back as
// inline code.
export function remarkInlineR() {
  const data = this.data();
  const ext = data.toMarkdownExtensions || (data.toMarkdownExtensions = []);
  ext.push({ handlers: { inlineR: (node) => "`r " + (node.value || "") + "`" } });
  return (tree) => {
    transform(tree);
    return tree;
  };
}
