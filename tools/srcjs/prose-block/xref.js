// A reference to a figure or a table in the prose block: an atom that keeps
// Quarto's label (`fig-mdl`) and shows what the host calls it ("Figure 2").

import { $node, $remark } from "@milkdown/kit/utils";
import { remarkXref } from "./xref-md.js";

export const xrefRemark = $remark("xref", () => remarkXref);

export const xrefNode = $node("xref", () => ({
  group: "inline",
  inline: true,
  atom: true,
  selectable: true,
  draggable: false,
  attrs: { key: { default: "" } },
  parseDOM: [
    { tag: "span[data-xref]", getAttrs: (dom) => ({ key: dom.getAttribute("data-xref") || "" }) },
  ],
  toDOM: (node) => [
    "span",
    { "data-xref": node.attrs.key || "", class: "blockr-xref" },
    "@" + (node.attrs.key || ""),
  ],
  parseMarkdown: {
    match: (node) => node.type === "xref",
    runner: (state, node, type) => {
      state.addNode(type, { key: node.value || "" });
    },
  },
  toMarkdown: {
    match: (node) => node.type.name === "xref",
    runner: (state, node) => {
      state.addNode("xref", undefined, node.attrs.key || "");
    },
  },
}));
