// Inline R in the prose block: `r expr`, Quarto's own inline code.
//
// In markdown it is inline code whose text starts with "r ". The remark plugin
// turns such inline code into an `inlineR` node on the way in and writes it
// back as `r expr` on the way out, so the stored markdown is exactly what a
// Quarto document would hold. In the editor it is an atomic inline node, the
// chip: it shows the value (the controller patches it in when the server
// sends values) and keeps the code in its `expr` attribute.

import { $node, $remark, $prose } from "@milkdown/kit/utils";
import { Plugin, PluginKey } from "@milkdown/kit/prose/state";
import { remarkInlineR } from "./inline-r-md.js";

export { collectExprs } from "./inline-r-md.js";

export const inlineRRemark = $remark("inlineR", () => remarkInlineR);

export const inlineRNode = $node("inline_r", () => ({
  group: "inline",
  inline: true,
  atom: true,
  selectable: true,
  draggable: false,
  attrs: { expr: { default: "" } },
  parseDOM: [
    {
      tag: "span[data-r]",
      getAttrs: (dom) => ({ expr: dom.getAttribute("data-r") || "" }),
    },
  ],
  // The chip shows its code until a value arrives (dormant).
  toDOM: (node) => [
    "span",
    {
      "data-r": node.attrs.expr || "",
      "data-blockr-tooltip": "r " + (node.attrs.expr || ""),
      class: "blockr-r-chip is-dormant",
    },
    node.attrs.expr || "",
  ],
  parseMarkdown: {
    match: (node) => node.type === "inlineR",
    runner: (state, node, type) => {
      state.addNode(type, { expr: node.value || "" });
    },
  },
  toMarkdown: {
    match: (node) => node.type.name === "inline_r",
    runner: (state, node) => {
      state.addNode("inlineR", undefined, node.attrs.expr || "");
    },
  },
}));

// A click on a chip opens its code; typing `r and a space opens a new one.
const key = new PluginKey("BLOCKR_INLINE_R");

export function inlineRPlugin(onOpen) {
  return $prose(
    () =>
      new Plugin({
        key,
        props: {
          handleClickOn: (view, pos, node, nodePos) => {
            if (node.type.name === "inline_r") {
              onOpen(view, nodePos);
              return true;
            }
            return false;
          },
          handleTextInput: (view, from, to, text) => {
            if (text !== " ") return false;
            const $from = view.state.doc.resolve(from);
            const before = $from.parent.textBetween(
              Math.max(0, $from.parentOffset - 2), $from.parentOffset, null, "￼");
            if (before !== "`r") return false;
            const type = view.state.schema.nodes.inline_r;
            const tr = view.state.tr.replaceWith(from - 2, to, type.create({ expr: "" }));
            view.dispatch(tr);
            onOpen(view, from - 2, true);
            return true;
          },
        },
      })
  );
}
