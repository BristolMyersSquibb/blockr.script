# Prose block

Text that can compute. The block holds markdown, edited as it reads
(bold as bold, no syntax to learn), and R inside it the way Quarto
writes inline code: `` `r nrow(data)` ``. Typing `` `r `` and a space in
the editor opens a small code field that suggests the inputs, their
columns and a few common functions; Enter computes it, and the text
shows the value, marked with a dotted underline. Pointing at a value
shows its code, a click edits it.

## Usage

``` r
new_prose_block(text = character(), ...)
```

## Arguments

- text:

  Markdown string, with inline R as `` `r expr` ``.

- ...:

  Forwarded to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html).

## Value

A `prose_block` (also a `text_block`).

## Details

The markdown with its code is the block's setting, in its control. Its
result is the markdown with the values written in, which the next block
takes as text (a report, a slide). Because the code is Quarto's own
inline code, the same text runs in a Quarto document where the inputs
are objects of the same names.

An expression that fails marks its value in the editor; in the result it
stops the block with the error.

Values refresh eagerly as you type, outside the DAG. The text itself
commits on blur or Ctrl+Enter, never on a keystroke, so typing does not
recompute the blocks downstream.

## Who can run code

Whoever can edit the text can run R code on the server, as with the
script block. The code runs in a fresh environment whose parent is the
global environment, so `<<-` and `assign(..., envir = globalenv())`
write to a place that all sessions of the app share.

## Inputs

The block takes any number of inputs, including none, named as in
[`new_script_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md):

- The first link into the block is `data`.

- Every further link is read by its link name, or as `.arg2`, `.arg3`,
  ... when the link has none.

- With no input connected the text is plain text, or computes with
  values that need no input.

On export each input the text uses is replaced by the id of the upstream
block.

## Examples

``` r
if (interactive()) {
  library(blockr.core)
  serve(
    new_board(
      blocks = list(
        data = new_dataset_block("iris", "datasets"),
        note = new_prose_block("## Iris\n\nThere are **`r nrow(data)`** rows.")
      ),
      links = links(from = "data", to = "note", input = "data")
    )
  )
}
```
