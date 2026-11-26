# Changelog

## blockr.script 0.0.0.9000

- A control takes `label_off` in its `#|` annotation: the text of the
  `+` chip the sentence offers while the control is off (a checkbox set
  to `FALSE`, or an empty value), in the chip’s text, tooltip and ARIA
  label. Every word carries an ARIA label.
- [`new_script_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md):
  a block whose state is an R script. A top-level assignment of a plain
  value becomes a control on the block, and the exported code is the
  script with the current values written in. The editor’s “Start from”
  menu offers starter scripts.
- [`new_prose_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_prose_block.md):
  markdown with inline R, as ‘Quarto’ writes it.
- [`html_page()`](https://bristolmyerssquibb.github.io/blockr.script/reference/html_page.md):
  a script result that draws as a sandboxed frame, either from HTML the
  script writes or from a URL. Scripts in the page read the input as
  `blockr.data`.
- Both blocks take any number of inputs. The first one reads as `data`.
- [`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md):
  a generic another package can extend to draw its own result class.
- [`block_result_update()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_update.md):
  a generic for a result class that can change what it has on screen.
  The block calls it when the result drawn last is still on screen and
  the new one has the same class, and then leaves its output as it is,
  so a scroll position or a search text survives a change of a control.
- [`new_script_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md)
  takes `class` and `starters`, so another package can build a block of
  its own on the script block, with its own list under “Start from”. A
  saved board restores such a block through its own constructor.
- [`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md):
  a
  [`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
  method puts tools into the block’s head row, left of the gear, and
  gives the sentence titles for the lines the script does not declare.
- [`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md):
  the title, subtitle and caption now on screen, as text, for a download
  or a caption drawn under a table. `declared` and `templates` say which
  lines the script wrote and how, and `ui = FALSE` leaves out the
  script’s `ui_*` lines.
- [`sentence_data()`](https://bristolmyerssquibb.github.io/blockr.script/reference/sentence_data.md):
  a generic for the data frame the sentence’s data tokens read. A `dm`
  input now gives `{label(col)}` its column labels.
- [`script_expr()`](https://bristolmyerssquibb.github.io/blockr.script/reference/script_expr.md):
  a script as one expression with the control values written in, for a
  place that has no block, such as a report.
- While a script runs for longer than about 300 ms, the result dims and
  a line above it counts the wait (“computing… 1.2 s”), as blockr.viz’s
  outputs do.
- A result no longer goes pale and comes back when its dock tab is shown
  again. Shiny’s fade of a recalculating output is off on the result;
  the busy cue above is what shows an evaluation.
- A `NULL` written in a script is kept: `x <- NULL` no longer fails with
  “incorrect number of arguments”, and `function(a = NULL) NULL` keeps
  its formal and its body.
- On a dock card the rule between the head row and the result is gone
  while the script tray is closed.
- A list word of the sentence whose menu is open follows the word when R
  draws it again: the open list takes the new choices and drops a pick
  that is no longer among them.
- The select controls need a ‘blockr.ui’ whose `Blockr.Select` has
  [`list()`](https://rdrr.io/r/base/list.html), where a multi-select
  shows its picks as text and in the list. An older build is reported on
  the page and in the browser console.
