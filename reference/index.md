# Package index

## Blocks

A block whose state is an R script, and one whose state is markdown.

- [`new_script_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md)
  : Script block
- [`new_prose_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_prose_block.md)
  : Prose block

## Results

What a script can return beyond data frames, plots and tables.

- [`html_page()`](https://bristolmyerssquibb.github.io/blockr.script/reference/html_page.md)
  : A web page as a block result
- [`inline_r()`](https://bristolmyerssquibb.github.io/blockr.script/reference/inline_r.md)
  : Inline R in markdown

## Extending

Draw another package’s result class, and build a block of your own on
the script block.

- [`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
  : Render a block result, dispatching on the result's class
- [`block_result_update()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_update.md)
  : Update a block result that is on screen
- [`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md)
  : Fill the head row of a script block from a result method
- [`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md)
  : The sentence of a script block as text
- [`sentence_data()`](https://bristolmyerssquibb.github.io/blockr.script/reference/sentence_data.md)
  : The data frame the sentence reads
- [`script_expr()`](https://bristolmyerssquibb.github.io/blockr.script/reference/script_expr.md)
  : A script as one expression
- [`register_script_blocks()`](https://bristolmyerssquibb.github.io/blockr.script/reference/register_script_blocks.md)
  : Register the script and prose blocks
