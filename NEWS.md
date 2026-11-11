# blockr.script 0.0.0.9000

- `new_script_block()`: a block whose state is an R script. A top-level
  assignment of a plain value becomes a control on the block, and the
  exported code is the script with the current values written in. The
  editor's "Start from" menu offers starter scripts.
- `new_prose_block()`: markdown with inline R, as 'Quarto' writes it.
- `html_page()`: a script result that draws as a sandboxed frame, either
  from a mustache template filled from the input or from a URL.
- Both blocks take any number of inputs. The first one reads as `data`.
- `block_result_output()`: a generic another package can extend to draw its
  own result class.
