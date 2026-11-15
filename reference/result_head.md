# Fill the head row of a script block from a result method

A script block has one head row: the sentence on the left, the gear on
the right. A
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
method calls `result_head()` to put its own tools into that row, left of
the gear, and to give the sentence titles for the lines the script does
not declare.

## Usage

``` r
result_head(session, tools = NULL, titles = NULL, caption = TRUE)
```

## Arguments

- session:

  The block's session, as the method received it.

- tools:

  Tags for the head row, or `NULL` for none.

- titles:

  A list with any of `title`, `subtitle` and `caption`, each one plain
  string. The sentence shows them for the lines the script does not
  declare. A line the script declares wins, and a script line with
  nothing after the colon (`#| title:`) switches that line off. The
  strings are shown as they are; `{...}` is not read in them.

- caption:

  `FALSE` when the result draws the caption itself.

## Value

`NULL`, invisibly.

## Details

The head row starts empty before every render. A method that does not
call `result_head()` leaves the row as the script block draws it, so the
tools of one result class are gone as soon as the script returns
another. The method is called on every re-evaluation; calling
`result_head()` again with the same arguments redraws nothing. The tools
are hidden while the block has no result on screen, for example after an
error.

## Tools

`tools` is drawn as it is. A direct child that is a link or a button
gets the size and look of an icon button (26 px square), so give it an
icon as its content and a `title` as its name. Ids are namespaced with
`session$ns()` and their outputs and inputs live on `session`, as
anywhere in the method. Assigning a `downloadHandler` to
`session$output` on every call is fine. An observer on `session$input`
must be created once per block and not once per call, see
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md).

## Caption

The script block draws a `#| caption:` line in the head row, under the
subtitle. A result that draws the caption itself, such as a table with
the caption under it, passes `caption = FALSE` and reads the text from
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md).
The head row then leaves the caption out, so there is one copy on
screen. A control that only the caption names stays in the grid of
controls, because the head row has no word for it.

## See also

[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md)
for the sentence as text,
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
for the method this is called from.

## Examples

``` r
if (FALSE) { # \dontrun{
block_result_output.my_table <- function(result, block, session, ...) {
  session$output$csv <- shiny::downloadHandler(
    filename = "table.csv",
    content = function(file) utils::write.csv(result$data, file)
  )
  result_head(
    session,
    tools = shiny::downloadLink(
      session$ns("csv"), label = shiny::icon("download"),
      title = "Download"
    ),
    titles = list(title = "My table"),
    caption = FALSE
  )
  shiny::renderUI(
    shiny::tagList(
      draw_my_table(result),
      shiny::tags$p(result_titles(session)$caption)
    )
  )
}
} # }
```
