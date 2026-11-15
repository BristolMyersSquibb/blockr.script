# Render a block result, dispatching on the result's class

[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.html)
dispatches on the BLOCK, which is the right hinge for a block that
always draws the same kind of thing. A function or code block draws
whatever its code returned, so the thing worth dispatching on is the
RESULT: a data frame, a gt table, a ggplot, a package's own table class.

## Usage

``` r
# S3 method for class 'html_page'
block_result_output(result, block, session, ...)

block_result_output(result, block, session, ...)

# Default S3 method
block_result_output(result, block, session, ...)
```

## Arguments

- result:

  The block's evaluated result.

- block:

  The block object.

- session:

  The block's Shiny module session.

- ...:

  Passed to methods.

## Value

A shiny render function, as
[`blockr.core::block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.html)
requires.

## Details

The default method is `render_dynamic_output()`, the type switch this
package has always used, so adding this generic changes nothing on its
own. It exists so another package can teach a function block to draw its
own object without that object's renderer having to live here – and,
unlike an
[`htmltools::as.tags()`](https://rstudio.github.io/htmltools/reference/as.tags.html)
method, a method here receives the block's module `session`. That is
what a renderer needs to namespace an element id, register a
`downloadHandler` on `session$output`, or observe a click on
`session$input`.

For example, a package with its own table class can register a method
that draws the table through blockr.viz's renderer rather than through
gt.

## Called on every re-evaluation

blockr.core re-runs
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.html)
whenever the block's result changes (`output_render_observer()`).
Assigning to `session$output` replaces the output and can be done on
every call. An observer must be created once per block and not once per
call, so a method keeps a note that it has made them.

That note, and any state the method keeps with it, has to end with the
block. blockr.core destroys a block's session when the block is removed
(`session$destroy()`). The observers, outputs and reactive values made
in it are then gone, and Shiny refuses every later read or write of
them. `session$userData` is shared by the whole Shiny session and is not
cleared, so a note kept there is still found when a block is added again
under the same id. Remove it in `session$onDestroy()`:

    key <- paste0("my_table:", session$ns(""))
    if (is.null(session$userData[[key]])) {
      session$userData[[key]] <- TRUE
      session$onDestroy(function() session$userData[[key]] <- NULL)
      shiny::observeEvent(session$input$row, my_click(session$input$row))
    }

## Updating in place

The function a method returns replaces the block's output, so the
browser draws the result again on every evaluation and what the user did
to it (a scroll position, a search text) is lost. A result class that
can change what it has on screen also registers a
[`block_result_update()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_update.md)
method.

## The head row

A method can put its own tools, such as a download button, into the
block's head row and give the block's sentence a title with
[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md).
It reads the title, subtitle and caption now on screen with
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md).

## See also

[`block_result_update()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_update.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a package with its own table class `my_table`:
block_result_output.my_table <- function(result, block, session, ...) {
  shiny::renderUI(draw_my_table(result))
}
} # }
```
