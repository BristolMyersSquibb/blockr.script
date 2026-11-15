# Update a block result that is on screen

[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
draws a result from scratch: the function it returns replaces the
block's output, and the browser builds the result again. A table then
loses its scroll position, its search text and its collapsed rows on
every change of a control. A class that can change what it has drawn
registers a method here. The method sends the new result to the browser
itself, for example with `session$sendCustomMessage()` or with the
update function of the widget it drew, and returns `TRUE`. The block
then leaves its output as it is.

## Usage

``` r
block_result_update(result, block, session, ...)

# Default S3 method
block_result_update(result, block, session, ...)
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

`TRUE` when the result on screen was updated. Anything else has the
block draw the result from scratch.

## When it is called

The block calls the method when the result it drew last is still on
screen and the new result has the same class vector. In every other case
it calls
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md):

- the first result of a block, also of a block that was removed and
  added again under the same id;

- a result of another class;

- the first result after an error or after an input was unavailable;

- a result whose predecessor has not reached the browser: a result
  evaluated twice while the block was hidden, or a second evaluation in
  the flush that drew the first;

- after a method here returned anything but `TRUE`, or failed.

So a method can return `FALSE` for a change it cannot apply in place,
such as a table with other columns, and the result is drawn from
scratch.

A class without a method is drawn as before: the function its
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
method returns is the block's output, unchanged.

## What the method has to do

The output keeps the HTML the first render sent, and the render function
of that render does not run again. So everything that follows the result
has to be sent by this method: the data, and a caption the result draws
itself. Use ids that are the same in every call, such as
`session$ns("table")`.

[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md)
and
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md)
work as in
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md).
The head row starts empty before the call, so call
[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md)
with the same arguments as in the first render.
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md),
called in the method, gives the sentence as it is now, and the block
calls the method again when the sentence changes.

## What the method can rely on

When the method is called, the HTML that
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
drew for the last result was sent to the browser in an earlier flush. A
message the method sends therefore arrives after that HTML, and after
every message an earlier call sent.

It cannot rely on more than that. The block does not know whether the
browser has the element in the page at this moment: a layout can take a
hidden card out of the document. A message for an element that is not
there is lost unless the widget keeps it, so a widget that can be hidden
this way should apply the last message when it is attached again. If the
browser attaches the output element again without a new evaluation, it
draws the HTML of the first render.

The block also knows only the class of what it drew last, not which
method drew it. A method that sometimes hands the result on with
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) has to note that
itself, and return `FALSE` here while the drawing in the page is not its
own.

## See also

[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# A table widget with an update function of its own.
block_result_output.my_table <- function(result, block, session, ...) {
  id <- session$ns("table")
  shiny::renderUI(my_table_widget(result$data, id = id))
}

block_result_update.my_table <- function(result, block, session, ...) {
  update_my_table_widget(session, session$ns("table"), result$data)
  TRUE
}
} # }
```
