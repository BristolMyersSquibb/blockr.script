#' Render a block result, dispatching on the result's class
#'
#' `block_output()` dispatches on the BLOCK, which is the right hinge for a
#' block that always draws the same kind of thing. A function or code block
#' draws whatever its code returned, so the thing worth dispatching on is the
#' RESULT: a data frame, a gt table, a ggplot, a package's own table class.
#'
#' The default method is `render_dynamic_output()`, the type switch this
#' package has always used, so adding this generic changes nothing on its
#' own. It exists so another package can teach a function block to draw its
#' own object without that object's renderer having to live here -- and,
#' unlike an `htmltools::as.tags()` method, a method here receives the block's
#' module `session`. That is what a renderer needs to namespace an element id,
#' register a `downloadHandler` on `session$output`, or observe a click on
#' `session$input`.
#'
#' For example, a package with its own table class can register a method that
#' draws the table through blockr.viz's renderer rather than through gt.
#'
#' @section Called on every re-evaluation:
#' blockr.core re-runs `block_output()` whenever the block's result changes
#' (`output_render_observer()`). Assigning to `session$output` replaces the
#' output and can be done on every call. An observer must be created once per
#' block and not once per call, so a method keeps a note that it has made
#' them.
#'
#' That note, and any state the method keeps with it, has to end with the
#' block. blockr.core destroys a block's session when the block is removed
#' (`session$destroy()`). The observers, outputs and reactive values made in
#' it are then gone, and Shiny refuses every later read or write of them.
#' `session$userData` is shared by the whole Shiny session and is not
#' cleared, so a note kept there is still found when a block is added again
#' under the same id. Remove it in `session$onDestroy()`:
#'
#' ```r
#' key <- paste0("my_table:", session$ns(""))
#' if (is.null(session$userData[[key]])) {
#'   session$userData[[key]] <- TRUE
#'   session$onDestroy(function() session$userData[[key]] <- NULL)
#'   shiny::observeEvent(session$input$row, my_click(session$input$row))
#' }
#' ```
#'
#' @section Updating in place:
#' The function a method returns replaces the block's output, so the browser
#' draws the result again on every evaluation and what the user did to it
#' (a scroll position, a search text) is lost. A result class that can
#' change what it has on screen also registers a [block_result_update()]
#' method.
#'
#' @section The head row:
#' A method can put its own tools, such as a download button, into the
#' block's head row and give the block's sentence a title with
#' [result_head()]. It reads the title, subtitle and caption now on screen
#' with [result_titles()].
#'
#' @param result The block's evaluated result.
#' @param block The block object.
#' @param session The block's Shiny module session.
#' @param ... Passed to methods.
#' @return A shiny render function, as [blockr.core::block_output()] requires.
#' @seealso [block_result_update()]
#' @examples
#' \dontrun{
#' # In a package with its own table class `my_table`:
#' block_result_output.my_table <- function(result, block, session, ...) {
#'   shiny::renderUI(draw_my_table(result))
#' }
#' }
#' @export
block_result_output <- function(result, block, session, ...) {
  UseMethod("block_result_output")
}

#' @rdname block_result_output
#' @export
block_result_output.default <- function(result, block, session, ...) {
  render_dynamic_output(result, block, session)
}

#' Update a block result that is on screen
#'
#' [block_result_output()] draws a result from scratch: the function it
#' returns replaces the block's output, and the browser builds the result
#' again. A table then loses its scroll position, its search text and its
#' collapsed rows on every change of a control. A class that can change what
#' it has drawn registers a method here. The method sends the new result to
#' the browser itself, for example with `session$sendCustomMessage()` or with
#' the update function of the widget it drew, and returns `TRUE`. The block
#' then leaves its output as it is.
#'
#' @section When it is called:
#' The block calls the method when the result it drew last is still on
#' screen and the new result has the same class vector. In every other case
#' it calls [block_result_output()]:
#'
#' * the first result of a block, also of a block that was removed and added
#'   again under the same id;
#' * a result of another class;
#' * the first result after an error or after an input was unavailable;
#' * a result whose predecessor has not reached the browser: a result
#'   evaluated twice while the block was hidden, or a second evaluation in
#'   the flush that drew the first;
#' * after a method here returned anything but `TRUE`, or failed.
#'
#' So a method can return `FALSE` for a change it cannot apply in place, such
#' as a table with other columns, and the result is drawn from scratch.
#'
#' A class without a method is drawn as before: the function its
#' [block_result_output()] method returns is the block's output, unchanged.
#'
#' @section What the method has to do:
#' The output keeps the HTML the first render sent, and the render function
#' of that render does not run again. So everything that follows the result
#' has to be sent by this method: the data, and a caption the result draws
#' itself. Use ids that are the same in every call, such as
#' `session$ns("table")`.
#'
#' [result_head()] and [result_titles()] work as in [block_result_output()].
#' The head row starts empty before the call, so call [result_head()] with
#' the same arguments as in the first render. [result_titles()], called in
#' the method, gives the sentence as it is now, and the block calls the
#' method again when the sentence changes.
#'
#' @section What the method can rely on:
#' When the method is called, the HTML that [block_result_output()] drew for
#' the last result was sent to the browser in an earlier flush. A message the
#' method sends therefore arrives after that HTML, and after every message
#' an earlier call sent.
#'
#' It cannot rely on more than that. The block does not know whether the
#' browser has the element in the page at this moment: a layout can take a
#' hidden card out of the document. A message for an element that is not
#' there is lost unless the widget keeps it, so a widget that can be hidden
#' this way should apply the last message when it is attached again. If the
#' browser attaches the output element again without a new evaluation, it
#' draws the HTML of the first render.
#'
#' The block also knows only the class of what it drew last, not which
#' method drew it. A method that sometimes hands the result on with
#' `NextMethod()` has to note that itself, and return `FALSE` here while the
#' drawing in the page is not its own.
#'
#' @inheritParams block_result_output
#' @return `TRUE` when the result on screen was updated. Anything else has
#'   the block draw the result from scratch.
#' @seealso [block_result_output()]
#' @examples
#' \dontrun{
#' # A table widget with an update function of its own.
#' block_result_output.my_table <- function(result, block, session, ...) {
#'   id <- session$ns("table")
#'   shiny::renderUI(my_table_widget(result$data, id = id))
#' }
#'
#' block_result_update.my_table <- function(result, block, session, ...) {
#'   update_my_table_widget(session, session$ns("table"), result$data)
#'   TRUE
#' }
#' }
#' @export
block_result_update <- function(result, block, session, ...) {
  UseMethod("block_result_update")
}

#' @rdname block_result_update
#' @export
block_result_update.default <- function(result, block, session, ...) {
  FALSE
}

# What the block has drawn ---------------------------------------------------
#
# blockr.core assigns the function block_output() returns to the block's
# output on every evaluation. Shiny then runs it and sends its value, and the
# browser draws an HTML value again even when it has not changed. The one way
# to leave an output as it is on screen is a render function that cancels
# (`req(cancelOutput = TRUE)`): Shiny sends nothing for it.
#
# An update in place is right only while the result drawn last is still on
# screen. The block keeps that in `draw`, an environment in the head state:
#
#   now    the result this run of core's render observer drew
#   last   the one the run before it drew
#
# Each is `NULL` or an environment with `class`, the class vector of the
# result, and `shown`, `TRUE` once its render function has run and the flush
# that carries its HTML has gone to the browser. Shiny writes a custom message
# to the browser at once and an output's value at the end of the flush, so an
# update sent in the flush of the draw would arrive before the HTML it is
# for. Core calls block_render_trigger() at the start of
# every run, where `now` becomes `last`. A run that ends without a result (an
# error, an unavailable input) writes no `now`, so the run after it finds no
# `last` and draws from scratch.

#' Forget what a block has drawn
#' @param draw The `draw` environment of a head state.
#' @noRd
sb_draw_reset <- function(draw) {
  draw$last <- NULL
  draw$now <- NULL
  invisible(NULL)
}

#' A new run of core's render observer starts
#' @param draw The `draw` environment of a head state.
#' @noRd
sb_draw_step <- function(draw) {
  draw$last <- draw$now
  draw$now <- NULL
  invisible(NULL)
}

#' Does a class of `result` have a `block_result_update()` method?
#'
#' Only such a result is tracked. Any other is drawn exactly as a block
#' without this hook draws it.
#' @noRd
sb_updates_in_place <- function(result) {
  for (cls in class(result)) {
    method <- utils::getS3method(
      "block_result_update", cls, optional = TRUE,
      envir = asNamespace("blockr.script")
    )
    if (!is.null(method)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Draw a result, in place where its class can and the last one is on screen
#'
#' @param head From `sb_head()`, with the stage open.
#' @param result,block,session As in `block_result_output()`.
#' @return A render function for the block's output.
#' @noRd
sb_draw_result <- function(head, result, block, session) {
  draw <- head$draw
  last <- draw$last
  # Used up here. A caller that does not go through block_render_trigger()
  # never updates in place.
  sb_draw_reset(draw)

  if (!sb_updates_in_place(result)) {
    return(block_result_output(result, block, session))
  }

  on_screen <- !is.null(last) && isTRUE(last$shown) &&
    identical(last$class, class(result))
  if (on_screen && isTRUE(block_result_update(result, block, session))) {
    draw$now <- last
    return(sb_keep_output())
  }

  # What a method that then declined asked of the head row does not count.
  head$stage$new <- SB_HEAD_EMPTY
  render <- block_result_output(result, block, session)
  drawn <- new.env(parent = emptyenv())
  drawn$class <- class(result)
  drawn$shown <- FALSE
  draw$now <- drawn
  sb_track_render(render, drawn, session)
}

#' A render function that leaves the output as it is in the browser
#' @noRd
sb_keep_output <- function() {
  shiny::renderUI(shiny::req(FALSE, cancelOutput = TRUE))
}

#' Note when what a render function drew has gone to the browser
#'
#' Shiny runs an output's function at the next flush, and not at all while
#' the output is hidden. The value it returns is sent when that flush ends.
#' Until then the browser has nothing to update.
#'
#' @param render A render function.
#' @param drawn The environment `shown` is set in.
#' @param session The block's session.
#' @return `render`, with its attributes. When it has returned a value,
#'   `drawn$shown` is set after the flush that sends the value.
#' @noRd
sb_track_render <- function(render, drawn, session) {
  if (!is.function(render)) {
    return(render)
  }
  # Shiny passes `name` and `shinysession` to a function that has formals.
  bare <- length(formals(render)) == 0L
  tracked <- function(...) {
    out <- if (bare) render() else render(...)
    session$onFlushed(function() drawn$shown <- TRUE, once = TRUE)
    out
  }
  attributes(tracked) <- attributes(render)
  tracked
}
