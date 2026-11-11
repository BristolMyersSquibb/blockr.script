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
#' (`output_render_observer()`), so a method that creates observers must guard
#' them (once per session, keyed on the namespace). Assigning to
#' `session$output` is idempotent and needs no guard.
#'
#' @param result The block's evaluated result.
#' @param block The block object.
#' @param session The block's Shiny module session.
#' @param ... Passed to methods.
#' @return A shiny render function, as [blockr.core::block_output()] requires.
#' @export
block_result_output <- function(result, block, session, ...) {
  UseMethod("block_result_output")
}

#' @rdname block_result_output
#' @export
block_result_output.default <- function(result, block, session, ...) {
  render_dynamic_output(result, block, session)
}
