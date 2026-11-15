# The head row and a result method ------------------------------------------
#
# The head row (sentence, tools, gear) is drawn by the block's expression
# module. A `block_result_output()` method runs in the block's own session,
# one level up. The two meet in a small state object per block, kept in
# `session$userData`, which every module session of a Shiny session shares.
# It is keyed by the block's namespace and removed when the block's scope is
# destroyed.
#
#   tools, titles, caption   written by result_head(), read by the head row
#   resolved                 written by the sentence, read by result_titles()
#   draw                     what the block has drawn (R/result-output.R)

SB_HEAD_EMPTY <- list(tools = NULL, titles = list(), caption = TRUE)

SB_NO_TITLES <- list(
  title = NULL, subtitle = NULL, caption = NULL, filters = character(),
  declared = character(), templates = list()
)

#' The key of a block's head state
#'
#' @param prefix `session$ns("")` of the block's session or of its expression
#'   module, which core mounts under the block as `expr`.
#' @return The block's namespace prefix.
#' @noRd
sb_head_key <- function(prefix) {
  sub("expr-$", "", prefix)
}

#' The head state of one block, created on first use
#'
#' @param session A session of the block or of its expression module.
#' @param key From `sb_head_key()`.
#' @return A list of four `reactiveVal`s (`tools`, `titles`, `caption`,
#'   `resolved`, which holds both forms of the sentence), `stage`, an
#'   environment `block_output.script_block()` collects a method's
#'   `result_head()` call in, and `draw`, an environment with what the block
#'   has drawn.
#' @noRd
sb_head <- function(session, key) {
  store <- session$userData
  if (is.null(store$blockr_script_head)) {
    store$blockr_script_head <- new.env(parent = emptyenv())
  }
  heads <- store$blockr_script_head
  # A root session has the empty prefix, which is no name for assign().
  key <- paste0("head:", key)
  if (is.null(heads[[key]])) {
    heads[[key]] <- list(
      tools = shiny::reactiveVal(NULL),
      titles = shiny::reactiveVal(list()),
      caption = shiny::reactiveVal(TRUE),
      # With the script's `ui_*` lines and without them.
      resolved = shiny::reactiveVal(list(ui = SB_NO_TITLES, plain = SB_NO_TITLES)),
      stage = new.env(parent = emptyenv()),
      draw = new.env(parent = emptyenv())
    )
    # blockr.core destroys a block's scope when the block is removed, and
    # Shiny then refuses every reactive made in it. The state goes with the
    # scope, so a block added again under the same id gets a new one.
    sb_on_destroy(session, function() {
      if (exists(key, envir = heads, inherits = FALSE)) {
        rm(list = key, envir = heads)
      }
    })
  }
  heads[[key]]
}

#' Run a function when a session's scope is destroyed
#'
#' @param session A module session. A session that cannot be destroyed (a
#'   root session, a Shiny without `onDestroy()`) is left alone.
#' @param fun A function without arguments.
#' @noRd
sb_on_destroy <- function(session, fun) {
  register <- tryCatch(session$onDestroy, error = function(e) NULL)
  if (is.function(register)) {
    try(register(fun), silent = TRUE)
  }
  invisible(NULL)
}

#' Write what a method asked for, changing only what differs
#'
#' A method is called on every re-evaluation and usually asks for the same
#' thing again. Nothing is invalidated then, so the head row is not redrawn.
#'
#' @param head From `sb_head()`.
#' @param new A list with `tools`, `titles` and `caption`.
#' @noRd
sb_head_commit <- function(head, new) {
  for (field in c("tools", "titles", "caption")) {
    if (!identical(shiny::isolate(head[[field]]()), new[[field]])) {
      head[[field]](new[[field]])
    }
  }
  invisible(NULL)
}

#' Check the titles a method gave
#' @return A list with the tiers that are non-empty strings.
#' @noRd
sb_check_titles <- function(titles) {
  if (is.null(titles)) {
    return(list())
  }
  stopifnot("`titles` must be a list" = is.list(titles))
  unknown <- setdiff(names(titles), SB_TIERS)
  if (is.null(names(titles)) || length(unknown)) {
    stop(
      "`titles` takes `title`, `subtitle` and `caption`.",
      call. = FALSE
    )
  }
  out <- list()
  for (tier in SB_TIERS) {
    x <- titles[[tier]]
    if (is.null(x)) {
      next
    }
    if (!is.character(x) || length(x) != 1L) {
      stop("`titles$", tier, "` must be one string.", call. = FALSE)
    }
    if (!is.na(x) && nzchar(x)) {
      out[[tier]] <- x
    }
  }
  out
}

#' Fill the head row of a script block from a result method
#'
#' A script block has one head row: the sentence on the left, the gear on
#' the right. A [block_result_output()] method calls `result_head()` to put
#' its own tools into that row, left of the gear, and to give the sentence
#' titles for the lines the script does not declare.
#'
#' The head row starts empty before every render. A method that does not
#' call `result_head()` leaves the row as the script block draws it, so the
#' tools of one result class are gone as soon as the script returns another.
#' The method is called on every re-evaluation; calling `result_head()` again
#' with the same arguments redraws nothing. The tools are hidden while the
#' block has no result on screen, for example after an error.
#'
#' @section Tools:
#' `tools` is drawn as it is. A direct child that is a link or a button gets
#' the size and look of an icon button (26 px square), so give it an icon as
#' its content and a `title` as its name. Ids are namespaced with
#' `session$ns()` and their outputs and inputs live on `session`, as anywhere
#' in the method. Assigning a `downloadHandler` to `session$output` on every
#' call is fine. An observer on `session$input` must be created once per
#' block and not once per call, see [block_result_output()].
#'
#' @section Caption:
#' The script block draws a `#| caption:` line in the head row, under the
#' subtitle. A result that draws the caption itself, such as a table with the
#' caption under it, passes `caption = FALSE` and reads the text from
#' [result_titles()]. The head row then leaves the caption out, so there is
#' one copy on screen. A control that only the caption names stays in the
#' grid of controls, because the head row has no word for it.
#'
#' @param session The block's session, as the method received it.
#' @param tools Tags for the head row, or `NULL` for none.
#' @param titles A list with any of `title`, `subtitle` and `caption`, each
#'   one plain string. The sentence shows them for the lines the script does
#'   not declare. A line the script declares wins, and a script line with
#'   nothing after the colon (`#| title:`) switches that line off. The
#'   strings are shown as they are; `{...}` is not read in them.
#' @param caption `FALSE` when the result draws the caption itself.
#' @return `NULL`, invisibly.
#' @seealso [result_titles()] for the sentence as text,
#'   [block_result_output()] for the method this is called from.
#' @examples
#' \dontrun{
#' block_result_output.my_table <- function(result, block, session, ...) {
#'   session$output$csv <- shiny::downloadHandler(
#'     filename = "table.csv",
#'     content = function(file) utils::write.csv(result$data, file)
#'   )
#'   result_head(
#'     session,
#'     tools = shiny::downloadLink(
#'       session$ns("csv"), label = shiny::icon("download"),
#'       title = "Download"
#'     ),
#'     titles = list(title = "My table"),
#'     caption = FALSE
#'   )
#'   shiny::renderUI(
#'     shiny::tagList(
#'       draw_my_table(result),
#'       shiny::tags$p(result_titles(session)$caption)
#'     )
#'   )
#' }
#' }
#' @export
result_head <- function(session, tools = NULL, titles = NULL, caption = TRUE) {
  stopifnot(
    "`caption` must be TRUE or FALSE" =
      is.logical(caption) && length(caption) == 1L && !is.na(caption)
  )
  new <- list(tools = tools, titles = sb_check_titles(titles), caption = caption)
  head <- sb_head(session, session$ns(""))
  if (isTRUE(head$stage$open)) {
    # Inside block_output.script_block(), which writes it when the method
    # has returned.
    head$stage$new <- new
  } else {
    sb_head_commit(head, new)
  }
  invisible(NULL)
}

#' The sentence of a script block as text
#'
#' The title, subtitle and caption a script block shows, as the plain text
#' now on screen. For a [block_result_output()] method that needs them
#' outside the head row: in a downloaded file, or for a caption drawn under a
#' table.
#'
#' A line the script declares is resolved with the current control values and
#' the data tokens (`{n}`, `{label(col)}`). A line it does not declare is the
#' title the method gave to [result_head()]. A line that is neither, or that
#' the script switched off, is `NULL`.
#'
#' The sentence can change without the block being evaluated again, for
#' example when a control that only the title names is changed. So read it
#' where it is used and do not keep a copy: inside a render function the call
#' takes a reactive dependency and the output follows the sentence, inside
#' the `content` function of a `downloadHandler` it gives the current text.
#'
#' @section Lines for the app only:
#' A script can carry `#| ui_title:`, `#| ui_subtitle:` and `#| ui_caption:`
#' lines, which are meant for the app: a `ui_title` or `ui_caption` replaces
#' the plain line on screen and a `ui_subtitle` is added to it. With
#' `ui = TRUE` the result is the sentence as on screen. A download or a
#' report passes `ui = FALSE` and gets the sentence built from the plain
#' lines only. Both forms follow the controls.
#'
#' @section What the script declares:
#' `declared` names the lines the script has, also one it switched off with
#' nothing after the colon. So for `caption`:
#'
#' * in `declared` and a string: the script's caption;
#' * in `declared` and `NULL`: the script switched the caption off, or it
#'   resolved to nothing;
#' * not in `declared` and a string: the title the method gave;
#' * not in `declared` and `NULL`: there is none.
#'
#' `templates` holds the declared lines as the script wrote them, before
#' anything is filled in, for a rule that depends on the wording, such as a
#' caption that is exactly `{filters}`.
#'
#' @param session The block's session, as the method received it.
#' @param ui `TRUE` for the sentence as on screen, `FALSE` for the sentence
#'   without the script's `ui_*` lines.
#' @return A list with
#'
#'   * `title`, `subtitle` and `caption`, each one string or `NULL`;
#'   * `filters`, the filter trail that `{filters}` prints, as a character
#'     vector (empty when the input carries none);
#'   * `declared`, a character vector with those of `"title"`, `"subtitle"`
#'     and `"caption"` that the script has a line for;
#'   * `templates`, a list keyed by the names in `declared`, each the line's
#'     text as written, `""` for a line that is switched off.
#' @seealso [result_head()]
#' @examples
#' \dontrun{
#' block_result_output.my_table <- function(result, block, session, ...) {
#'   session$output$csv <- shiny::downloadHandler(
#'     filename = "table.csv",
#'     content = function(file) {
#'       # without the lines the script wrote for the app only
#'       writeLines(c(result_titles(session, ui = FALSE)$title, ""), file)
#'       utils::write.table(result$data, file, append = TRUE, sep = ",")
#'     }
#'   )
#'   shiny::renderUI(draw_my_table(result))
#' }
#' }
#' @export
result_titles <- function(session, ui = TRUE) {
  stopifnot(
    "`ui` must be TRUE or FALSE" =
      is.logical(ui) && length(ui) == 1L && !is.na(ui)
  )
  resolved <- sb_head(session, session$ns(""))$resolved
  # Outside a reactive context, such as a test or a callback, the read has
  # nothing to register with.
  both <- tryCatch(resolved(), error = function(e) shiny::isolate(resolved()))
  both[[if (ui) "ui" else "plain"]]
}

#' Tell the browser that the block is being evaluated
#'
#' The message leaves at once, also while R is busy with the script. The end
#' is sent after the flush that follows, when the new result or the error
#' has gone to the browser. It is sent whether the script returned or failed.
#' The browser shows the cue only when the end takes longer than about 300 ms
#' to arrive (script-head.js).
#'
#' @param session The block's session, or `NULL` outside Shiny.
#' @noRd
sb_busy <- function(session) {
  if (is.null(session)) {
    return(invisible(NULL))
  }
  send <- function(on) {
    try(
      session$sendCustomMessage(
        "blockr-script-busy", list(block = session$ns(""), busy = on)
      ),
      silent = TRUE
    )
  }
  send(TRUE)
  session$onFlushed(function() send(FALSE), once = TRUE)
  invisible(NULL)
}
