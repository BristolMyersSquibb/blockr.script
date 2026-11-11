#' The script editor layer
#'
#' The `Blockr.Script` CodeMirror 6 widget plus its R server logic: column
#' completions, the inline diff when the assistant rewrites the script, Run
#' and Cmd-Enter, and the footer. It edits one string; the block supplies the
#' inputs and the expression.
#'
#' @name script-editor
#' @noRd
NULL


#' Return the R parse-error message for a code string, or NULL if it parses.
#'
#' Used to gate the editor's Run button: only syntactically valid code may be
#' committed/run, so runtime errors (not syntax errors) are what reach the
#' normal blockr evaluation system.
#'
#' @param code Character string of R code.
#' @return `NULL` if `code` parses, otherwise the condition message (string).
#' @noRd
parse_error <- function(code) {
  if (is.null(code) || !nzchar(trimws(code))) {
    return(NULL)
  }
  tryCatch({
    parse(text = code)
    NULL
  }, error = function(e) conditionMessage(e))
}

#' Condense a `parse()` error message for the status bar.
#'
#' `parse(text=)` reports e.g. `"<text>:6:0: unexpected end of input\n5: ...\n^"`.
#' We keep just the first line and reformat the `<text>:L:C:` prefix to a terse
#' `"Line L: <reason>"`. Falls back to the trimmed first line if it doesn't match.
#'
#' @param msg The string returned by `parse_error()`.
#' @return A short single-line string suitable for the footer.
#' @noRd
parse_error_brief <- function(msg) {
  first <- sub("\n.*$", "", msg)
  m <- regmatches(first, regexec("^<text>:(\\d+):\\d+:\\s*(.*)$", first))[[1]]
  if (length(m) == 3L) {
    sprintf("Line %s: %s", m[2L], m[3L])
  } else {
    trimws(first)
  }
}


#' Wire the script editor's server logic
#'
#' Adds the editor's reactive machinery: pushes completions, opens the inline
#' diff when the script is written from outside (the assistant), commits the
#' editor's text on Run and Cmd-Enter, handles Accept all and Reject all, and
#' renders the footer (`output$footer_ui`). The editor input id is `fn_code`.
#'
#' @param input,output,session Shiny module objects.
#' @param r_script The committed script, a `reactiveVal`. Run writes to it.
#' @param cols A reactive returning the completions to offer, see
#'   `sb_completions()`. Defaults to none.
#' @param rest_label A reactive returning a short string for the footer at
#'   rest, or `NULL` for the blank bar. The script block uses it to say how
#'   many lines became controls.
#' @param active A reactive that is `TRUE` once the editor has been shown.
#'   The footer is not built before.
#' @param problem A function of the editor's text that returns `NULL` when
#'   the text may be committed, and otherwise one line saying why not. Run is
#'   disabled while it returns a reason. The default only asks that the text
#'   parses.
#' @return Invisibly `NULL` (used for its side effects).
#' @noRd
setup_code_editor_server <- function(input, output, session, r_script,
                                     cols = shiny::reactive(NULL),
                                     rest_label = shiny::reactive(NULL),
                                     active = shiny::reactive(TRUE),
                                     problem = function(code) {
                                       err <- parse_error(code)
                                       if (!is.null(err)) parse_error_brief(err)
                                     }) {
  ns <- session$ns

  # Push the completions to the editor whenever they change.
  shiny::observeEvent(cols(), {
    session$sendCustomMessage(
      "blockr-script-completions",
      list(id = ns("fn_code"), columns = as.list(cols()))
    )
  }, ignoreNULL = FALSE)

  # The last script that ran, and whether a review of an outside write is
  # open (the diff is showing).
  r_prev   <- shiny::reactiveVal(shiny::isolate(r_script()))
  r_review <- shiny::reactiveVal(FALSE)
  # Number of unresolved diff hunks, reported by the client: the "N edits"
  # label.
  r_nedits <- shiny::reactiveVal(0L)
  # The script from before the outside writes began. The assistant may write
  # several times in a row, and the diff's "original" side stays this one.
  r_review_original <- shiny::reactiveVal(NULL)
  # Set by a run this editor commits itself (Cmd-Enter, a rejected hunk), so
  # the observer below does not take it for an outside write.
  r_internal <- shiny::reactiveVal(FALSE)

  # A write of the script from outside (the assistant, `external_ctrl`): it
  # runs at once, and the inline diff opens for review against the script
  # from before. A run this editor committed is skipped, and so is the Run
  # button, where the script equals the editor's text.
  shiny::observeEvent(r_script(), {
    new <- r_script()
    if (shiny::isolate(r_internal())) {
      r_internal(FALSE)
      r_prev(new)
      return()
    }
    if (!identical(new, input$fn_code)) {
      if (is.null(r_review_original())) {
        r_review_original(r_prev())
      }
      session$sendCustomMessage(
        "blockr-script-set",
        list(id = ns("fn_code"), code = new, original = r_review_original())
      )
      r_review(TRUE)
    }
    r_prev(new)
  }, ignoreInit = TRUE)

  # Commit the editor's text as the script. This is the one gate: text that
  # is empty or that `problem()` objects to never becomes the script, so what
  # reaches evaluation can only fail at run time, and blockr's own error band
  # reports that.
  commit <- function(code, internal) {
    if (is.null(code) || !nzchar(trimws(code))) {
      return()
    }
    if (!is.null(problem(code))) {
      return()
    }
    # Unchanged text does not fire the observer above, so the flag would stay
    # set and hide the next outside write.
    if (identical(code, r_script())) {
      return()
    }
    if (internal) {
      r_internal(TRUE)
    }
    r_script(code)
  }

  # Cmd-Enter, the footer's Run button and the run after a rejected hunk. The
  # client sends the text itself, which may be ahead of the debounced
  # `fn_code`.
  shiny::observeEvent(input$fn_exec, commit(input$fn_exec, internal = TRUE))

  # The client reports that every hunk has been resolved one by one.
  shiny::observeEvent(input$fn_review_done, {
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Accept all: the new script is already running, so only the diff closes.
  shiny::observeEvent(input$accept_all, {
    session$sendCustomMessage("blockr-script-merge-clear", list(id = ns("fn_code")))
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Reject all: the client puts the diff's original back and runs it through
  # `fn_exec`. Only the review state is closed here.
  shiny::observeEvent(input$reject_all, {
    session$sendCustomMessage("blockr-script-reject-all", list(id = ns("fn_code")))
    r_review(FALSE)
    r_review_original(NULL)
  })

  # Client reports the live hunk count (drives the "N edits" label).
  shiny::observeEvent(input$fn_nedits, {
    n <- suppressWarnings(as.integer(input$fn_nedits))
    r_nedits(if (is.na(n)) 0L else n)
  })

  # One footer bar. During a review it shows the number of edits with Accept
  # all and Reject all. Otherwise it shows Run, with what the block did at
  # rest, or the state of an edit that has not run. The first keystroke ends a
  # review, so the two never show together.
  output$footer_ui <- shiny::renderUI({
    shiny::req(active())
    if (isTRUE(r_review())) {
      n <- r_nedits()
      label <- if (is.na(n) || n <= 0L) {
        "Review change"
      } else {
        sprintf("%d %s", n, if (n == 1L) "edit" else "edits")
      }
      code_block_footer(
        label,
        shiny::actionButton(ns("accept_all"), "Accept all",
                            class = "blockr-script-btn blockr-script-btn--accept"),
        shiny::actionButton(ns("reject_all"), "Reject all",
                            class = "blockr-script-btn blockr-script-btn--reject")
      )
    } else {
      ed <- input$fn_code
      if (is.null(ed) || identical(trimws(ed), trimws(r_script()))) {
        # At rest: what the block did (the counts, or a warning about the
        # script) and Run. The bar is always there, so the frame keeps its
        # height on every Run.
        lab <- tryCatch(rest_label(), error = function(e) NULL)
        kind <- attr(lab, "kind", exact = TRUE)
        return(
          code_block_footer(
            if (is.null(lab)) "" else as.character(lab),
            code_run_button(ns),
            status = if (is.null(kind)) "ok" else kind,
            class = "blockr-script-footer--rest"
          )
        )
      }
      why <- problem(ed)
      if (!is.null(why)) {
        # The edit cannot run as it is: Run is disabled and the status says
        # why. Run-time errors go through blockr's own error band.
        code_block_footer(
          why,
          code_run_button(ns, disabled = TRUE),
          status = "error"
        )
      } else {
        code_block_footer("Edited, not run", code_run_button(ns),
                          status = "dirty")
      }
    }
  })

  # The footer is inside the gear tray, which starts `display:none`. Shiny
  # would suspend the output, and opening the tray from JS does not resume
  # it. So it is never suspended; `active` holds it back until the tray has
  # been opened once.
  shiny::outputOptions(output, "footer_ui", suspendWhenHidden = FALSE)

  invisible(NULL)
}


#' The editor UI: a bordered frame holding the CodeMirror mount and the footer
#'
#' The host block decides where to place it. Carries the dependencies.
#'
#' @param ns Namespace function (`shiny::NS(id)`).
#' @param fn_text The script to start with (written into `data-value`).
#' @param label Field label shown above the editor, or `NULL` for none.
#' @param strip Optional UI for the editor's header strip: the input chips
#'   and "+ Control".
#' @param marks The marks of the lines that are controls, see
#'   `cb_syntactic_marks()`, written into `data-input-marks`. An editor whose
#'   tray is never opened never gets the pushed message, so the first paint
#'   comes from the markup.
#' @return A Shiny tag.
#' @noRd
code_editor_ui <- function(ns, fn_text, label = "Script",
                           marks = NULL, strip = NULL) {
  shiny::tagList(
    script_editor_dep(),
    script_css_dep(),
    shiny::div(
      class = "blockr-script-block",
      if (!is.null(label)) {
        shiny::tags$label(class = "blockr-script-label", label)
      },
      shiny::div(
        class = "blockr-script-frame",
        if (!is.null(strip)) {
          shiny::div(class = "blockr-script-head", strip)
        },
        shiny::div(
          id = ns("fn_code"),
          class = "blockr-script",
          `data-value` = fn_text,
          # The inputs the client writes to: run now, the review is done,
          # the number of open hunks.
          `data-run-input` = ns("fn_exec"),
          `data-review-done` = ns("fn_review_done"),
          `data-count-input` = ns("fn_nedits"),
          `data-input-marks` = if (length(marks)) {
            as.character(jsonlite::toJSON(marks, auto_unbox = TRUE))
          }
        ),
        shiny::uiOutput(ns("footer_ui"))
      )
    )
  )
}


#' The gear icon (same path as `Blockr.icons.gear`), `fill="currentColor"`.
#' @noRd
gear_svg <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16">',
    '<path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ',
    "1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82",
    ".023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ",
    ".872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ",
    "2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31",
    ".17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-",
    ".1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-",
    "1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ",
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
}


#' The editor behind the gear
#'
#' The standard gear button opens the editor (and any `top` UI above it) in
#' the design system's gear tray: `.blockr-settings`,
#' driven by `Blockr.gearTray()` from blockr.ui. When the block is wide, the
#' open tray moves beside the output instead of above it (gear-editor.js).
#'
#' @param ns Namespace function.
#' @param fn_text The script to start with.
#' @param top Optional UI rendered above the editor inside the section.
#' @param label Editor field label.
#' @param marks The marks of the lines that are controls, passed on to
#'   `code_editor_ui()`.
#' @param head Optional UI drawn in the gear's row, left of the gear: the
#'   script's sentence.
#' @param strip Optional UI for the editor's header strip (see
#'   `code_editor_ui()`).
#' @param side_with Id of the element the open tray sits beside when the block
#'   is wide. The default is blockr.core's output for the block,
#'   `NS(<block id>, "result")`.
#' @return A Shiny tag.
#' @noRd
gear_editor_ui <- function(ns, fn_text, top = NULL, label = "Script",
                           marks = NULL, head = NULL, strip = NULL,
                           side_with = sub("-expr-$", "-result", ns(""))) {
  sec_id <- ns("fn-editor")
  tray <- shiny::div(
    id = sec_id,
    class = "blockr-settings blockr-settings--beak blockr-script-tray",
    `aria-label` = label,
    `data-side-with` = side_with,
    `data-code` = ns("fn_code"),
    `data-opened-input` = ns("tray_opened"),
    top,
    code_editor_ui(ns, fn_text, label = label, marks = marks, strip = strip)
  )
  # On the tray, which a host block may lift out of this wrapper.
  tray <- htmltools::attachDependencies(
    tray,
    c(htmltools::findDependencies(blockr.ui::controls_dep()),
      list(gear_editor_dep()))
  )
  shiny::div(
    class = "blockr-script-gear",
    shiny::div(
      class = "blockr-gear-header",
      head,
      shiny::tags$button(
        id = ns("fn-gear-btn"),
        type = "button",
        class = "blockr-gear-btn",
        `data-tray` = sec_id,
        htmltools::HTML(gear_svg())
      )
    ),
    tray
  )
}


#' A flush footer bar: a left status label and right-aligned actions.
#' @noRd
code_block_footer <- function(label, ..., status = NULL, class = NULL) {
  shiny::div(
    class = paste(c("blockr-script-footer", class), collapse = " "),
    shiny::span(
      class = paste(c("blockr-script-footer__label",
                      if (!is.null(status)) paste0("blockr-script-status--", status)),
                    collapse = " "),
      title = if (is.character(label)) label,
      label
    ),
    shiny::span(class = "blockr-script-footer__actions", ...)
  )
}

#' The Run button, with its Cmd/Ctrl-Enter hint.
#'
#' Not a Shiny input. A click sends the editor's current text to `fn_exec`
#' (script-tools.js), the path Cmd-Enter takes, so Run never commits text that
#' is one debounce behind what is on screen.
#' @noRd
code_run_button <- function(ns, disabled = FALSE) {
  shiny::tags$button(
    type = "button",
    class = "btn btn-default blockr-script-btn blockr-script-btn--run",
    `data-editor` = ns("fn_code"),
    disabled = if (disabled) NA,
    title = if (!disabled) "Run (\u2318/Ctrl + Enter)",
    "Run",
    # Mod-Enter: Cmd+Enter on a Mac, Ctrl+Enter elsewhere. The page decides
    # which hint shows (gear-editor.js marks a Mac).
    if (!disabled) {
      shiny::tagList(
        shiny::span(class = "blockr-script-kbd blockr-script-kbd--mac",
                    "\u2318\u21B5"),
        shiny::span(class = "blockr-script-kbd blockr-script-kbd--pc",
                    "Ctrl \u21B5")
      )
    }
  )
}
