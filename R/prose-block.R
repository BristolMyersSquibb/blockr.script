#' Prose block
#'
#' Text that can compute. The block holds markdown, edited as it reads (bold
#' as bold, no syntax to learn), and R inside it the way Quarto writes inline
#' code: `` `r nrow(data)` ``. Typing `` `r `` and a space in the editor opens
#' a small code field that suggests the inputs, their columns and a few common
#' functions; Enter computes it, and the text shows the value, marked with a
#' dotted underline. Pointing at a value shows its code, a click edits it.
#'
#' The markdown with its code is the block's setting, in its control. Its
#' result is the markdown with the values written in, which the next block
#' takes as text (a report, a slide). Because the code is Quarto's own inline
#' code, the same text runs in a Quarto document where the inputs are objects
#' of the same names.
#'
#' An expression that fails marks its value in the editor; in the result it
#' stops the block with the error.
#'
#' Values refresh eagerly as you type, outside the DAG. The text itself
#' commits on blur or Ctrl+Enter, never on a keystroke, so typing does not
#' recompute the blocks downstream.
#'
#' @section Who can run code:
#' Whoever can edit the text can run R code on the server, as with the script
#' block. The code runs in a fresh environment whose parent is the global
#' environment, so `<<-` and `assign(..., envir = globalenv())` write to a
#' place that all sessions of the app share.
#'
#' @section Inputs:
#' The block takes any number of inputs, including none, named as in
#' [new_script_block()]:
#'
#' * The first link into the block is `data`.
#' * Every further link is read by its link name, or as `.arg2`, `.arg3`, ...
#'   when the link has none.
#' * With no input connected the text is plain text, or computes with values
#'   that need no input.
#'
#' On export each input the text uses is replaced by the id of the upstream
#' block.
#'
#' @param text Markdown string, with inline R as `` `r expr` ``.
#' @param ... Forwarded to [blockr.core::new_block()].
#'
#' @return A `prose_block` (also a `text_block`).
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         data = new_dataset_block("iris", "datasets"),
#'         note = new_prose_block("## Iris\n\nThere are **`r nrow(data)`** rows.")
#'       ),
#'       links = links(from = "data", to = "note", input = "data")
#'     )
#'   )
#' }
#'
#' @export
new_prose_block <- function(text = character(), ...) {

  blockr.core::new_text_block(
    function(id, ...args) {
      # Purely variadic, with the script block's naming rule (R/args.R).
      if (missing(...args)) {
        ...args <- list()
      }
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_text <- shiny::reactiveVal(paste(text, collapse = "\n"))

          # The slots, named by what the text reads them as.
          r_refs <- shiny::reactive(sb_slots(...args))

          # The inputs that can be read now, for the code field's
          # suggestions.
          r_data <- shiny::reactive({
            inputs <- sb_inputs(...args)
            Filter(
              function(x) !is.null(x) && !sb_is_unavailable(x),
              inputs
            )
          })

          # Inputs and their columns -> JS, for the code field's suggestions.
          shiny::observe({
            inputs <- lapply(r_data(), function(x) {
              as.list(if (is.data.frame(x)) colnames(x) else names(x))
            })
            session$sendCustomMessage(
              "prose-columns",
              list(id = session$ns("editor"), inputs = inputs)
            )
          })

          # Value preview: the editor reports every inline expression it
          # holds (eagerly, on edit, not the document commit); each is
          # evaluated on its own and the values travel back in one message.
          # A failure is local to its value; nothing here touches the DAG.
          shiny::observe({
            exprs <- unique(unlist(input$prose_exprs))
            inputs <- sb_inputs(...args)
            # A connected input that has no value yet. An expression that
            # names one waits (dormant) and does not show an error.
            waiting <- Filter(
              function(nm) {
                is.null(inputs[[nm]]) || sb_is_unavailable(inputs[[nm]])
              },
              names(r_refs())
            )
            data <- inputs[setdiff(names(inputs), waiting)]

            if (length(exprs) == 0L) {
              return()
            }

            vals <- lapply(exprs, function(e) {
              if (any(prose_code_names(e) %in% waiting)) {
                return(list(ok = NA))
              }
              tryCatch(
                list(
                  ok = TRUE,
                  value = inline_value(eval_inline(e, data), 80L)
                ),
                error = function(err) {
                  list(ok = FALSE, value = conditionMessage(err))
                }
              )
            })

            session$sendCustomMessage(
              "prose-values",
              list(
                id = session$ns("editor"),
                values = stats::setNames(vals, exprs)
              )
            )
          })

          # What the editor and the server know of each other. `version`
          # counts the writes that did not come from the editor, `synced` is
          # the text the editor last had from the server or committed, and
          # `own` marks a change of `r_text` that came from the editor.
          sync <- new.env(parent = emptyenv())
          sync$version <- 0L
          sync$synced <- shiny::isolate(r_text())
          sync$own <- NULL

          # UI -> state (explicit commit from JS: blur / Ctrl-Enter). A
          # commit is stale when the text was written from outside after the
          # editor last heard from the server: the write is still waiting to
          # be sent, or the commit carries an older version than the write.
          # A stale commit is dropped, so the outside write stands.
          shiny::observeEvent(input$text, {
            current <- shiny::isolate(r_text())
            version <- input$text_version
            stale <- !identical(current, sync$synced) ||
              (!is.null(version) && version < sync$version)
            if (stale || identical(input$text, current)) {
              return()
            }
            sync$own <- input$text
            sync$synced <- input$text
            r_text(input$text)
          })

          # State -> UI: writes from outside (another block, an assistant)
          # are pushed to the editor. A commit is not sent back: the editor
          # already has it, and the echo would replace what the user typed
          # since.
          shiny::observeEvent(r_text(), {
            own <- sync$own
            sync$own <- NULL
            if (identical(own, r_text())) {
              return()
            }
            sync$version <- sync$version + 1L
            sync$synced <- r_text()
            session$sendCustomMessage(
              "prose-set",
              list(
                id = session$ns("editor"),
                markdown = r_text(),
                version = sync$version
              )
            )
          }, ignoreInit = TRUE)

          list(
            expr = shiny::reactive(prose_expr(r_text(), r_refs())),
            state = list(
              text = r_text
            )
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        blockr.ui::controls_dep(),
        prose_block_dep(),
        shiny::div(
          id = shiny::NS(id, "editor"),
          class = "blockr-prose",
          `data-input-id` = shiny::NS(id, "text"),
          `data-exprs-id` = shiny::NS(id, "prose_exprs"),
          `data-initial` = paste(text, collapse = "\n")
        )
      )
    },
    expr_type = "bquoted",
    class = "prose_block",
    external_ctrl = "text",
    # text needs no input: one without any computes, as plain text
    allow_empty_state = list(data = list(...args = 0L)),
    ...
  )
}

# The block expression: `inline_r()` on the text, with the inputs the text
# uses as `.()` slots, which core binds to the upstream results (and, on
# export, replaces by the upstream block ids).
prose_expr <- function(text, refs) {
  if (is.null(names(refs))) {
    refs <- stats::setNames(refs, refs)
  }
  names_used <- prose_names(text)
  used <- intersect(names(refs), names_used)
  data <- stats::setNames(
    lapply(used, function(x) call(".", as.name(refs[[x]]))),
    used
  )
  # No input, but the text reads `data`: it is NULL.
  if ("data" %in% names_used && !"data" %in% names(refs)) {
    data <- c(data, list(data = NULL))
  }
  bquote(
    blockr.script::inline_r(.(txt), list(..(data))),
    list(txt = text, data = data),
    splice = TRUE
  )
}

# Every name the inline code of a text uses. Code that does not parse uses
# nothing; it fails when the text is evaluated.
prose_names <- function(text) {
  unique(unlist(lapply(inline_r_codes(text), prose_code_names)))
}

prose_code_names <- function(code) {
  tryCatch(
    all.names(parse(text = code, keep.source = FALSE)),
    error = function(e) character()
  )
}

#' Inline R in markdown
#'
#' Writes the value of every `` `r expr` `` in a markdown string into the
#' text, as knitr does for inline code: each expression is evaluated with the
#' objects in `data` in scope, and its value replaces the code.
#'
#' Left alone: code in fenced blocks, other inline code, and inline code
#' written between double backticks to show the syntax
#' (``` `` `r x` `` ```). Code in a block indented by four spaces is
#' evaluated, as knitr does.
#'
#' A value has to be a vector of at most 100 elements. A number is written in
#' full (`1000000`, not `1e+06`) and several values are joined by commas.
#' `NULL` writes nothing. When the code holds several expressions, the value
#' of the last one is written.
#'
#' Each expression is evaluated in a fresh environment whose parent is the
#' global environment. `data` is always bound there (`NULL` when the list has
#' none), so it never resolves to `utils::data`. `<<-` writes to the global
#' environment, which all sessions of a Shiny app share.
#'
#' @param text A markdown string.
#' @param data A named list of objects the expressions can use.
#'
#' @return The markdown string with the values written in.
#'
#' @examples
#' inline_r("There are `r nrow(data)` cars.", list(data = mtcars))
#'
#' @export
inline_r <- function(text, data = list()) {

  text <- paste(text, collapse = "\n")
  if (!nzchar(text)) {
    return(text)
  }
  md <- inline_r_lines(text)
  lines <- md$lines

  for (i in which(!md$fence)) {
    spans <- inline_r_spans(lines[i])
    # From the right, so the positions of the spans to the left stay valid.
    for (k in rev(seq_len(nrow(spans)))) {
      val <- inline_value(eval_inline(spans$code[k], data))
      lines[i] <- paste0(
        substr(lines[i], 1L, spans$start[k] - 1L),
        val,
        substr(lines[i], spans$end[k] + 1L, nchar(lines[i]))
      )
    }
  }

  paste(lines, collapse = "\n")
}

# The inline R of one line: where each `r expr` starts and ends, and its
# code. Code spans are read as markdown reads them: a run of n backticks
# opens a span that the next run of n backticks closes. Only a span between
# single backticks whose content starts with "r " is inline R, so
# `` `r x` `` (the way to show the syntax) and ``r x`` are left alone.
inline_r_spans <- function(line) {
  none <- data.frame(start = integer(), end = integer(), code = character())
  runs <- gregexpr("`+", line)[[1L]]
  if (runs[1L] == -1L) {
    return(none)
  }
  pos <- as.integer(runs)
  len <- attr(runs, "match.length")
  out <- list()
  i <- 1L
  while (i < length(pos)) {
    j <- which(len == len[i] & seq_along(len) > i)[1L]
    if (is.na(j)) {
      i <- i + 1L
      next
    }
    inner <- substr(line, pos[i] + len[i], pos[j] - 1L)
    if (len[i] == 1L && grepl("^r[ \t]+[^ \t]", inner)) {
      out[[length(out) + 1L]] <- data.frame(
        start = pos[i], end = pos[j],
        code = sub("^r[ \t]+", "", inner)
      )
    }
    i <- j + 1L
  }
  if (length(out)) do.call(rbind, out) else none
}

# A markdown string as lines, empty lines and a trailing newline kept, with
# the lines that are inside (or open or close) a fenced code block marked.
# A fence closes on a line of the same character that is at least as long as
# the opening one, so a longer fence can hold a shorter one, and a backtick
# fence can hold a line of tildes.
inline_r_lines <- function(text) {
  lines <- regmatches(text, gregexpr("\n", text), invert = TRUE)[[1L]]
  mark <- sub("^\\s*(`{3,}|~{3,}).*$", "\\1", lines)
  mark[!grepl("^\\s*(`{3,}|~{3,})", lines)] <- ""
  closes <- grepl("^\\s*(`{3,}|~{3,})\\s*$", lines)

  fence <- logical(length(lines))
  open <- ""
  for (i in seq_along(lines)) {
    if (!nzchar(open)) {
      open <- mark[i]
      fence[i] <- nzchar(open)
    } else {
      fence[i] <- TRUE
      if (closes[i] && substr(mark[i], 1L, 1L) == substr(open, 1L, 1L) &&
            nchar(mark[i]) >= nchar(open)) {
        open <- ""
      }
    }
  }
  list(lines = lines, fence = fence)
}

# The code of every inline expression outside fenced blocks.
inline_r_codes <- function(text) {
  text <- paste(text, collapse = "\n")
  if (!nzchar(text)) {
    return(character())
  }
  md <- inline_r_lines(text)
  unlist(lapply(
    md$lines[!md$fence],
    function(line) inline_r_spans(line)$code
  ))
}

# One piece of inline code, evaluated with the inputs in scope. Functions
# come from the search path, as they do in a Quarto document. `data` is bound
# in every case, so code that reads it without an input gets NULL and not
# the function utils::data. Several expressions run in order and the last
# value counts.
eval_inline <- function(expr, data) {
  data <- as.list(data)
  if (!"data" %in% names(data)) {
    data["data"] <- list(NULL)
  }
  env <- list2env(data, parent = globalenv())
  exprs <- parse(text = expr, keep.source = FALSE)
  if (!length(exprs)) {
    stop("The inline code holds no expression.", call. = FALSE)
  }
  value <- NULL
  for (e in exprs) {
    value <- eval(e, env)
  }
  value
}

# The most values one inline expression may write.
INLINE_MAX_LENGTH <- 100L

# A value as text in a sentence: each number as format() writes it on its
# own, a vector as a comma-separated list. `max` shortens it for the editor's
# preview, with an ellipsis.
inline_value <- function(x, max = NULL) {
  if (is.null(x)) {
    return("")
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.atomic(x)) {
    stop(
      "An inline value must be a short vector, not a ", class(x)[1L], ".",
      call. = FALSE
    )
  }
  n <- length(x)
  if (is.null(max) && n > INLINE_MAX_LENGTH) {
    stop(
      "An inline value must be a short vector. This one has ", n,
      " values.", call. = FALSE
    )
  }
  shown <- if (is.null(max)) x else utils::head(x, 20L)
  parts <- if (is.numeric(shown) && !is.object(shown)) {
    # One by one, so 1 and 2.5 do not become 1.0 and 2.5, and with a penalty
    # on scientific notation, so a million is 1000000.
    vapply(
      shown, format, character(1L), trim = TRUE, scientific = 10L,
      USE.NAMES = FALSE
    )
  } else {
    format(shown, trim = TRUE)
  }
  out <- paste(parts, collapse = ", ")
  if (!is.null(max) && (n > 20L || nchar(out) > max)) {
    out <- paste0(substr(out, 1L, max - 1L), "\u2026")
  }
  out
}

#' HTML dependency for the prose block JS/CSS
#'
#' Ships `inst/js/prose-block.js`, built from `tools/srcjs/prose-block/` with
#' esbuild (see `package.json`), and `inst/css/prose-block.css`.
#'
#' @return A list of [htmltools::htmlDependency()] objects.
#' @keywords internal
prose_block_dep <- memoise0(function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-prose-js",
      version = js_version("prose-block.js"),
      src = system.file("js", package = "blockr.script"),
      script = "prose-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-prose-css",
      version = js_version("prose-block.css", "css"),
      src = system.file("css", package = "blockr.script"),
      stylesheet = "prose-block.css"
    )
  )
})
