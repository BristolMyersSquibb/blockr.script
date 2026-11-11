#' Evaluate a script and return what it shows
#'
#' Runs the expression through `evaluate::evaluate()`, which records base R
#' plots. The result is, in this order: a ggplot the script returned, the
#' last plot it drew, the visible value of its last statement, or, when it
#' only printed (`cat()`, `print()`), the printed text.
#'
#' An error is signalled again as the condition it was, with its class and
#' call. Warnings are signalled again after the script has run, so the block
#' can show them.
#'
#' @param expr Expression to evaluate
#' @param env Environment for evaluation
#' @return The result (any R object, including a `recordedplot`)
#' @noRd
eval_with_plot_capture <- function(expr, env) {
  # Create environment with access to all attached packages (stats, graphics,
  # etc.). all.names = TRUE is load-bearing: variadic inputs are bound under
  # dot-prefixed reference symbols (.arg1, .arg2, ... for unnamed DAG-UI slots),
  # and the default as.list() drops names starting with ".", which would strip
  # those inputs out of the eval environment.
  eval_env <- list2env(as.list(env, all.names = TRUE), parent = .GlobalEnv)

  # evaluate() takes text. The expression is not deparsed for it, because
  # that loses digits of long numbers and changes some strings. It is handed
  # over in a variable, and the text only evaluates that variable.
  runner <- new.env(parent = eval_env)
  runner$expr <- expr
  runner$env <- eval_env

  # The value of the script is wrapped, so it can be told from printed output
  # further down: both would otherwise arrive as plain character vectors.
  res <- evaluate::evaluate(
    "eval(expr, env)",
    runner,
    stop_on_error = 1L,
    output_handler = evaluate::new_output_handler(
      value = function(x) structure(list(value = x), class = "sb_value")
    )
  )

  value <- NULL
  recorded_plot <- NULL
  printed <- character()
  warnings <- list()

  for (item in res) {
    if (inherits(item, "recordedplot")) {
      # Keep the last plot (in case of multiple snapshots)
      recorded_plot <- item
    } else if (inherits(item, "error")) {
      stop(item)
    } else if (inherits(item, "warning")) {
      warnings <- c(warnings, list(item))
    } else if (inherits(item, "sb_value")) {
      value <- item
    } else if (is.character(item) && !inherits(item, "source")) {
      printed <- c(printed, item)
    }
  }

  for (w in warnings) {
    warning(w)
  }

  result <- value$value

  # Priority: ggplot result > recorded plot > other result > printed text
  if (inherits(result, "ggplot")) {
    return(result)
  }

  if (!is.null(recorded_plot)) {
    return(recorded_plot)
  }

  if (is.null(value) && length(printed)) {
    return(structure(paste(printed, collapse = ""), class = "script_output"))
  }

  result
}

# What a script printed, when it returned nothing visible. It prints as the
# text it is, without quotes and index.
#' @export
print.script_output <- function(x, ...) {
  cat(unclass(x), sep = "")
  invisible(x)
}

#' Is `x` an HTML-renderable object?
#'
#' TRUE for objects that carry their own HTML via the `htmltools::as.tags()`
#' contract: native HTML (`shiny.tag`, `html`, ...), htmlwidgets (plotly,
#' leaflet, DT, ...), gt tables, and anything that registers a dedicated
#' `as.tags` method in another package.
#'
#' Deliberately a tight allow-list, not "does `as.tags()` not error": `as.tags`
#' is eager (it turns a bare string or list into text/tag nodes), so we exclude
#' base implicit classes and only accept a *dedicated* `as.tags` method. Keeps
#' plain strings, numbers, lists and data frames off this branch so they reach
#' their own renderers (DataTable / preformatted text).
#'
#' @param x Any R object.
#' @return Single logical.
#' @noRd
is_html_renderable <- function(x) {
  if (inherits(x, c("shiny.tag", "shiny.tag.list", "html", "htmlwidget"))) {
    return(TRUE)
  }
  # A dedicated as.tags method on a non-base class (gt_tbl, ...).
  # The base-class exclusion drops htmltools' own eager
  # as.tags.character/.list/... so plain strings/lists/data frames don't get
  # mistaken for HTML.
  # "default" is on the list for an object that carries it as a class: it
  # would otherwise match htmltools' own as.tags.default.
  base_classes <- c(
    "list", "character", "numeric", "integer", "double", "logical", "complex",
    "factor", "data.frame", "function", "NULL", "environment", "name", "call",
    "default"
  )
  cls <- setdiff(class(x), base_classes)
  # Only methods registered for the generic count. getS3method() would also
  # find any function called as.tags.<class> in an attached package or
  # environment.
  registered <- asNamespace("htmltools")[[".__S3MethodsTable__."]]
  any(vapply(
    cls,
    function(cl) {
      method <- paste0("as.tags.", cl)
      # Registered method (an installed package such as gt, or a load_all'd one)
      exists(method, envir = registered, inherits = FALSE) ||
        # Method sourced into the global env. An app deployed as a bundle may
        # `source()` its methods there. Only the global env itself is
        # searched, not the packages attached behind it.
        exists(method, envir = globalenv(), mode = "function", inherits = FALSE)
    },
    logical(1)
  ))
}

#' House styling for a gt table, applied at RENDER time
#'
#' Three properties, and all of them are about the app's chrome rather than
#' about the table. gt sizes itself to its content, centres itself
#' (`margin-left: auto`) and ships its own font stack (`system-ui, 'Segoe UI',
#' Roboto, ...`), so a gt table lands in a blockr panel looking like a visitor:
#' a narrow slab adrift in the middle of a card, set in a different typeface
#' from everything around it. A panel is a box the user sized on purpose, so
#' the table takes the width it was given; `"inherit"` hands the type back to
#' the panel's CSS, which is the board's theme.
#'
#' Width does the work that alignment only half did: at `pct(100)` the table
#' fills the card and there is no slack left to align within. `table.align`
#' stays anyway, for the case where a caption or a narrow column keeps the
#' table under full width.
#'
#' DELIBERATELY NOT IN THE BLOCK'S SCRIPT. A code block's script is the code it
#' EXPORTS, so `gt::tab_options()` written there would travel into the reader's
#' document and impose our house style on theirs. Same reason `as_gt()` is
#' here: how a thing draws in our app is our business, and the exported call
#' should stay the call a statistician would have typed.
#'
#' The cost, named: this runs on the finished object, so a script that sets
#' width, alignment or a font on purpose is overridden. Those three are the
#' only properties taken; colours, borders, spanners, footnotes and the rest
#' of gt are left exactly as the author left them.
#'
#' @param x A `gt_tbl`.
#' @return The `gt_tbl`, restyled; unchanged if gt is unavailable.
#' @noRd
gt_house_style <- function(x) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(x)
  }
  tryCatch(
    gt::tab_options(
      x,
      table.width = gt::pct(100),
      table.align = "left",
      table.font.names = "inherit",
      table.font.size = "inherit"
    ),
    error = function(e) x
  )
}

#' Render any R object dynamically based on its type
#'
#' Detects the type of result and renders appropriately:
#' - `NULL`: nothing
#' - data frame: blockr.ui's HTML table
#' - HTML-renderable (gt, htmlwidgets, `html_page()`, raw tags, an HTML
#'   `knitr::kable()`): its `as.tags()` HTML
#' - ggplot, lattice (`trellis`), grid grobs and gtables, recorded base plots:
#'   a `plotOutput`
#' - other: `print()` as preformatted text
#'
#' @param result The R object to render
#' @param block The block object
#' @param session Shiny session
#' @return A shiny.render.function (renderUI)
#' @noRd
render_dynamic_output <- function(result, block, session) {
  # A NULL RESULT PAINTS NOTHING. Everything below ends at `print()` as
  # preformatted text for an object no branch claims, and printing NULL puts
  # the word NULL on the card.
  #
  # It is not a result the user wrote. blockr.core hands one down whenever the
  # block is momentarily outside the eval set -- `res()` returns NULL early
  # while `block_ready()`, which gates the render observer, does not test
  # `needed()` (blockr.core R/block-server.R). A view switch lands a block's
  # visibility across several flushes, so a block already on screen takes that
  # round trip and its output is rendered from the NULL. On a slow connection
  # the two paints arrive as two messages and the table is preceded by the
  # word NULL for a fraction of a second.
  #
  # A block that genuinely returns NULL paints nothing too, which is the same
  # thing the table block does with it (an empty frame, drawn empty).
  if (is.null(result)) {
    return(shiny::renderUI(NULL))
  }

  # A GTSUMMARY TABLE IS A DESCRIPTION OF A TABLE, NOT A RENDERED ONE. Its
  # HTML comes from gt, reached through `gtsummary::as_gt()`, so it has no
  # `as.tags` method of its own and would otherwise fall through to
  # `print()` as preformatted text.
  #
  # Converted HERE and not in the block's script on purpose: the script a
  # code block holds is also the code it EXPORTS, so a wrapper written only
  # to satisfy this renderer would travel into the exported document with
  # it. `gtsummary::tbl_summary(data, by = AREA)` is the line a statistician
  # writes and the line the report should carry; making it draw is this
  # function's job, not the author's.
  if (inherits(result, "gtsummary") &&
        requireNamespace("gtsummary", quietly = TRUE)) {
    result <- tryCatch(gtsummary::as_gt(result), error = function(e) result)
  }

  # Any gt table, however it got here -- a gtsummary above, a `gt::gt()` in a
  # code block, a sibling package's builder.
  if (inherits(result, "gt_tbl")) {
    result <- gt_house_style(result)
  }

  # Data frames always draw as blockr.ui's HTML table. This function is the
  # render half of a fixed `uiOutput` (see block_ui.script_block), and that
  # table's renderer is a renderUI, so it fits the container whatever
  # `tabular_display` the board sets.
  if (inherits(result, "data.frame")) {
    return(blockr.ui::html_table_result(result, block, session))
  }

  # knitr::kable(format = "html") is a character vector of markup with no
  # as.tags method of its own.
  if (inherits(result, "knitr_kable") &&
        identical(attr(result, "format"), "html")) {
    result <- htmltools::HTML(paste(result, collapse = "\n"))
  }

  # Both plot branches use this one output id. Assigning the output again on
  # each render is fine: the plot is rebuilt once per render.
  plot_output <- function(draw, ...) {
    session$output[["plot_output"]] <- shiny::renderPlot(
      draw(), ..., alt = "Plot drawn by the script"
    )
    shiny::plotOutput(session$ns("plot_output"))
  }

  shiny::renderUI({
    if (is_html_renderable(result)) {
      # Ask the object for its HTML (gt, htmlwidgets, html_page(), ...). Fall
      # back to text if the contract unexpectedly errors.
      tryCatch(
        htmltools::as.tags(result),
        error = function(e) text_output(result)
      )
    } else if (inherits(result, "ggplot")) {
      plot_output(function() print(result), bg = "transparent")
    } else if (inherits(result, "recordedplot")) {
      plot_output(function() evaluate::replay(result))
    } else if (inherits(result, "trellis")) {
      # A lattice plot draws when printed. Without a device of its own it
      # would go to Rplots.pdf and show nothing.
      plot_output(function() print(result))
    } else if (inherits(result, c("grob", "gtable", "gTree")) &&
                 requireNamespace("grid", quietly = TRUE)) {
      plot_output(function() {
        grid::grid.newpage()
        grid::grid.draw(result)
      })
    } else {
      # Fallback: print method as preformatted text
      text_output(result)
    }
  })
}

# A result as its printed text. The style is a class in script-block.css,
# which travels with the tag.
text_output <- function(result) {
  htmltools::tagList(
    script_css_dep(),
    shiny::pre(
      class = "blockr-script-text-output",
      paste(utils::capture.output(print(result)), collapse = "\n")
    )
  )
}
