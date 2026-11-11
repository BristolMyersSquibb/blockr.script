#' Script block
#'
#' A block whose state is an ordinary R script. The script reads its inputs by
#' name and returns whatever its last statement evaluates to: a data frame for
#' the blocks downstream, a ggplot, a gt table, or any other object.
#'
#' @section Inputs:
#' The block takes any number of inputs, including none.
#'
#' * The first link into the block is `data`. A script written for one input
#'   reads `data`, as it would in a function.
#' * Every link also reads by its own name: the link name, or `.arg1`,
#'   `.arg2`, ... for a link that has none. Only the links without a name are
#'   counted, so next to one named link the first unnamed one is `.arg1`.
#'   Links are named where they are drawn: "Edit link" or "Edit inputs" in
#'   the dock.
#' * The block is variadic (`function(id, ...args)`), so the order of the
#'   links decides which one is `data`. A link that is removed and drawn
#'   again comes last, and `data` is then another input. A script with more
#'   than one input should read each by its own name, which does not move.
#'   A link named `data` is always `data`.
#' * With no input connected, `data` is `NULL` and the script is a source: it
#'   reads a file, calls an API or builds a data frame itself.
#'
#' On export each input name is replaced by the id of the upstream block, so
#' the exported code reads like a script someone wrote by hand.
#'
#' @section Controls:
#' A top-level assignment whose right-hand side is a **plain value** becomes a
#' control on the block, meaning a literal, a `c()` of literals, or a call to
#' `factor()` or `as.Date()`. Everything else is code.
#'
#' ```r
#' .vars <- intersect(c("AGE", "SEX", "TRT"), names(data))  # private, no control
#'
#' vars <- factor(.vars, levels = .vars)                # a multi-select
#' species <- factor("setosa", levels(data$Species))    # a dropdown
#' n <- 10                                              # a number box
#' desc <- TRUE                                         # a checkbox
#'
#' data |>
#'   dplyr::filter(Species %in% species) |>
#'   dplyr::slice_head(n = n)
#' ```
#'
#' A **factor** is how a select is expressed: its levels are the choice list,
#' and the length of its value decides single or multiple. For a text column
#' write `sort(unique(data$col))` as the levels. `unique()` alone follows the
#' order of the rows, and the control is drawn again whenever that changes.
#' A pick whose level is no longer in the data is not used: the control goes
#' back to the value the script wrote.
#'
#' A name starting with a dot is never a control. A name the script assigns
#' again is not one either, since only the last assignment could be.
#'
#' The script reads a control as its current value, written into the code as
#' a literal. Three things follow, all by design:
#'
#' * A select's pick is a string (a character vector for a multi-select),
#'   not a factor. `levels(x)` or `as.integer(x)` on a control do not give
#'   what they give for the factor on the declaration line.
#' * Every read of the control's name is replaced, also inside data-masking
#'   calls. With a control `n`, `dplyr::filter(n > 5)` compares the control
#'   and not a column called `n`. Write `.data$n` for the column. `.env$n`
#'   does not reach the control: its line is no longer in the code that runs.
#' * A later declaration reads an earlier control at its current value, so
#'   the choices of one dropdown can depend on the pick in another:
#'   `col <- factor("SEX", names(data))` followed by
#'   `val <- factor(character(0), sort(unique(data[[col]])))`.
#'
#' The optional `#|` annotation carries what a value cannot say, such as a
#' numeric range or a label: `n <- 10  #| number(min = 1, max = 50)`.
#'
#' @section Sentence:
#' `#| title:`, `#| subtitle:` and `#| caption:` lines give the block a
#' sentence whose words are its controls, as blockr.viz's blocks do:
#'
#' ```r
#' arm <- factor("Placebo", levels = sort(unique(data$TRT01A)))
#' top_n <- 8  #| number(min = 3, max = 15)
#' serious <- FALSE  #| label = "serious events only"
#' #| title: Top {@top_n} adverse events in {@arm}
#' #| subtitle: Subjects per term[, {@serious}]
#' ```
#'
#' `{@arg}` prints a control's value and makes the word clickable: a select
#' opens its list, a number or text becomes a field, a checkbox toggles. A
#' clause in `[ ... ]` leaves when its value is empty, and its control is
#' offered as a `+ label` chip instead. `{n}`, `{label(col)}` and `{col}` read
#' the first input. The sentence sits in the row with the gear; the controls
#' it names leave the grid, the others stay. A date can be named but stays a
#' control in the grid. The sentence is shown above any result and is not
#' part of the exported code.
#'
#' @section Exported code:
#' The current values are written into the body as literals, so the exported
#' code has no assignment left for a control.
#'
#' blockr.core fills the input names in with `bquote()`, which also evaluates
#' every `.()` and `..()` the script calls itself, such as data.table's
#' `.(x)`. Write `list(x)` there. The editor footer names the line.
#'
#' @section Saved as lines:
#' A saved board writes the script as a JSON array, one element per line, so a
#' change to the script shows up in a git diff as the lines that changed.
#'
#' @param script R code, either a single string with embedded newlines or a
#'   character vector of lines, which is joined with `\n`.
#' @param values Named list of current control values, restored with a saved
#'   board. Values for names the script no longer declares are dropped.
#'
#' @details
#' A script that does not parse, or that has no statement returning a
#' result, still gives a block, with a warning. The block then shows that
#' reason as its error. A saved board that holds such a script loads, and the
#' script can be repaired in the editor.
#' @param ... Additional arguments passed to [blockr.core::new_block()].
#'
#' @return A block of class `script_block`.
#'
#' @examples
#' blk <- new_script_block("n <- 6\n\nutils::head(data, n)")
#'
#' # the same script, one element per line
#' blk <- new_script_block(c("n <- 6", "", "utils::head(data, n)"))
#'
#' # no input: the script is the source
#' blk <- new_script_block("data.frame(x = 1:3, y = c(\"a\", \"b\", \"c\"))")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_board(
#'       blocks = c(
#'         a = new_dataset_block("band_members", package = "dplyr"),
#'         b = new_dataset_block("band_instruments", package = "dplyr"),
#'         s = new_script_block("merge(data, instruments, by = \"name\")")
#'       ),
#'       links = links(
#'         from = c("a", "b"),
#'         to = c("s", "s"),
#'         input = c("data", "instruments")
#'       )
#'     )
#'   )
#' }
#'
#' @export
new_script_block <- function(script = "n <- 6\n\nutils::head(data, n)",
                             values = list(),
                             ...) {
  force(script)
  force(values)
  stopifnot(is.character(script))
  script <- cb_script_text(script)
  stopifnot(length(script) == 1L)
  stopifnot(is.list(values))

  # Say so at board build. The block is still built: a saved board holding a
  # broken script has to load, and the block shows the reason as its error.
  problem <- cb_problem(cb_parse(script))
  if (!is.null(problem)) {
    warning(problem, call. = FALSE)
  }

  blockr.core::new_block(
    server = function(id, ...args) {
      # Purely variadic: see the input naming rule in R/args.R. A direct
      # block_server() call (a test, a block served on its own) may pass
      # nothing.
      if (missing(...args)) {
        ...args <- list()
      }
      shiny::moduleServer(
        id,
        function(input, output, session) {
          r_script <- shiny::reactiveVal(script)
          r_values <- shiny::reactiveVal(values)

          # `script` is externally controllable, so a write can arrive from the
          # assistant, MCP or a restore as a vector of lines. Collapse it here,
          # where every write passes through.
          shiny::observeEvent(r_script(), {
            if (length(r_script()) != 1L) {
              r_script(cb_script_text(r_script()))
            }
          }, priority = 10L)

          r_parsed <- shiny::reactive(cb_parse(r_script()))

          # The slots, named by what the script reads them as.
          r_refs <- shiny::reactive(sb_slots(...args))

          r_inputs <- shiny::reactive(sb_inputs(...args))

          r_specs <- sb_specs_server(r_parsed, r_inputs, r_values)

          r_slot_args <- sb_sentence_server(
            input, output, session, r_parsed, r_specs, r_values, r_inputs
          )

          sb_values_server(input, output, session, r_specs, r_values, r_slot_args)

          sb_editor_server(
            input, output, session, r_script, r_parsed, r_specs, r_refs,
            r_inputs
          )

          r_expr <- sb_expr_server(r_parsed, r_specs, r_values, r_refs)

          list(
            expr = shiny::reactive({
              # Not `req(r_expr())`: a script may be the constant `FALSE`.
              expr <- r_expr()
              shiny::req(!is.null(expr))
              expr
            }),
            state = list(
              script = r_script,
              values = r_values
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        sb_select_deps(),
        sentence_dep(),
        script_tools_dep(),
        # On the page before the first result: it holds the handler for the
        # document an `html_page()` result sends.
        html_page_dep(),
        shiny::div(
          class = "blockr-script-container",
          gear_editor_ui(
            ns, script,
            head = shiny::uiOutput(ns("sentence"), class = "blockr-script-sentence-out"),
            strip = shiny::uiOutput(ns("inputs_ui"),
                                    class = "blockr-script-head__inputs"),
            label = NULL,
            marks = cb_syntactic_marks(script)
          ),
          shiny::div(
            class = "blockr-script-params",
            shiny::uiOutput(ns("dynamic_params"))
          )
        )
      )
    },
    class = "script_block",
    expr_type = "bquoted",
    allow_empty_state = list(input = TRUE, data = list(...args = 0L)),
    external_ctrl = "script",
    ...
  )
}


# The server of the block, in four parts. Each takes the reactives it reads
# and returns the one it adds. All of them run inside the block's module, so
# their observers belong to its session.

#' The control specs, following the script and the live inputs
#'
#' Declarations are evaluated against the live inputs, so a factor's levels
#' follow the data. An input cannot always be read: an upstream block is
#' unset, or this block's panel is hidden, where the visibility gate makes
#' `data()` throw. The last value read stands in then. Specs computed against
#' a missing input would drop the controls that read it, the expression would
#' lose their values, and the block would run again on every tab switch.
#'
#' Until every input has been read once there are no specs and no
#' expression. A restored board would otherwise publish one expression
#' without its stored values and a second one when the data arrives.
#'
#' @param r_parsed,r_inputs,r_values Reactives: the parsed script, the
#'   inputs by name, the stored control values.
#' @return A `reactiveVal` holding the specs, `NULL` until the inputs have
#'   been read.
#' @noRd
sb_specs_server <- function(r_parsed, r_inputs, r_values) {
  r_specs <- shiny::reactiveVal(NULL)
  last_read <- new.env(parent = emptyenv())

  shiny::observe({
    inputs <- sb_last_read(r_inputs(), last_read)
    if (is.null(inputs)) {
      return()
    }
    # The values are read because a later declaration may depend on the pick
    # in an earlier control.
    specs <- cb_specs(r_parsed(), inputs = inputs, values = r_values())
    if (!identical(specs, shiny::isolate(r_specs()))) {
      r_specs(specs)
    }
  })

  r_specs
}

#' The sentence: the script's `#| title:` lines, with words that are controls
#'
#' @return A reactive with the names of the controls the sentence took over.
#'   Those leave the grid.
#' @noRd
sb_sentence_server <- function(input, output, session, r_parsed, r_specs,
                               r_values, r_inputs) {
  ns <- session$ns

  r_templates <- shiny::reactive(sb_sentence_templates(r_parsed()$comments))
  r_slot_args <- shiny::reactive(sb_slot_arg_names(r_specs(), r_templates()))

  # A pick made on a word: `{arg, value}`, written to `values` the way the
  # grid would have written it.
  shiny::observeEvent(input$slot, {
    vals <- sb_apply_slot(input$slot, r_specs(), r_values())
    if (!identical(vals, r_values())) {
      r_values(vals)
    }
  })

  # The data tokens ({n}, {label(col)}) read the first input.
  r_first <- shiny::reactive({
    x <- r_inputs()[["data"]]
    if (sb_is_unavailable(x)) NULL else x
  })

  output$sentence <- shiny::renderUI(
    sb_sentence_ui(r_templates(), r_specs(), r_values(), r_first(), ns("slot"))
  )

  r_slot_args
}

#' The grid of controls, and collecting what they report into `r_values`
#' @noRd
sb_values_server <- function(input, output, session, r_specs, r_values,
                             r_slot_args) {
  ns <- session$ns

  # Seeded from the current values, so a restored board comes back with its
  # controls where the user left them, but not reactive to them, or every
  # change would rebuild the UI.
  output$dynamic_params <- shiny::renderUI({
    specs <- Filter(function(s) !s$name %in% r_slot_args(), r_specs())
    cb_params_ui(specs, ns, shiny::isolate(r_values()))
  })

  shiny::observe({
    specs <- r_specs()
    # No specs yet is not "no controls": the stored values stay.
    if (is.null(specs)) {
      return()
    }
    taken <- r_slot_args()
    vals <- shiny::isolate(r_values())
    for (s in specs) {
      # A word's control has no widget; a stale input from before the
      # sentence named it must not overwrite the word's pick.
      if (!cb_spec_ok(s) || s$name %in% taken) {
        next
      }
      raw <- input[[cb_input_id(s$name)]]
      if (!is.null(raw)) {
        vals <- sb_collect_value(vals, s, raw)
      }
    }
    declared <- vapply(specs, `[[`, character(1L), "name")
    vals <- vals[intersect(names(vals), declared)]
    # An emptied list would keep an empty `names` attribute, which is not
    # identical to `list()` and is saved as `{}`.
    if (!length(vals)) {
      vals <- list()
    }
    if (!identical(vals, shiny::isolate(r_values()))) {
      r_values(vals)
    }
  })

  invisible(NULL)
}

#' Everything in the gear tray: marks, chips, warnings, completions, footer
#'
#' Nothing here is built until the tray has been opened once: a block set up
#' by the assistant, whose tray nobody opens, costs nothing. gear-editor.js
#' sets `tray_opened`, and it stays set when the tray closes.
#'
#' @noRd
sb_editor_server <- function(input, output, session, r_script, r_parsed,
                             r_specs, r_refs, r_inputs) {
  ns <- session$ns

  r_opened <- shiny::reactive(isTRUE(input$tray_opened))

  # The editor's text, which is the script until someone types.
  r_live <- shiny::reactive({
    live <- input$fn_code
    if (is.null(live)) r_script() else live
  })

  # Tell the editor which lines became controls. While the text is being
  # edited the marks are a syntactic guess (classifying a keystroke must not
  # evaluate anything); once the text is the committed script the real specs
  # settle them. Until the tray has been opened the editor paints the marks
  # written into its markup.
  shiny::observe({
    shiny::req(r_opened())
    live <- r_live()
    specs <- r_specs()
    marks <- if (!is.null(specs) &&
                   identical(trimws(live), trimws(r_script()))) {
      cb_editor_marks(specs)
    } else {
      cb_syntactic_marks(live)
    }
    session$sendCustomMessage(
      "blockr-script-inputs",
      list(id = ns("fn_code"), marks = marks)
    )
  })

  # What the chips and the completions show. The tray stays "opened" after
  # it closes, so this runs on every change of an input. The result is split:
  # `r_shape` holds the names, columns and types and changes only when they
  # do, `r_dims` holds the sizes. New rows in an input then update one text
  # per chip and nothing else.
  r_shape <- shiny::reactiveVal(NULL)
  r_dims <- shiny::reactiveVal(character())

  shiny::observe({
    shiny::req(r_opened())
    info <- sb_split_info(sb_input_info(r_inputs()), shiny::isolate(r_shape()))
    if (!identical(info$shape, shiny::isolate(r_shape()))) {
      r_shape(info$shape)
    }
    if (!identical(info$dims, shiny::isolate(r_dims()))) {
      r_dims(info$dims)
    }
  })

  r_info <- shiny::reactive({
    shiny::req(r_opened(), !is.null(r_shape()))
    r_shape()
  })

  # One text output per chip for its size, made when a chip of that position
  # first exists.
  dims_made <- new.env(parent = emptyenv())
  dims_made$n <- 0L
  shiny::observe({
    while (dims_made$n < length(r_dims())) {
      dims_made$n <- dims_made$n + 1L
      sb_chip_dims_output(output, dims_made$n, r_dims)
    }
  })

  # "+ Control" offers a title sentence that names the first control.
  r_control <- sb_distinct(shiny::reactive(sb_first_control(r_specs())))

  # The row above the editor: input chips, "+ Control", "Start from".
  output$inputs_ui <- shiny::renderUI(
    sb_inputs_ui(
      r_info(), ns("fn_code"),
      control = r_control(),
      dims = shiny::isolate(r_dims()),
      dims_id = function(i) ns(sb_chip_dims_id(i))
    )
  )
  shiny::outputOptions(output, "inputs_ui", suspendWhenHidden = FALSE)

  # A name the script reads that is no input but a data frame from a package,
  # such as `cars`: underlined in the editor and named in the footer. The
  # footer reads the script and the underline the editor's text. While the
  # two are the same, one scan serves both.
  r_names <- shiny::reactive(unique(c("data", names(r_refs()))))
  r_columns <- shiny::reactive(sb_info_names(r_info()))
  r_foreign <- shiny::reactive(
    sb_foreign_frames(r_script(), r_names(), r_columns())
  )

  shiny::observe({
    shiny::req(r_opened())
    live <- r_live()
    marks <- if (identical(live, r_script())) {
      r_foreign()
    } else {
      sb_foreign_frames(live, r_names(), r_columns())
    }
    session$sendCustomMessage(
      "blockr-script-warnings",
      list(id = ns("fn_code"), marks = marks)
    )
  })

  # Run and Cmd-Enter commit the editor's text as the script, unless it does
  # not parse or has no result.
  setup_code_editor_server(
    input, output, session, r_script,
    cols = shiny::reactive(sb_completions(r_info())),
    rest_label = shiny::reactive(
      sb_rest_label(
        r_specs(), r_parsed(), length(r_info()), r_foreign(), names(r_refs())
      )
    ),
    active = r_opened,
    problem = function(code) cb_problem(cb_parse(code), brief = TRUE)
  )
}

#' The block's expression, published only when it changed
#'
#' The specs follow the live data, so they are recomputed whenever an
#' upstream reactive fires, and core evaluates the block on every firing of
#' `expr`.
#'
#' A script that cannot be used can still reach the state: the assistant
#' writes one, or a saved board holds one. Its expression stops with the
#' reason, so the block shows that and not the result of a script it no
#' longer has.
#'
#' @return A `reactiveVal` holding the expression, `NULL` until there is one.
#' @noRd
sb_expr_server <- function(r_parsed, r_specs, r_values, r_refs) {
  r_expr <- shiny::reactiveVal(NULL)

  shiny::observe({
    parsed <- r_parsed()
    problem <- cb_problem(parsed)
    expr <- if (is.null(problem)) {
      specs <- r_specs()
      shiny::req(!is.null(specs))
      cb_expr(parsed, specs, r_values(), data_name = r_refs())
    } else {
      cb_stop_expr(problem)
    }
    if (!identical(expr, shiny::isolate(r_expr()))) {
      r_expr(expr)
    }
  })

  r_expr
}


#' The row above the editor: input chips, "+ Control" and "Start from"
#'
#' A chip opens a searchable list of what its input holds (columns, a dm's
#' tables, a list's elements); a pick inserts it at the cursor. The chip of
#' the first input is called `data` and also shows that input's own name.
#' "+ Control" inserts a ready line that becomes a control, and says the rule
#' in one line. Built only once the tray has been opened.
#'
#' @param info The inputs, from `sb_input_info()`.
#' @param editor_id The namespaced id of the editor.
#' @param control Name of the script's first control, for the title sentence
#'   "+ Control" offers.
#' @param dims The size each chip shows, one string per input. Defaults to
#'   the `dims` in `info`.
#' @param dims_id A function of the chip's position giving the id of a text
#'   output. With it the size is that output, so it can change without the
#'   chips being built again.
#' @noRd
sb_inputs_ui <- function(info, editor_id, control = NULL, dims = NULL,
                         dims_id = NULL) {
  chips <- if (length(info)) {
    lapply(seq_along(info), function(i) {
      x <- info[[i]]
      size <- if (i <= length(dims)) dims[[i]] else x$dims
      shiny::tags$button(
        type = "button",
        class = "blockr-script-chip",
        `data-editor` = editor_id,
        `data-insert` = sb_symbol(x$name),
        `data-cols` = as.character(
          jsonlite::toJSON(x$cols, auto_unbox = TRUE)
        ),
        `data-what` = x$what,
        # A chip with something to search opens a list. One without inserts
        # its name.
        `aria-haspopup` = if (nzchar(x$what)) "menu",
        title = if (nzchar(x$what)) paste("Search", x$what) else "Insert the input",
        shiny::span(class = "blockr-script-chip__name", x$name),
        if (!is.null(x$alias)) {
          shiny::span(
            class = "blockr-script-chip__alias",
            title = sprintf(
              paste(
                "The first input reads as data. It also reads as %s, which",
                "stays this input when the links change."
              ),
              x$alias
            ),
            paste("=", x$alias)
          )
        },
        shiny::span(
          id = if (!is.null(dims_id)) dims_id(i),
          class = paste(
            c("blockr-script-chip__dims",
              if (!is.null(dims_id)) "shiny-text-output"),
            collapse = " "
          ),
          size
        )
      )
    })
  } else {
    shiny::span(
      class = "blockr-script-inputs__empty",
      "none \u00b7 the script is the source"
    )
  }
  shiny::div(
    class = "blockr-script-inputs",
    chips,
    shiny::span(
      class = "blockr-script-tools",
      shiny::tags$button(
        type = "button",
        class = "blockr-script-tool blockr-script-add",
        `aria-haspopup` = "menu",
        `data-editor` = editor_id,
        `data-templates` = as.character(
          jsonlite::toJSON(sb_control_templates(info, control), auto_unbox = TRUE)
        ),
        `data-note` = paste(
          "A line at the top level that assigns a plain value becomes a",
          "control; its glyph says which. A name starting with a dot stays",
          "code."
        ),
        "+ Control"
      ),
      shiny::tags$button(
        type = "button",
        class = "blockr-script-tool blockr-script-start",
        `aria-haspopup` = "menu",
        `data-editor` = editor_id,
        `data-starters` = as.character(
          jsonlite::toJSON(sb_starters(), auto_unbox = TRUE)
        ),
        `data-note` = "Replaces the script. Undo brings it back.",
        "Start from"
      ),
    )
  )
}

# The id, without namespace, of the text output that holds a chip's size.
sb_chip_dims_id <- function(i) {
  paste0("chip_dims_", i)
}

# Define that output. It lives in the gear tray, which is opened from JS, so
# it must not be suspended while hidden (see the footer in code-editor.R).
sb_chip_dims_output <- function(output, i, r_dims) {
  id <- sb_chip_dims_id(i)
  output[[id]] <- shiny::renderText({
    dims <- r_dims()
    if (i <= length(dims)) dims[[i]] else ""
  })
  shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
}

#' Split what the chips show into what rarely changes and what often does
#'
#' @param info From `sb_input_info()`.
#' @param prev The `shape` of the last call, or `NULL`.
#' @return `shape`, the info without the sizes, and `dims`, the sizes. An
#'   input that cannot be read right now keeps the shape it had, so hiding a
#'   panel does not empty the chip's list and the completions.
#' @noRd
sb_split_info <- function(info, prev = NULL) {
  dims <- vapply(info, `[[`, character(1L), "dims")
  before <- vapply(prev, `[[`, character(1L), "name")
  shape <- lapply(info, function(x) {
    old <- match(x$name, before)
    if (identical(x$dims, "waiting") && !is.na(old)) {
      return(prev[[old]])
    }
    x$dims <- NULL
    x
  })
  list(shape = shape, dims = dims)
}

# Every name the chips list: columns, a dm's tables, a list's elements.
sb_info_names <- function(info) {
  unique(unlist(lapply(info, function(x) {
    vapply(x$cols, `[[`, character(1L), "name")
  })))
}

# The name of the script's first control, or NULL.
sb_first_control <- function(specs) {
  ok <- Filter(cb_spec_ok, specs)
  if (length(ok)) ok[[1L]]$name
}

#' A reactive that changes only when its value does
#'
#' A `reactive()` invalidates its readers whenever it is recomputed, also
#' when the result is the same. This holds the value in a `reactiveVal` that
#' is only written when the value differs.
#'
#' @param r A reactive.
#' @return A `reactiveVal`, `NULL` until `r` has a value.
#' @noRd
sb_distinct <- function(r) {
  val <- shiny::reactiveVal(NULL)
  shiny::observe({
    x <- r()
    if (!identical(x, shiny::isolate(val()))) {
      val(x)
    }
  })
  val
}

#' Take one control's reported value into the values list
#'
#' @param vals The stored values.
#' @param spec The control's spec.
#' @param raw What the control's input holds.
#' @return `vals`, changed only when the report is a change. Every change of
#'   the values is a change of the block's state, and core evaluates the
#'   block again for each.
#' @noRd
sb_collect_value <- function(vals, spec, raw) {
  coerced <- cb_coerce(raw, spec)
  if (is.null(coerced)) {
    return(vals)
  }
  stored <- cb_coerce(vals[[spec$name]], spec)
  if (is.null(stored)) {
    if (!identical(coerced, spec$default)) {
      vals[[spec$name]] <- coerced
    } else if (!is.null(vals[[spec$name]])) {
      # The control reports the script's own value while a value is stored
      # that can no longer be used: a pick whose level left the data, or a
      # leftover from before the line changed kind. It goes.
      vals[[spec$name]] <- NULL
    }
    # Otherwise the control reports the script's own value and nothing is
    # stored. Writing it would add nothing.
    return(vals)
  }
  # Compared after coercion: JSON may have turned a stored 7 into 7L.
  if (!identical(coerced, stored)) {
    vals[[spec$name]] <- coerced
  }
  vals
}

#' Lines "+ Control" can insert
#'
#' The column-based dropdown reads the first text or factor column of the
#' first input, so it works as inserted. The title sentence names the first
#' control the script has.
#'
#' @noRd
sb_control_templates <- function(info, control = NULL) {
  col <- NULL
  inp <- NULL
  for (x in info) {
    hit <- Filter(
      function(cl) cl$type %in% c("chr", "fct") && is.null(cl$insert),
      x$cols
    )
    if (length(hit)) {
      col <- hit[[1L]]$name
      inp <- if (is.null(hit[[1L]]$of)) sb_symbol(x$name) else hit[[1L]]$of
      break
    }
  }
  out <- list()
  if (!is.null(col)) {
    ref <- if (identical(make.names(col), col)) {
      paste0(inp, "$", col)
    } else {
      paste0(inp, "[[\"", col, "\"]]")
    }
    out[[length(out) + 1L]] <- list(
      label = "Dropdown from a column",
      hint = col,
      line = sprintf(
        "pick <- factor(%s[1], levels = sort(unique(%s)))", ref, ref
      )
    )
  }
  c(out, list(
    list(label = "Dropdown", hint = "fixed choices",
         line = "choice <- factor(\"a\", levels = c(\"a\", \"b\", \"c\"))"),
    list(label = "Multi-select", hint = "several at once",
         line = "choices <- factor(c(\"a\", \"b\"), levels = c(\"a\", \"b\", \"c\"))"),
    list(label = "Number", hint = "with a range",
         line = "n <- 10  #| number(min = 1, max = 100)"),
    list(label = "Checkbox", hint = "TRUE or FALSE",
         line = "flag <- TRUE"),
    list(label = "Text", hint = "free text",
         line = "label <- \"text\""),
    list(label = "Date", hint = "a date picker",
         line = "from <- as.Date(\"2024-01-01\")"),
    sb_title_template(control)
  ))
}

# The sentence lines: a title whose word is one of the script's controls, a
# subtitle with a data token, and a plain caption. Delete what you don't need.
sb_title_template <- function(control = NULL) {
  name <- if (is.null(control)) "name" else control
  line <- paste(
    sprintf("#| title: Results for {@%s}", name),
    "#| subtitle: {n} rows",
    "#| caption: Source: your note here",
    sep = "\n"
  )
  list(
    label = "Title sentence",
    hint = "title, subtitle, caption",
    line = line
  )
}

# A link name need not be a syntactic R name; the script then reads it in
# backticks.
sb_symbol <- function(x) {
  if (identical(make.names(x), x)) {
    x
  } else {
    paste0("`", x, "`")
  }
}


#' The quiet line in the editor footer
#'
#' How many controls and inputs the script has, or the one thing about the
#' script that needs saying: it does not parse, a line that looks like a
#' control is not one, a name is no input.
#'
#' @param specs The control specs, `NULL` while the inputs have not been read.
#' @param parsed The result of `cb_parse()`.
#' @param n_inputs Number of connected inputs.
#' @param foreign From `sb_foreign_frames()`.
#' @param inputs The names the script reads its inputs by.
#' @noRd
sb_rest_label <- function(specs, parsed, n_inputs = 1L, foreign = list(),
                          inputs = character()) {
  problem <- cb_problem(parsed, brief = TRUE)
  if (!is.null(problem)) {
    return(sb_warn(problem))
  }
  if (is.null(specs)) {
    return("waiting for an input")
  }
  bad <- Filter(function(s) !is.null(s$error), specs)
  if (length(bad)) {
    return(sb_warn(paste0(bad[[1L]]$name, ": ", bad[[1L]]$error)))
  }
  if (length(parsed$dots)) {
    return(sb_warn(sprintf(
      paste(
        "line %d calls .(), which blockr evaluates before the script runs;",
        "write list() or another name"
      ),
      parsed$dots[[1L]]
    )))
  }
  if (length(foreign)) {
    return(sb_warn(foreign[[1L]]$title))
  }
  live <- vapply(Filter(cb_spec_ok, specs), `[[`, character(1L), "name")
  clash <- intersect(live, inputs)
  if (length(clash)) {
    return(sb_warn(sprintf(
      "%s is an input and a control; the script reads the control",
      clash[[1L]]
    )))
  }
  demoted <- cb_demoted(parsed)
  reason <- attr(demoted, "reason")
  name <- attr(demoted, "name")
  hit <- which(reason == "assigned")
  if (length(hit)) {
    return(sb_warn(sprintf(
      "%s assigned in the body, so %s stays code",
      paste(unique(name[hit]), collapse = ", "),
      if (length(unique(name[hit])) == 1L) "it" else "they"
    )))
  }
  hit <- which(reason == "redeclared")
  if (length(hit)) {
    return(sb_warn(sprintf(
      "%s declared twice, so only the last one is a control",
      paste(unique(name[hit]), collapse = ", ")
    )))
  }
  plural <- function(n, word) {
    sprintf("%d %s%s", n, word, if (n == 1L) "" else "s")
  }
  n <- length(specs)
  ctrl <- if (n) {
    plural(n, "control")
  } else {
    "no controls \u00b7 assign a plain value to get one"
  }
  paste(ctrl, plural(n_inputs, "input"), sep = " \u00b7 ")
}


#' One string to work in, one line per element to diff
#'
#' `cb_script_text()` joins lines into the working form, `cb_script_lines()`
#' cuts it back into lines. They round-trip exactly, trailing newline
#' included, which is why the split is not a bare `strsplit()`.
#'
#' @noRd
cb_script_text <- function(x) {
  if (length(x) == 1L) {
    return(x)
  }
  paste(x, collapse = "\n")
}

#' @noRd
cb_script_lines <- function(x) {
  x <- cb_script_text(x)
  if (!nzchar(x)) {
    return(x)
  }
  out <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  if (endsWith(x, "\n")) {
    return(c(out, ""))
  }
  out
}

#' Why a script cannot be used, or `NULL` when it can
#'
#' A script has to parse and has to have a statement that returns the
#' result. Controls alone return nothing. Both are read off the parsed
#' script; nothing is evaluated.
#'
#' The same check guards Run, words the constructor's warning and gives the
#' block its error when such a script is its state.
#'
#' @param parsed The result of `cb_parse()`.
#' @param brief One short line for the editor footer.
#' @noRd
cb_problem <- function(parsed, brief = FALSE) {
  if (!parsed$ok) {
    where <- parse_error_brief(parsed$error)
    return(
      if (brief) paste0("does not parse: ", where)
      else paste0("The script does not parse. ", where)
    )
  }
  if (!length(parsed$stmts)) {
    return(if (brief) "the script is empty" else "The script is empty.")
  }
  if (all(vapply(parsed$stmts, `[[`, logical(1L), "input"))) {
    return(
      if (brief) "no result: add a last line that returns one"
      else paste(
        "The script has no result. It holds only controls and needs a last",
        "statement that returns something."
      )
    )
  }
  NULL
}

#' The expression of a script that cannot be used: it stops with the reason
#' @noRd
cb_stop_expr <- function(problem) {
  call("stop", problem, call. = FALSE)
}

# Write the script to a saved board as lines. The constructor takes both
# forms, so a board saved either way loads.
#' @importFrom blockr.core blockr_ser
#' @export
blockr_ser.script_block <- function(x, state = NULL, ...) {
  res <- NextMethod()
  res[["payload"]][["script"]] <- cb_script_lines(res[["payload"]][["script"]])
  res
}

#' @importFrom blockr.core block_eval
#' @export
block_eval.script_block <- function(x, expr, env, ...) {
  eval_with_plot_capture(expr, env)
}

#' @importFrom blockr.core block_ui
#' @export
block_ui.script_block <- function(id, x, ...) {
  shiny::uiOutput(shiny::NS(id, "result"))
}

#' @importFrom blockr.core block_output
#' @export
block_output.script_block <- function(x, result, session) {
  block_result_output(result, x, session)
}

#' A status line that warns: drawn amber in the editor's status bar.
#' @noRd
sb_warn <- function(x) {
  structure(x, kind = "warn")
}
