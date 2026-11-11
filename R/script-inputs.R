#' The controls of a script
#'
#' The script is ordinary R. A top-level assignment whose right-hand side is
#' a plain value (see `CB_VALUE_CALLS`) becomes a control on the block; every
#' other statement is code. A name starting with a dot is never a control,
#' which is how a line that looks like a declaration says it is scaffolding.
#'
#' Both are decided by reading the line, without evaluating anything and
#' without regard to where in the script it sits. There is no fenced region,
#' no marker comment and no header: a control is a kind of line, not a place
#' in the script. The editor paints the lines that became controls (the
#' `blockr-script-inputs` message), which tells the user which lines they are.
#'
#' The `cb_` prefix is shared with the copies of this parser in sibling
#' packages, so the three can be merged later.
#'
#' @name script-inputs
#' @noRd
NULL


#' Calls whose result counts as a plain value
#'
#' Kept short on purpose. Every addition widens what silently becomes a
#' control. `factor()` is the important one: its levels are the choice list,
#' which is how a select is written without any blockr vocabulary. `c()`
#' counts only when every element is a literal, see `cb_is_value_rhs()`.
#'
#' @noRd
CB_VALUE_CALLS <- c("c", "factor", "as.Date")

#' Annotation keys accepted after `#|`
#' @noRd
CB_ANN_KEYS <- c("label", "min", "max", "step", "placeholder")

#' The gutter glyph naming each widget
#'
#' Read off the gutter, these say what the whole UI is without a word of prose.
#' Kept to glyphs that render legibly at gutter size in a UI font. The
#' ballot-box and geometric-shape characters some of these would naturally use
#' fall back to an emoji face or a hairline at 11px.
#'
#' @noRd
CB_GLYPHS <- c(select = "\u25be", number = "#", text = "Aa", flag = "\u2713",
               date = "\u25a4")


#' Is this statement an assignment to a bare name?
#'
#' The shape a declaration has to have, whether or not it turns out to be one.
#'
#' @param e A top-level expression from `parse()`.
#' @noRd
cb_is_assign_stmt <- function(e) {
  if (!is.call(e) || length(e) != 3L) {
    return(FALSE)
  }
  op <- e[[1L]]
  if (!identical(op, quote(`<-`)) && !identical(op, quote(`=`))) {
    return(FALSE)
  }
  is.name(e[[2L]])
}


#' Is this a name the block keeps to itself?
#'
#' The escape hatch, and it is the one R already has: a leading dot means
#' internal. `ls()` hides those names, so does a file manager, and so does
#' this. It is how a line that would otherwise be read as a knob says it is
#' scaffolding for the knobs around it.
#'
#' @param name A variable name.
#' @noRd
cb_is_private_name <- function(name) {
  !is.na(name) && startsWith(name, ".")
}


#' Is this statement an input declaration?
#'
#' @param e A top-level expression from `parse()`.
#' @return `TRUE` for `name <- <plain value>`, the name not starting with a dot.
#' @noRd
cb_is_input_stmt <- function(e) {
  if (!cb_is_assign_stmt(e)) {
    return(FALSE)
  }
  if (cb_is_private_name(as.character(e[[2L]]))) {
    return(FALSE)
  }
  cb_is_value_rhs(e[[3L]])
}


#' Is this a literal: `6`, `"total"`, `TRUE`, `-1`?
#'
#' Length 1 only; a longer atomic constant cannot appear in source without a
#' call to `c()`. `-1` and `+1` parse as unary calls, not as literals.
#'
#' @param x An expression.
#' @noRd
cb_is_literal <- function(x) {
  if (is.atomic(x) && length(x) == 1L) {
    return(TRUE)
  }
  is.call(x) && length(x) == 2L && is.name(x[[1L]]) &&
    as.character(x[[1L]]) %in% c("-", "+") && is.numeric(x[[2L]])
}


#' Is this right-hand side a plain value?
#'
#' Purely syntactic. Errs toward "not a control": `6L * 2` is arithmetic, not
#' a literal, so it stays code. A line then never becomes a widget by
#' surprise; the reverse mistake only costs an explicit edit.
#'
#' `c()` is a value only when every element is a literal. `c(min(x), max(x))`
#' computes something, and `c(data$id)` would ship a whole column to the
#' browser as a choice list.
#'
#' @param rhs The right-hand side expression.
#' @noRd
cb_is_value_rhs <- function(rhs) {
  if (cb_is_literal(rhs)) {
    return(TRUE)
  }
  if (!is.call(rhs)) {
    return(FALSE)
  }
  nm <- cb_call_name(rhs[[1L]])
  if (is.null(nm) || !nm %in% CB_VALUE_CALLS) {
    return(FALSE)
  }
  if (identical(nm, "c")) {
    args <- as.list(rhs)[-1L]
    return(length(args) > 0L && all(vapply(args, cb_is_literal, logical(1L))))
  }
  TRUE
}


#' The bare name of a call head, following a `::` prefix.
#' @noRd
cb_call_name <- function(head) {
  if (is.name(head)) {
    return(as.character(head))
  }
  if (is.call(head) && identical(head[[1L]], quote(`::`))) {
    return(as.character(head[[3L]]))
  }
  NULL
}


#' Split a script into input declarations and body statements
#'
#' @param text The script.
#' @return A list with
#'   * `ok` and `error` (the parse message, or `NULL`);
#'   * `stmts`, one record per top-level statement: `expr`, `line` (its first
#'     line), `input`, `name` and `ann` (the `#|` comment that ends it, or
#'     `NA`);
#'   * `comments`, the `#|` comments that stand on a line of their own, in
#'     script order. The sentence lines are among them;
#'   * `dots`, the first lines of the statements that call `.()` or `..()`.
#' @noRd
cb_parse <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(ok = TRUE, error = NULL, stmts = list(),
                comments = character(), dots = integer()))
  }
  exprs <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(e) e
  )
  if (inherits(exprs, "error")) {
    # No tokens to go by, so the sentence lines are found by text. The block
    # keeps its title while the script is broken.
    lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
    return(list(
      ok = FALSE, error = conditionMessage(exprs), stmts = list(),
      comments = trimws(grep("^\\s*#\\|", lines, value = TRUE)),
      dots = integer()
    ))
  }

  refs <- utils::getSrcref(exprs)
  comments <- cb_comments(exprs, text)
  trailing <- comments[!comments$own, , drop = FALSE]

  stmts <- lapply(seq_along(exprs), function(i) {
    e <- exprs[[i]]
    is_input <- cb_is_input_stmt(e)
    ref <- if (is.null(refs)) NULL else refs[[i]]
    list(
      expr = e,
      line = if (is.null(ref)) NA_integer_ else as.integer(ref)[1L],
      input = is_input,
      name = if (is_input) as.character(e[[2L]]) else NA_character_,
      ann = cb_trailing_comment(i, refs, trailing)
    )
  })

  has_dot <- vapply(exprs, cb_calls_dot, logical(1L))
  list(
    ok = TRUE, error = NULL, stmts = stmts,
    comments = comments$text[comments$own],
    dots = vapply(stmts[has_dot], `[[`, integer(1L), "line")
  )
}


#' The `#|` comments of a parsed script
#'
#' Read off the parser's tokens, not off the text, so a `#|` inside a string
#' is not a comment.
#'
#' @param exprs The result of `parse(keep.source = TRUE)`.
#' @param text The script.
#' @return A data frame with `line`, `col`, `text` and `own`: whether the
#'   comment is the first thing on its line.
#' @noRd
cb_comments <- function(exprs, text) {
  none <- data.frame(
    line = integer(), col = integer(), text = character(), own = logical(),
    stringsAsFactors = FALSE
  )
  # The token table costs more than the parse. Most scripts have no `#|`.
  if (!grepl("#|", text, fixed = TRUE)) {
    return(none)
  }
  pd <- utils::getParseData(exprs)
  if (is.null(pd)) {
    return(none)
  }
  pd <- pd[pd$terminal, , drop = FALSE]
  hit <- pd$token == "COMMENT" & startsWith(pd$text, "#|")
  if (!any(hit)) {
    return(none)
  }
  cm <- pd[hit, , drop = FALSE]
  cm <- cm[order(cm$line1, cm$col1), , drop = FALSE]
  code <- pd[pd$token != "COMMENT", , drop = FALSE]
  own <- vapply(seq_len(nrow(cm)), function(i) {
    line <- cm$line1[[i]]
    !any(code$line1 <= line & code$line2 >= line)
  }, logical(1L))
  data.frame(
    line = cm$line1, col = cm$col1, text = cm$text, own = own,
    stringsAsFactors = FALSE
  )
}


#' The `#|` comment that ends a statement
#'
#' A comment belongs to the statement it follows on the statement's last
#' line. With two statements on a line that is the second one.
#'
#' @param i Index of the statement.
#' @param refs The srcrefs of all statements.
#' @param trailing The comments that share a line with code.
#' @return The comment text, or `NA`.
#' @noRd
cb_trailing_comment <- function(i, refs, trailing) {
  if (is.null(refs) || !nrow(trailing)) {
    return(NA_character_)
  }
  ref <- as.integer(refs[[i]])
  # A srcref is first line, first byte, last line, last byte, first column,
  # last column.
  hit <- which(trailing$line == ref[[3L]] & trailing$col > ref[[6L]])
  if (!length(hit)) {
    return(NA_character_)
  }
  hit <- hit[[1L]]
  # Another statement between this one and the comment owns it.
  for (j in seq_along(refs)) {
    other <- as.integer(refs[[j]])
    if (j != i && other[[3L]] == ref[[3L]] && other[[6L]] > ref[[6L]] &&
          other[[6L]] < trailing$col[[hit]]) {
      return(NA_character_)
    }
  }
  trailing$text[[hit]]
}


#' Does this expression call `.()` or `..()`?
#'
#' blockr.core fills the input slots with `bquote()`, which evaluates every
#' `.()` and splices every `..()` in the expression, also the ones the script
#' wrote itself (data.table's `.(x)`, for one). There is no escape inside
#' `bquote()`, so the editor footer names the line.
#'
#' @param e An expression.
#' @noRd
cb_calls_dot <- function(e) {
  if (is.pairlist(e)) {
    return(any(vapply(as.list(e), cb_calls_dot, logical(1L))))
  }
  if (!is.call(e)) {
    return(FALSE)
  }
  head <- e[[1L]]
  if (is.name(head) && as.character(head) %in% c(".", "..")) {
    return(TRUE)
  }
  for (i in seq_along(e)) {
    if (!cb_is_empty_sym(e[[i]]) && cb_calls_dot(e[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}


#' Read a `#|` annotation
#'
#' The widget kind already follows from the value, so the annotation only
#' carries what a value cannot say: a numeric range, a label, a placeholder.
#' Both `#| number(min = 1, max = 50)` and the bare `#| min = 1, max = 50` are
#' read. The name of the wrapper call does not matter.
#'
#' @param text A `#|` comment, or a line that ends in one.
#' @return A named list of evaluated annotation arguments, possibly empty.
#' @noRd
cb_annotation <- function(text) {
  if (is.null(text) || is.na(text) || !grepl("#|", text, fixed = TRUE)) {
    return(list())
  }
  body <- trimws(sub("^.*?#\\|", "", text, perl = TRUE))
  if (!nzchar(body)) {
    return(list())
  }

  e <- tryCatch(parse(text = body)[[1L]], error = function(e) NULL)
  args <- NULL
  # `label = "x"` parses as a call to `=`, so the head has to be a word.
  if (is.call(e) && is.name(e[[1L]]) &&
        grepl("^[A-Za-z.][A-Za-z0-9._]*$", as.character(e[[1L]]))) {
    args <- as.list(e)[-1L]
  } else {
    e2 <- tryCatch(parse(text = paste0("list(", body, ")"))[[1L]],
                   error = function(e) NULL)
    if (!is.null(e2)) {
      args <- as.list(e2)[-1L]
    }
  }
  if (is.null(args) || !length(args) || is.null(names(args))) {
    return(list())
  }
  args <- args[names(args) %in% CB_ANN_KEYS]
  if (!length(args)) {
    return(list())
  }
  lapply(args, function(a) tryCatch(eval(a, baseenv()), error = function(e) NULL))
}


#' Derive the widget from a declaration's value
#'
#' The three facts a control needs (kind, choice pool, single or multiple)
#' all come from the *declaration*, never from the live value. A multi-select
#' whose user narrows it to one pick must stay a multi-select, so `multiple` is
#' fixed by the script.
#'
#' Exactly one pick declares a single select; anything else declares a
#' multi-select. **Zero is deliberately on the multi side**, so a script can
#' offer a set of choices with nothing chosen yet
#' (`factor(character(0), levels = lv)`). Reading it as a single select instead
#' would leave no way to write "pick any number of these, starting from none":
#' the declaration's length is the only signal available, and a one-element
#' default is not always wanted.
#'
#' @param v The evaluated right-hand side.
#' @return A list with `kind`, and for selects `choices` and `multiple`. A
#'   value that cannot be a control gives a list with `error`, the reason.
#' @noRd
cb_widget_for <- function(v) {
  if (is.factor(v)) {
    if (!nlevels(v)) {
      # An empty factor is almost always a mistyped column name; an empty
      # dropdown would hide that.
      return(list(error = "no choices - is the column name right?"))
    }
    if (anyNA(v)) {
      # The select would show its first choice and report it, which writes
      # the block's state without anyone having picked anything.
      return(list(error = "the value is not one of the choices"))
    }
    return(list(kind = "select", choices = levels(v),
                multiple = length(v) != 1L))
  }
  if (inherits(v, "Date")) {
    if (length(v) != 1L) {
      return(list(error = "a date control holds exactly one date"))
    }
    return(list(kind = "date"))
  }
  if (is.logical(v) && length(v) == 1L && !is.na(v)) {
    return(list(kind = "flag"))
  }
  if (is.numeric(v) && length(v) == 1L) {
    return(list(kind = "number"))
  }
  if (is.character(v) && length(v) == 1L) {
    return(list(kind = "text"))
  }
  # A bare vector is the value, so the pool is exactly that vector. Widen it
  # with a factor.
  if ((is.character(v) || is.numeric(v)) && length(v) > 1L) {
    return(list(kind = "select", choices = as.character(v), multiple = TRUE))
  }
  list(error = paste0("no control for a value of class ",
                      paste(class(v), collapse = "/")))
}


#' Build the control specs for a script
#'
#' Declarations are evaluated in the block's own evaluation environment, so a
#' declaration can reach exactly what the body can reach. The statements
#' around them are handled so that the body is never run:
#'
#' * An assignment that is not a control is bound lazily. A helper line like
#'   `.lv <- unique(data$site)` is there for a later `factor(x, .lv)`, and is
#'   only evaluated if a declaration reads it.
#' * A statement that is not an assignment cannot be bound lazily, so the ones
#'   above the last declaration are run: a preamble that works a column out
#'   with an `if` has to have happened before the declarations under it.
#'   Nothing below the last declaration is touched. That is where the
#'   pipeline lives.
#'
#' Every binding goes into a new environment whose parent is the scope as it
#' was before the line. A lazy helper is evaluated in that parent, so it sees
#' the names as they were where it was written, as it would in plain R, and
#' not what a later line made of them.
#'
#' A later declaration reads an earlier control at its current value, the
#' value the body gets too (`values`). That is what makes one dropdown's
#' choices depend on another's pick.
#'
#' @param parsed The result of `cb_parse()`.
#' @param inputs Named list of every input the declarations may read, by the
#'   name the script uses (`data`, `.arg1`, a link name).
#' @param values Named list of the current control values.
#' @return A list of spec records.
#' @noRd
cb_specs <- function(parsed, inputs = list(), values = list()) {
  if (!parsed$ok || !length(parsed$stmts)) {
    return(list())
  }

  env <- blockr.core::eval_env(inputs)
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  last_decl <- if (any(is_input)) max(which(is_input)) else 0L
  # A declaration the script assigns again is not a control: it is bound like
  # any other helper line. See cb_demoted().
  demoted <- cb_demoted(parsed)
  specs <- list()

  for (i in seq_along(parsed$stmts)) {
    st <- parsed$stmts[[i]]
    if (!st$input || demoted[[i]]) {
      if (cb_is_assign_stmt(st$expr)) {
        env <- cb_delay(env, as.character(st$expr[[2L]]), st$expr[[3L]])
      } else if (i < last_decl) {
        # It fails on its own account when the block runs, so a failure here
        # is not reported twice.
        env <- new.env(parent = env)
        try(suppressWarnings(eval(st$expr, env)), silent = TRUE)
      }
      next
    }

    # A helper that failed is evaluated again by the next declaration that
    # reads it, and R warns about restarting the promise.
    value <- tryCatch(
      suppressWarnings(eval(st$expr[[3L]], env)),
      error = function(e) e
    )
    if (inherits(value, "error")) {
      specs[[length(specs) + 1L]] <- list(
        name = st$name, line = st$line, kind = NA_character_,
        error = conditionMessage(value)
      )
      next
    }

    w <- cb_widget_for(value)
    if (!is.null(w$error)) {
      specs[[length(specs) + 1L]] <- list(
        name = st$name, line = st$line, kind = NA_character_,
        error = w$error
      )
      # The line stays code, so later declarations see what it assigns.
      env <- cb_bind(env, st$name, value)
      next
    }

    ann <- cb_annotation(st$ann)
    spec <- c(
      list(
        name = st$name,
        line = st$line,
        kind = w$kind,
        choices = w$choices,
        multiple = isTRUE(w$multiple),
        default = if (is.factor(value)) as.character(value) else value,
        error = NULL
      ),
      ann[setdiff(names(ann), c("name", "line", "kind", "choices", "multiple",
                                "default", "error"))]
    )
    specs[[length(specs) + 1L]] <- spec
    env <- cb_bind(env, st$name, cb_value(spec, values))
  }

  specs
}


#' Is this spec a control?
#'
#' A declaration whose value could not be read, or that maps to no widget,
#' has a spec that only carries the reason.
#'
#' @param s A spec record.
#' @noRd
cb_spec_ok <- function(s) {
  is.null(s$error) && !is.na(s$kind)
}


#' Bind a name in a new scope on top of `env`
#' @return The new environment.
#' @noRd
cb_bind <- function(env, name, value) {
  new <- new.env(parent = env)
  assign(name, value, envir = new)
  new
}


#' Bind a name to an expression that is evaluated only if something reads it
#'
#' A declaration may depend on a helper line above it, but the statements in
#' between must not run just to build that scope: one of them is usually the
#' block's whole pipeline. The promise is evaluated on first read, in `env`,
#' the scope before the line. So `x <- f(x)` reads the old `x`, and a helper
#' does not see what later lines assign.
#'
#' @return The new environment, which holds the binding.
#' @noRd
cb_delay <- function(env, name, expr) {
  new <- new.env(parent = env)
  do.call(delayedAssign, list(name, expr, env, new))
  new
}


#' Coerce a value coming back from a widget to the declared type
#'
#' Shiny inputs arrive as strings from selects and as lists from JS arrays,
#' and a saved board hands back whatever survived JSON. The declaration knows
#' the type, so it drives the coercion.
#'
#' A value that cannot be read as that type gives `NULL`, and the script's
#' own value then applies. That happens when a line changes kind while an
#' old value is still stored (`d <- "hello"` becoming `d <- as.Date(...)`),
#' and when a pick is no longer among a select's choices because its level
#' left the data.
#'
#' @param value The raw input value.
#' @param spec The spec record it belongs to.
#' @return The value, or `NULL` for none. An emptied multi-select is
#'   `character()`: nothing picked is a pick.
#' @noRd
cb_coerce <- function(value, spec) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.list(value)) {
    value <- unlist(value, use.names = FALSE)
  }
  multi <- identical(spec$kind, "select") && isTRUE(spec$multiple)
  if (is.null(value) || !length(value)) {
    return(if (multi) character() else NULL)
  }
  switch(
    spec$kind,
    number = {
      # Numeric NA ships as the string "NA".
      v <- suppressWarnings(as.numeric(value))
      if (all(is.na(v))) {
        return(NULL)
      }
      # A number box reports a double. `n <- 6L` stays an integer, so the
      # control reporting 6 is the script's own value and not a change.
      if (is.integer(spec$default) && !anyNA(v) && all(v == round(v)) &&
            all(abs(v) < .Machine$integer.max)) {
        v <- as.integer(v)
      }
      v
    },
    flag = {
      v <- suppressWarnings(as.logical(value))[[1L]]
      if (is.na(v)) NULL else v
    },
    date = {
      v <- tryCatch(as.Date(value), error = function(e) NULL)
      if (length(v) != 1L || is.na(v)) NULL else v
    },
    text = {
      v <- as.character(value)
      # `x <- NA_character_` draws an empty box, which reports "".
      if (identical(v, "") && length(spec$default) == 1L &&
            is.na(spec$default)) {
        return(NULL)
      }
      v
    },
    select = {
      out <- as.character(value)
      out <- out[out %in% as.character(spec$choices)]
      if (!length(out)) {
        return(if (multi) character() else NULL)
      }
      # A numeric-valued select round-trips through the DOM as text.
      if (is.numeric(spec$default)) {
        num <- suppressWarnings(as.numeric(out))
        if (!anyNA(num)) out <- num
      }
      out
    },
    value
  )
}


#' The value a control has now
#'
#' The stored value, read as the declared type, or the script's own value
#' when there is none. This is what the expression inlines, what the control
#' shows and what the sentence prints.
#'
#' @param spec A spec record.
#' @param values Named list of stored control values.
#' @noRd
cb_value <- function(spec, values = list()) {
  v <- cb_coerce(values[[spec$name]], spec)
  if (is.null(v)) spec$default else v
}


#' The Shiny input id of a control
#'
#' A control is named by a variable, and a backticked name may hold anything.
#' An id must not: a space breaks `label[for]` and CSS selectors. Characters
#' outside letters, digits, `_` and `.` are written as their code point
#' between dashes, so two names never share an id.
#'
#' @param name The control's name.
#' @return The id, without namespace.
#' @noRd
cb_input_id <- function(name) {
  if (grepl("^[A-Za-z0-9_.]+$", name)) {
    return(paste0("cb_", name))
  }
  chars <- strsplit(name, "", fixed = TRUE)[[1L]]
  bad <- !grepl("^[A-Za-z0-9_.]$", chars)
  chars[bad] <- vapply(
    chars[bad],
    function(ch) sprintf("-%x-", utf8ToInt(ch)),
    character(1L)
  )
  paste0("cb_", paste(chars, collapse = ""))
}


#' A control's label: the `#|` label, else the name made readable
#' @noRd
cb_label <- function(spec) {
  if (!is.null(spec$label)) {
    return(as.character(spec$label)[[1L]])
  }
  label <- gsub("[._]", " ", spec$name)
  paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
}


#' Render one control
#'
#' @param spec A spec record.
#' @param ns Namespace function.
#' @param value The stored value. The control shows what `cb_value()` makes
#'   of it, so it never shows something the expression does not use.
#' @noRd
cb_input_ui <- function(spec, ns, value = NULL) {
  input_id <- ns(cb_input_id(spec$name))
  label <- cb_label(spec)
  value <- cb_value(spec, stats::setNames(list(value), spec$name))

  inner <- switch(
    spec$kind,
    select = sb_select_input(
      input_id = input_id,
      label = label,
      choices = spec$choices,
      selected = as.character(value),
      multiple = spec$multiple
    ),
    # An absent annotation must reach numericInput() as NA, not NULL: it tests
    # its limits with `if (!is.na(min))`, and NULL makes that a length-zero
    # condition.
    number = shiny::numericInput(
      inputId = input_id, label = label, value = value,
      min = cb_or_na(spec$min), max = cb_or_na(spec$max),
      step = cb_or_na(spec$step)
    ),
    text = shiny::textInput(
      inputId = input_id, label = label, value = value,
      placeholder = spec$placeholder
    ),
    flag = shiny::checkboxInput(
      inputId = input_id, label = label, value = isTRUE(value)
    ),
    date = shiny::dateInput(
      inputId = input_id, label = label, value = value
    ),
    NULL
  )
  if (is.null(inner)) {
    return(NULL)
  }
  sb_field_wrapper(spec$kind, inner)
}


#' The params grid holding every control
#' @noRd
cb_params_ui <- function(specs, ns, values = list()) {
  ok <- Filter(cb_spec_ok, specs)
  if (!length(ok)) {
    return(NULL)
  }
  fields <- lapply(ok, function(s) cb_input_ui(s, ns, values[[s$name]]))
  fields <- Filter(Negate(is.null), fields)
  if (!length(fields)) {
    return(NULL)
  }
  # Read the kinds back off the fields that survived, not off `ok`: a spec can
  # produce no control, and the band's track floor has to describe what is
  # actually in it.
  band <- sb_grid_track(vapply(fields, sb_field_kind, character(1L)))
  shiny::div(
    class = paste("blockr-script-params-grid", band$class),
    style = band$style,
    fields
  )
}


#' The per-line marks the editor paints
#'
#' One record per input line: the 1-based line number and a short glyph naming
#' the widget. Pushed to the client as `blockr-script-inputs`.
#'
#' @noRd
cb_editor_marks <- function(specs) {
  marks <- lapply(specs, function(s) {
    if (is.na(s$line)) {
      return(NULL)
    }
    kind <- if (cb_spec_ok(s)) s$kind else NA_character_
    list(
      line = s$line,
      kind = if (is.na(kind)) "error" else kind,
      glyph = if (is.na(kind)) "!" else unname(CB_GLYPHS[[kind]]),
      title = if (!is.null(s$error)) {
        paste0(s$name, ": ", s$error)
      } else {
        cb_mark_title(s)
      }
    )
  })
  unname(Filter(Negate(is.null), marks))
}

#' @noRd
cb_mark_title <- function(s) {
  why <- cb_kind_rule(s$kind)
  what <- switch(
    s$kind,
    select = sprintf(
      "%s, %d choice%s",
      if (isTRUE(s$multiple)) "multi-select" else "dropdown",
      length(s$choices), if (length(s$choices) == 1L) "" else "s"
    ),
    number = "number box",
    text = "text box",
    flag = "checkbox",
    date = "date picker",
    s$kind
  )
  paste0(s$name, ": ", what, ". ", why)
}

# Why a line became the control it did, for the glyph's hover text.
cb_kind_rule <- function(kind) {
  switch(
    kind,
    select = paste(
      "A factor is a dropdown; its levels are the choices, and a value of",
      "length 2 or more makes it a multi-select."
    ),
    number = "A bare number is a number box; #| number(min =, max =) sets a range.",
    text = "A string is a text box.",
    flag = "TRUE or FALSE is a checkbox.",
    date = "as.Date() is a date picker.",
    ""
  )
}


#' Classify input lines without evaluating anything
#'
#' The editor's band and gutter glyphs are pushed on every (debounced)
#' keystroke, so they must not evaluate declarations against the data. The
#' point is that you type a line and watch the glyph appear. The widget kind
#' follows from the *shape* of the right-hand side, which is enough for a
#' glyph; the real specs (choices, multiplicity, defaults) still come from
#' `cb_specs()` on the committed script.
#'
#' Re-assignment is syntactic too, so a declaration the script overwrites
#' further down gets no band either: what the editor paints and what turns into
#' a control are the same set of lines.
#'
#' @param text The script (usually the live editor text).
#' @return The same mark records `cb_editor_marks()` produces.
#' @noRd
cb_syntactic_marks <- function(text) {
  parsed <- cb_parse(text)
  if (!parsed$ok) {
    return(list())
  }
  title <- c(select = "dropdown", number = "number box", text = "text box",
             flag = "checkbox", date = "date picker")
  demoted <- cb_demoted(parsed)
  marks <- lapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    if (!st$input || is.na(st$line) || demoted[[i]]) {
      return(NULL)
    }
    kind <- cb_kind_syntactic(st$expr[[3L]])
    if (is.null(kind)) {
      return(NULL)
    }
    list(line = st$line, kind = kind, glyph = unname(CB_GLYPHS[[kind]]),
         title = paste0(st$name, ": ", unname(title[[kind]]), ". ",
                        cb_kind_rule(kind)))
  })
  unname(Filter(Negate(is.null), marks))
}


#' The widget kind implied by the shape of a right-hand side
#' @noRd
cb_kind_syntactic <- function(rhs) {
  if (is.call(rhs)) {
    nm <- cb_call_name(rhs[[1L]])
    if (!is.null(nm)) {
      if (nm %in% c("-", "+")) {
        return("number")
      }
      # `c(5)` is the number 5.
      if (identical(nm, "c") && length(rhs) == 2L) {
        return(cb_kind_syntactic(rhs[[2L]]))
      }
      return(switch(nm,
        factor = "select",
        c = "select",
        as.Date = "date",
        NULL
      ))
    }
    return(NULL)
  }
  if (is.logical(rhs)) {
    return("flag")
  }
  if (is.numeric(rhs)) {
    return("number")
  }
  if (is.character(rhs)) {
    return("text")
  }
  NULL
}


#' `NA` for an absent annotation value
#' @noRd
cb_or_na <- function(x) {
  if (is.null(x) || !length(x)) NA else x
}
