#' Compiling a script into the block's expression
#'
#' The block does not wrap the user's code in a function and call it. It
#' writes the current value of every control into the body as a literal, puts
#' a `.(slot)` where the script reads an input, and returns that. The line
#' that declared a control is gone from the result, so the exported code is
#' the code an analyst would have written, with the values written in:
#' `dplyr::filter(block_1, Species == "setosa")`.
#'
#' @name script-expr
#' @noRd
NULL


#' Marker left behind by a folded-away `if` with no `else`.
#' @noRd
CB_DROP <- quote(`__cb_drop__`)


#' Is this the empty symbol (a missing argument, as in `x[, 1]`)?
#' @noRd
cb_is_empty_sym <- function(x) {
  is.name(x) && !nzchar(as.character(x))
}


#' Turn a value into an AST node that deparses idiomatically
#'
#' Most values can be inlined as themselves: a character vector deparses to
#' `c("a", "b")`, an integer to `6L`. A date cannot. It would deparse to
#' `structure(20318, class = "Date")`, so it is written as the `as.Date()`
#' call instead. A factor is written as its values, a character vector: the
#' script reads a select's pick as a string.
#'
#' @param v A value.
#' @noRd
cb_literal <- function(v) {
  if (inherits(v, "Date")) {
    return(bquote(as.Date(.(format(v, "%Y-%m-%d")))))
  }
  if (is.factor(v)) {
    return(as.character(v))
  }
  v
}


#' Substitute symbols throughout an expression
#'
#' Only value positions are touched: never a call head, never the right side of
#' `$` or `@`, never an argument name. Those are the positions where a symbol is
#' not a variable reference, and substituting them is how naive source rewriting
#' produces broken code.
#'
#' Scope is respected too. A `function()` formal rebinds the name for that
#' body, so the substitution stops at the boundary; a `for` variable rebinds it
#' in the same environment, so the declaration is demoted to code instead (see
#' `cb_demoted()`).
#'
#' @param e An expression.
#' @param subs A named list of replacement nodes.
#' @noRd
cb_subst <- function(e, subs) {
  if (is.name(e)) {
    nm <- as.character(e)
    if (nzchar(nm) && nm %in% names(subs)) {
      return(subs[[nm]])
    }
    return(e)
  }
  if (!is.call(e)) {
    return(e)
  }
  # `function(x) ...` opens a scope: a formal binds the name for the body, so
  # the control of the same name is not what that body means. Substituting it
  # would rewrite `function(data) nrow(data)` into a reference to the upstream
  # block and quietly return the wrong number. A formal shadows for the
  # defaults too, which R evaluates in the function's own frame.
  if (identical(e[[1L]], quote(`function`)) && length(e) >= 3L) {
    inner <- subs[setdiff(names(subs), names(e[[2L]]))]
    fmls <- e[[2L]]
    for (i in seq_along(fmls)) {
      if (!cb_is_empty_sym(fmls[[i]])) {
        fmls[[i]] <- cb_subst(fmls[[i]], inner)
      }
    }
    # `function()` has NULL formals. `e[[2L]] <- NULL` would delete the
    # element and turn the body into the formals.
    if (!is.null(fmls)) {
      e[[2L]] <- fmls
    }
    e[[3L]] <- cb_subst(e[[3L]], inner)
    # The srcref would otherwise deparse the pre-substitution source.
    if (length(e) >= 4L) {
      e[[4L]] <- NULL
    }
    return(e)
  }
  # `x <- v`: the left side is the name being bound, not a reference to it. A
  # demoted declaration keeps its line in the body, and if a later declaration
  # of the same name gave it a control, substituting here would rewrite the
  # line into `c("a", "b") <- c("a", "b")`. An indexed target (`x[i] <- v`) is
  # a call, and the index inside it IS a value reference, so only a bare name
  # is skipped.
  if (length(e) == 3L && is.name(e[[2L]]) &&
      (identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`)) ||
         identical(e[[1L]], quote(`<<-`)))) {
    e[[3L]] <- cb_subst(e[[3L]], subs)
    return(e)
  }
  # `x$name` / `x@name`: the right side is a literal name, not a variable.
  if (length(e) == 3L &&
      (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`@`)))) {
    e[[2L]] <- cb_subst(e[[2L]], subs)
    return(e)
  }
  # `pkg::name` and `pkg:::name`: both sides are literal names.
  if (identical(e[[1L]], quote(`::`)) || identical(e[[1L]], quote(`:::`))) {
    return(e)
  }
  # A call head that is a name is a function, never an input. A head that is
  # itself a call is an expression like any other: `match.fun(pick)(x)`,
  # `data$f(1)`.
  if (is.call(e[[1L]])) {
    e[[1L]] <- cb_subst(e[[1L]], subs)
  }
  # A substitution that comes back NULL must NOT be assigned into the call:
  # `e[[i]] <- NULL` DELETES the element and shortens `e`, while the loop
  # bounds were fixed before it started. A NULL argument in last position then
  # vanishes from the emitted code, and one anywhere else walks the loop off
  # the end ("subscript out of bounds"), killing the block. Intentional
  # removal has its own sentinel (CB_DROP), so NULL here only ever means "this
  # argument is NULL", which is already what the call says.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      sub <- cb_subst(e[[i]], subs)
      if (is.null(sub)) {
        next
      }
      e[[i]] <- sub
    }
  }
  e
}


#' Fold `if` on a condition that substitution turned into a literal
#'
#' A checkbox is the one input that leaves branching behind: without this,
#' every flag would put `if (TRUE) ...` in the exported script. The condition is
#' known at export time, so the untaken branch is simply dropped. This is the
#' one place the emitted code is not a literal substitution of the source.
#'
#' @param e An expression.
#' @noRd
cb_fold <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  # Same trap as cb_subst(): assigning NULL into a call deletes the element.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      folded <- cb_fold(e[[i]])
      if (is.null(folded)) {
        next
      }
      e[[i]] <- folded
    }
  }

  if (identical(e[[1L]], quote(`if`))) {
    cond <- e[[2L]]
    if (is.logical(cond) && length(cond) == 1L && !is.na(cond)) {
      if (cond) {
        return(e[[3L]])
      }
      if (length(e) >= 4L) {
        return(e[[4L]])
      }
      return(CB_DROP)
    }
  }

  cb_prune(e)
}


#' Remove folded-away operands from the shapes they can appear in
#'
#' `p + if (FALSE) geom_smooth()` should emit `p`, not `p + NULL`; a dropped
#' statement inside `{}` should vanish rather than leave a blank. The last
#' statement is the exception, see `cb_drop_statements()`.
#'
#' @param e A call whose arguments have already been folded.
#' @noRd
cb_prune <- function(e) {
  is_drop <- function(x) identical(x, CB_DROP)

  if (identical(e[[1L]], quote(`{`)) && length(e) > 1L) {
    return(as.call(c(list(quote(`{`)), cb_drop_statements(as.list(e)[-1L]))))
  }
  # Binary operators where an absent operand means "leave the other one".
  if (length(e) == 3L && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("+", "|>", "%>%")) {
    if (is_drop(e[[3L]])) {
      return(e[[2L]])
    }
    if (is_drop(e[[2L]])) {
      return(e[[3L]])
    }
  }
  # Anywhere else a dropped branch really is NULL -- SET to NULL, which is
  # `e[i] <- list(NULL)`. `e[[i]] <- NULL` deletes the element instead, the
  # trap the comments in cb_subst() and cb_fold() already name. On an
  # assignment that cost the whole right-hand side: `x <- if (FALSE) 1` became
  # a one-argument `<-`, which DEPARSES as `x <- NULL` and throws
  # `incorrect number of arguments to "<-"` when the block evaluates it. The
  # script is valid R, nothing in the message or in the printed expression
  # points at the line, and no edit to the script can fix it.
  if (length(e) > 1L) {
    for (i in 2L:length(e)) {
      if (!cb_is_empty_sym(e[[i]]) && is_drop(e[[i]])) {
        e[i] <- list(NULL)
      }
    }
  }
  e
}


#' Take folded-away statements out of a sequence
#'
#' A dropped statement leaves. The last one is the value of the sequence, and
#' `if (FALSE) x` in last position is `NULL` in plain R, so there it becomes
#' `NULL`. Removing it would make the statement before it the result.
#'
#' @param stmts A list of statements, already folded.
#' @noRd
cb_drop_statements <- function(stmts) {
  n <- length(stmts)
  if (!n) {
    return(stmts)
  }
  dropped <- vapply(stmts, identical, logical(1L), CB_DROP)
  if (dropped[[n]]) {
    stmts[n] <- list(NULL)
    dropped[[n]] <- FALSE
  }
  stmts[!dropped]
}


#' Names assigned anywhere in a set of statements
#'
#' `x <- `, `x = `, `x <<- ` and a `for` variable, at any depth. A loop
#' variable is an assignment in the same environment: after `for (n in 1:2)`
#' plain R leaves `n` at 2, whatever it was before.
#'
#' A control whose name the body also assigns is not a control: the script
#' means what plain R would mean, so the declaration stays code. The editor
#' footer says so.
#'
#' @param exprs A list of expressions.
#' @param formals Also count the formals of every function. They bind a name
#'   for the function's body only, so the compiler leaves them out and
#'   `cb_subst()` handles the scope. The "not an input" check counts them.
#' @noRd
cb_assigned_names <- function(exprs, formals = FALSE) {
  walk <- function(e) {
    if (is.pairlist(e)) {
      return(unlist(lapply(as.list(e), walk)))
    }
    if (!is.call(e)) {
      return(character())
    }
    out <- character()
    head <- e[[1L]]
    if (length(e) == 3L && is.name(e[[2L]]) &&
          (identical(head, quote(`<-`)) || identical(head, quote(`=`)) ||
             identical(head, quote(`<<-`)))) {
      out <- as.character(e[[2L]])
    }
    if (identical(head, quote(`for`)) && length(e) == 4L && is.name(e[[2L]])) {
      out <- c(out, as.character(e[[2L]]))
    }
    if (formals && identical(head, quote(`function`))) {
      out <- c(out, names(e[[2L]]))
    }
    for (i in seq_along(e)) {
      if (!cb_is_empty_sym(e[[i]])) {
        out <- c(out, walk(e[[i]]))
      }
    }
    out
  }
  unique(as.character(unlist(lapply(exprs, walk))))
}


#' Which declarations are not controls after all?
#'
#' A declaration is a control unless the script assigns the name again, which
#' happens two ways: a body statement writes it (the knob's value would be
#' thrown away), or a second declaration further down replaces it (only the
#' last one can be the control). Both are decided by reading the script,
#' without evaluating anything.
#'
#' A helper line says so for itself by starting its name with a dot, so there
#' is nothing to work out about `.lv <- unique(data$site)` feeding
#' `site <- factor("Basel", .lv)`.
#'
#' @param parsed The result of `cb_parse()`.
#' @return A logical vector over `parsed$stmts`.
#' @noRd
cb_demoted <- function(parsed) {
  n <- length(parsed$stmts)
  if (!parsed$ok || !n) {
    return(logical(n))
  }
  is_input <- vapply(parsed$stmts, `[[`, logical(1L), "input")
  nms <- vapply(parsed$stmts, `[[`, character(1L), "name")

  body_assigned <- cb_assigned_names(lapply(parsed$stmts[!is_input], `[[`, "expr"))

  reason <- vapply(seq_len(n), function(i) {
    if (!is_input[[i]]) {
      return("")
    }
    nm <- nms[[i]]
    if (nm %in% body_assigned) {
      return("assigned")
    }
    later <- is_input & seq_len(n) > i
    if (any(later & !is.na(nms) & nms == nm)) {
      return("redeclared")
    }
    ""
  }, character(1L))

  out <- nzchar(reason)
  attr(out, "reason") <- reason
  attr(out, "name") <- nms
  out
}


#' Compile the script into the block's expression
#'
#' @param parsed The result of `cb_parse()`.
#' @param specs Control specs from `cb_specs()`.
#' @param values Named list of current control values.
#' @param data_name The block's input slots: core's slot names (`.arg1`, a
#'   link name), named by what the script calls them (see `sb_slots()`). An
#'   unnamed entry is read under its own name. Each becomes a `.(slot)` that
#'   core fills on evaluation and replaces with the upstream block id on
#'   export. A script that reads `data` with no slot behind it gets
#'   `data <- NULL` in front.
#' @return A quoted expression with a `.(slot)` for every read of an input,
#'   or `NULL` when the script has no body.
#' @noRd
cb_expr <- function(parsed, specs, values = list(), data_name = "data") {
  if (!parsed$ok) {
    return(NULL)
  }
  demoted <- cb_demoted(parsed)
  specs <- Filter(cb_spec_ok, specs)

  # A declaration stays where it was written, as code, unless a usable control
  # replaced it. That covers the demoted lines and, just as importantly, a
  # declaration whose value could not be read: dropping the line while nothing
  # substitutes the name leaves the expression referring to a symbol that does
  # not exist.
  live <- vapply(specs, `[[`, character(1L), "name")
  keep <- vapply(seq_along(parsed$stmts), function(i) {
    st <- parsed$stmts[[i]]
    !st$input || demoted[[i]] || !st$name %in% live
  }, logical(1L))
  body <- lapply(parsed$stmts[keep], `[[`, "expr")
  if (!length(body)) {
    return(NULL)
  }

  script_names <- names(data_name)
  if (is.null(script_names)) {
    script_names <- data_name
  }
  script_names[!nzchar(script_names)] <- data_name[!nzchar(script_names)]
  slot_of <- function(i) call(".", as.name(data_name[[i]]))

  # An input name the script assigns is a local variable from then on, as in
  # plain R. Substituting its later reads would point them back at the
  # upstream block. Such a name is bound once, in front, and left alone.
  rebound <- setdiff(intersect(script_names, cb_assigned_names(body)), live)

  subs <- list()
  prelude <- list()
  for (i in seq_along(data_name)) {
    if (script_names[[i]] %in% rebound) {
      prelude[[length(prelude) + 1L]] <-
        call("<-", as.name(script_names[[i]]), slot_of(i))
    } else {
      subs[[script_names[[i]]]] <- slot_of(i)
    }
  }
  # A control wins over an input of the same name; the footer says so.
  for (s in specs) {
    subs[[s$name]] <- cb_literal(cb_value(s, values))
  }

  # No input, but the script reads `data`: it is NULL, as the README promises.
  # Bound in the expression, not the eval env, so the export says so too.
  if (!"data" %in% script_names &&
        "data" %in% unlist(lapply(body, all.names))) {
    prelude <- c(prelude, list(quote(data <- NULL)))
  }

  body <- lapply(body, function(e) cb_fold(cb_subst(e, subs)))
  body <- c(prelude, cb_drop_statements(body))
  # A lone NULL statement must stay a call: NULL means "no expression".
  if (length(body) == 1L && !is.null(body[[1L]])) {
    return(body[[1L]])
  }
  as.call(c(list(quote(`{`)), body))
}
