# The script's inputs, by the name the script uses.
#
# Script and prose blocks are purely variadic, `function(id, ...args)`, so a
# DAG or dock treats them like any other variadic block. Core names each slot
# by its link name. A link without a name is `.arg1`, `.arg2`, ...: core
# counts the unnamed links only, so with one named and one unnamed link the
# unnamed one is `.arg1`.
#
# The block then decides what the script calls each slot:
#
# * every slot reads by core's name for it;
# * the first slot also reads as `data`, whatever core calls it;
# * a slot core already calls `data` (a link named "data") is `data`, and the
#   first-slot rule does not apply;
# * with no input, `data` is NULL.
#
# Which slot is first follows the order of the links, and core puts a link
# that is removed and drawn again last. So `data` can move to another input.
# A slot's own name does not move. A script with several inputs is stable
# when it reads each by its own name, or when one link is named "data".
#
# sb_arg_refs() and sb_arg_values() mirror blockr.core's dot_arg_refs() and
# dot_arg_values() (R/utils-misc.R), which are not exported. Keep them in
# sync until core exports them.

sb_arg_refs <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    nms <- character(length(x))
  }
  unnamed <- !nzchar(nms)
  nms[unnamed] <- paste0(".arg", seq_len(sum(unnamed)))
  nms
}

sb_arg_values <- function(x) {
  vals <- if (inherits(x, "reactivevalues")) {
    shiny::reactiveValuesToList(x)
  } else {
    as.list(x)
  }
  stats::setNames(vals, sb_arg_refs(x))
}

# Marks an input that exists but cannot be read right now: an upstream block
# that is unset or failing, or a panel the visibility gate has closed. Distinct
# from `NULL`, which is what an unconnected `data` port gives.
sb_unavailable <- structure(list(), class = "sb_unavailable")

sb_read <- function(v) {
  tryCatch(
    if (is.function(v)) v() else v,
    error = function(e) sb_unavailable
  )
}

#' Map the block's slots to the names the script reads
#'
#' @param args The block's `...args`.
#' @return A character vector of core slot names, named by what the script
#'   calls each one. Every slot is there under its own name, in link order.
#'   The first slot is there a second time, in front, under the name `data`,
#'   unless a slot is itself called `data`. So one slot can have two names,
#'   and `names()` is the full set of names the script reads inputs by.
#' @noRd
sb_slots <- function(args) {
  core <- sb_arg_refs(args)
  out <- stats::setNames(core, core)
  if (length(core) && !"data" %in% core) {
    out <- c(data = core[[1L]], out)
  }
  out
}

#' Every input of a script block, by name
#'
#' @param args The block's `...args`.
#' @return A named list with one entry per name in `sb_slots()`: `data` first
#'   (NULL with no input), then each input under its own name. An input that
#'   cannot be read is `sb_unavailable`. When `data` is the first input by
#'   position, that input's own name is the `alias` attribute, so a caller
#'   can tell the two entries are one input.
#' @noRd
sb_inputs <- function(args) {
  slots <- sb_slots(args)
  vals <- lapply(sb_arg_values(args), sb_read)
  out <- stats::setNames(vals[unname(slots)], names(slots))
  if (!"data" %in% names(out)) {
    out <- c(list(data = NULL), out)
  }
  out <- out[c("data", setdiff(names(out), "data"))]
  if (length(slots) && !identical(slots[["data"]], "data")) {
    attr(out, "alias") <- slots[["data"]]
  }
  out
}

sb_is_unavailable <- function(x) {
  inherits(x, "sb_unavailable")
}

#' Fill inputs that cannot be read right now with the last value read
#'
#' @param inputs From `sb_inputs()`.
#' @param store An environment that keeps the last readable value of each
#'   input between calls.
#' @return `inputs` with every unavailable entry replaced, or `NULL` when one
#'   of them has never been read.
#' @noRd
sb_last_read <- function(inputs, store) {
  nms <- names(inputs)
  gone <- vapply(inputs, sb_is_unavailable, logical(1L))
  rm(list = setdiff(ls(store, all.names = TRUE), nms), envir = store)
  for (i in which(!gone)) {
    assign(nms[[i]], inputs[[i]], envir = store)
  }
  if (!all(nms[gone] %in% ls(store, all.names = TRUE))) {
    return(NULL)
  }
  inputs[gone] <- mget(nms[gone], envir = store)
  inputs
}

# One chip per connected input: its name, its size and its columns. `data`
# shows only when something is connected to it. The first input has two
# names, `data` and its own; it gets one chip, called `data`, with the other
# name as `alias`.
sb_input_info <- function(inputs) {
  alias <- attr(inputs, "alias", exact = TRUE)
  nms <- names(inputs)
  keep <- vapply(
    seq_along(inputs),
    function(i) {
      if (identical(nms[[i]], alias)) {
        return(FALSE)
      }
      nms[[i]] != "data" || !is.null(inputs[[i]]) || !is.null(alias)
    },
    logical(1L)
  )
  lapply(nms[keep], function(nm) {
    x <- inputs[[nm]]
    head <- list(name = nm, alias = if (identical(nm, "data")) alias)
    if (sb_is_unavailable(x)) {
      return(c(head, list(dims = "waiting", cols = list(), what = "")))
    }
    c(head, sb_describe(x, sb_symbol(nm)))
  })
}

#' What a chip says about one input, and what its search lists
#'
#' * a data frame: rows by columns; its columns
#' * a dm: its tables; each table (inserting `x$table`), then its columns
#' * a model fit, such as `lm`: its class and number of observations; its
#'   coefficients (inserting `coef(x)[["term"]]`), then its elements
#' * any other list: its class and length; its elements, each inserting
#'   `x$element`
#' * a matrix: rows by columns; a vector: type and length; nothing to search
#'
#' @param x The input.
#' @param ref How the script reads it (`data`, `.arg1`, `` `ae events` ``).
#' @return `list(dims, cols, what)`. A `cols` entry has `name`, `type`,
#'   `label` and, where the name alone is not what to insert, `insert`. A dm
#'   column also has `of`, the table it belongs to. `what` says what the
#'   search covers ("5 columns"), empty when there is nothing.
#' @noRd
sb_describe <- function(x, ref) {
  if (is.data.frame(x)) {
    return(list(
      dims = sb_dims_df(x),
      cols = sb_columns(x),
      what = sb_count(ncol(x), "column")
    ))
  }
  if (inherits(x, "dm") && requireNamespace("dm", quietly = TRUE)) {
    return(sb_describe_dm(x, ref))
  }
  if (is.list(x)) {
    return(sb_describe_list(x, ref))
  }
  if (is.atomic(x) && length(dim(x)) == 2L) {
    return(list(
      dims = paste(sb_type_abbr(x), sb_dims_df(x), sep = " \u00b7 "),
      cols = list(),
      what = ""
    ))
  }
  if (is.atomic(x) && is.null(dim(x))) {
    return(list(
      dims = paste(sb_type_abbr(x), format(length(x), big.mark = ","), sep = " \u00b7 "),
      cols = list(),
      what = ""
    ))
  }
  list(dims = class(x)[[1L]], cols = list(), what = "")
}

sb_describe_dm <- function(x, ref) {
  tbls <- tryCatch(dm::dm_get_tables(x), error = function(e) list())
  cols <- list()
  for (tb in names(tbls)) {
    t <- tbls[[tb]]
    of <- paste0(ref, "$", sb_symbol(tb))
    cols <- c(
      cols,
      list(list(name = tb, type = "tbl", label = sb_dims_df(t), insert = of)),
      lapply(sb_columns(t), function(cl) {
        cl$label <- paste(c(tb, if (nzchar(cl$label)) cl$label), collapse = " \u00b7 ")
        cl$of <- of
        cl
      })
    )
  }
  n <- length(tbls)
  list(
    dims = paste("dm", sb_count(n, "table"), sep = " \u00b7 "),
    cols = cols,
    what = paste(
      c(sb_count(n, "table"), if (length(cols) > n) sb_count(length(cols) - n, "column")),
      collapse = ", "
    )
  )
}

# A model is a list that stats::nobs() can count. It lists its coefficients
# first: they are what a script about a fit reads most.
sb_describe_list <- function(x, ref) {
  cls <- class(x)[[1L]]
  size <- if (identical(cls, "list")) {
    NULL
  } else {
    tryCatch(stats::nobs(x), error = function(e) NULL, warning = function(w) NULL)
  }
  model <- is.numeric(size) && length(size) == 1L && !is.na(size) && size > 0
  coefs <- if (model) {
    tryCatch(stats::coef(x), error = function(e) NULL)
  } else {
    NULL
  }
  if (!is.numeric(coefs) || is.null(names(coefs)) || !is.null(dim(coefs))) {
    coefs <- NULL
  }
  coef_cols <- lapply(names(coefs), function(term) {
    list(
      name = term, type = "coef",
      label = format(signif(coefs[[term]], 4L)),
      insert = sprintf("coef(%s)[[%s]]", ref, encodeString(term, quote = "\""))
    )
  })
  nms <- names(x)
  nms <- if (is.null(nms)) character() else nms[nzchar(nms)]
  el_cols <- lapply(nms, function(el) {
    list(
      name = el, type = sb_type_abbr(x[[el]]), label = "",
      insert = paste0(ref, "$", sb_symbol(el))
    )
  })
  dims <- if (model) {
    paste(cls, paste(format(size, big.mark = ","), "obs"), sep = " \u00b7 ")
  } else {
    paste(cls, sb_count(length(x), "element"), sep = " \u00b7 ")
  }
  list(
    dims = dims,
    cols = c(coef_cols, el_cols),
    what = paste(
      c(
        if (length(coef_cols)) sb_count(length(coef_cols), "coef"),
        if (length(el_cols)) sb_count(length(el_cols), "element")
      ),
      collapse = ", "
    )
  )
}

sb_count <- function(n, noun) {
  sprintf("%s %s%s", format(n, big.mark = ","), noun, if (n == 1L) "" else "s")
}

sb_dims_df <- function(x) {
  paste(format(nrow(x), big.mark = ","), "\u00d7", ncol(x))
}

sb_columns <- function(x) {
  lapply(names(x), function(cl) {
    lab <- attr(x[[cl]], "label", exact = TRUE)
    list(
      name = cl,
      type = sb_type_abbr(x[[cl]]),
      label = if (is.character(lab) && length(lab) == 1L) lab else ""
    )
  })
}

sb_type_abbr <- function(x) {
  if (is.data.frame(x)) {
    return("df")
  }
  if (is.factor(x)) {
    return("fct")
  }
  if (inherits(x, "Date")) {
    return("date")
  }
  if (inherits(x, "POSIXct")) {
    return("dttm")
  }
  switch(
    typeof(x),
    character = "chr",
    double = "dbl",
    integer = "int",
    logical = "lgl",
    list = "list",
    typeof(x)
  )
}
