# What the editor offers while the script is written: completions that say
# what a name is, and a warning for names that look like inputs but are not.


#' Completions for the editor
#'
#' Input names first, then each input's columns, tables or elements (a
#' model's coefficients are no names to complete). Each entry carries a detail
#' line, so the list says what a name is: an input and what it holds, or a
#' column's type, input and label. The detail leaves the row count out, so
#' the list is the same when only the rows of an input change.
#'
#' @param info The result of `sb_input_info()`.
#' @return A list of `list(label, detail)`.
#' @noRd
sb_completions <- function(info) {
  inputs <- unlist(lapply(info, function(x) {
    detail <- paste(c("input", if (nzchar(x$what)) x$what), collapse = " \u00b7 ")
    lapply(c(x$name, x$alias), function(nm) list(label = nm, detail = detail))
  }), recursive = FALSE)
  cols <- unlist(lapply(info, function(x) {
    keep <- Filter(function(cl) !identical(cl$type, "coef"), x$cols)
    lapply(keep, function(cl) {
      list(
        label = cl$name,
        detail = paste(
          c(cl$type, x$name, if (nzchar(cl$label)) cl$label),
          collapse = " \u00b7 "
        )
      )
    })
  }), recursive = FALSE)
  out <- c(inputs, cols)
  out[!duplicated(vapply(out, `[[`, character(1L), "label"))]
}


#' Names that are no input but a package's data frame
#'
#' A script that reads `cars` in a block with no input called `cars` gets
#' `datasets::cars` from the search path, silently. This finds such names:
#' symbols the script reads as a value, that it does not assign, that are no
#' input and no column of one, and that are bound on the search path to a
#' data frame a package provides.
#'
#' A name inside a data-masking call is most likely a column, so it is not
#' flagged there, see `sb_frame_candidates()`. Once a name is flagged, every
#' place the script reads it is marked.
#'
#' @param text The script.
#' @param inputs The names the script reads its inputs by.
#' @param columns Column names of the inputs.
#' @return A list of `list(line, from, to, name, pkg, title)`, with `from` and
#'   `to` 1-based columns on `line`. Empty when the script does not parse.
#' @noRd
sb_foreign_frames <- function(text, inputs, columns = character()) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list())
  }
  exprs <- tryCatch(
    parse(text = text, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(exprs)) {
    return(list())
  }

  assigned <- cb_assigned_names(as.list(exprs), formals = TRUE)
  known <- c(inputs, assigned)
  names <- setdiff(sb_frame_candidates(exprs, known), c(known, columns))
  if (!length(names)) {
    return(list())
  }
  pkgs <- sb_frame_packages(names)
  pkgs <- pkgs[!is.na(pkgs)]
  if (!length(pkgs)) {
    return(list())
  }

  # The token table is only needed to say where a flagged name stands, so a
  # script without one never pays for it.
  pd <- utils::getParseData(exprs, includeText = TRUE)
  if (is.null(pd) || !nrow(pd)) {
    return(list())
  }
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  prev <- c("", utils::head(pd$text, -1L))
  hits <- which(
    pd$token == "SYMBOL" &
      pd$text %in% names(pkgs) &
      !prev %in% c("$", "@", "::", ":::")
  )

  lapply(hits, function(i) {
    nm <- pd$text[[i]]
    list(
      line = pd$line1[[i]],
      from = pd$col1[[i]],
      to = pd$col2[[i]] + 1L,
      name = nm,
      pkg = pkgs[[nm]],
      title = sb_foreign_note(nm, pkgs[[nm]])
    )
  })
}

sb_foreign_note <- function(name, pkg) {
  sprintf(
    "%s is not an input; it comes from package %s. Write %s::%s if you mean it.",
    name, pkg, pkg, name
  )
}

#' Names a script reads where a data frame could stand
#'
#' Every symbol in a value position, except inside a data-masking call. In
#' `dplyr::filter(data, mpg > 20)` and `subset(data, sleep > 20)` the names
#' in the later arguments are columns of the first, and ggplot2 or datasets
#' having a data frame of the same name does not make them one.
#'
#' The rule: in a call to a named function whose first argument reads an
#' input or a variable of the script, a later argument that is itself a call
#' is not searched. A later argument that is a bare name still is, so
#' `merge(data, cars)` is found. Operators and control flow (`+`, `[`, `if`)
#' do not mask.
#'
#' @param exprs A list of expressions.
#' @param known The names of the inputs and of everything the script assigns.
#' @return A character vector of distinct names.
#' @noRd
sb_frame_candidates <- function(exprs, known) {
  flow <- c("if", "for", "while", "repeat", "function")
  walk <- function(e, masked) {
    if (is.name(e)) {
      if (masked || cb_is_empty_sym(e)) {
        return(character())
      }
      return(as.character(e))
    }
    if (is.pairlist(e)) {
      return(unlist(lapply(as.list(e), walk, masked)))
    }
    if (!is.call(e)) {
      return(character())
    }
    head <- e[[1L]]
    if (identical(head, quote(`::`)) || identical(head, quote(`:::`))) {
      return(character())
    }
    if (length(e) == 3L &&
          (identical(head, quote(`$`)) || identical(head, quote(`@`)))) {
      return(walk(e[[2L]], masked))
    }
    if (identical(head, quote(`function`))) {
      # Defaults and body. The fourth element is the srcref.
      return(c(walk(e[[2L]], masked), walk(e[[3L]], masked)))
    }
    verb <- if (is.name(head)) {
      nm <- as.character(head)
      grepl("^[A-Za-z.][A-Za-z0-9._]*$", nm) && !nm %in% flow
    } else {
      identical(head[[1L]], quote(`::`)) || identical(head[[1L]], quote(`:::`))
    }
    out <- if (is.call(head)) walk(head, masked) else character()
    inner <- masked
    for (i in seq_along(e)[-1L]) {
      # An empty argument (`x[, 1]`) cannot be assigned to a variable.
      if (cb_is_empty_sym(e[[i]])) {
        next
      }
      a <- e[[i]]
      if (i == 2L) {
        out <- c(out, walk(a, masked))
        if (verb && any(all.names(a) %in% known)) {
          inner <- TRUE
        }
      } else {
        out <- c(out, walk(a, if (is.name(a)) masked else inner))
      }
    }
    out
  }
  unique(unlist(lapply(exprs, walk, FALSE)))
}

# What sb_frame_packages() has looked up so far, and the search path it was
# looked up on.
sb_frame_cache <- new.env(parent = emptyenv())

#' The package each name's data frame comes from
#'
#' One lookup per distinct name, remembered for as long as the search path
#' stays the same. The editor asks on every keystroke, mostly for names it
#' asked about before.
#'
#' @param names Variable names.
#' @return A character vector named by `names`: the package, or `NA` when the
#'   name is not a data frame from an attached package.
#' @noRd
sb_frame_packages <- function(names) {
  path <- search()
  cache <- sb_frame_cache
  if (!identical(cache$path, path) || length(cache$found) > 5000L) {
    cache$path <- path
    cache$found <- character()
  }
  new <- setdiff(names, names(cache$found))
  if (length(new)) {
    cache$found[new] <- vapply(new, sb_frame_package, character(1L), path)
  }
  cache$found[names]
}

# The first attached package that has `name`, if what it has is a data frame.
# exists() on the package's own environment is one hash lookup;
# utils::find() lists every attached package for every name.
sb_frame_package <- function(name, path = search()) {
  for (where in path[startsWith(path, "package:")]) {
    env <- as.environment(where)
    if (!exists(name, envir = env, inherits = FALSE)) {
      next
    }
    obj <- tryCatch(
      get(name, envir = env, inherits = FALSE),
      error = function(e) NULL
    )
    if (is.data.frame(obj)) {
      return(sub("^package:", "", where))
    }
    return(NA_character_)
  }
  NA_character_
}
