# Helpers for blocks declaring `expr_type = "bquoted"`.
#
# Such a block must reference its input through the data SLOT -- the literal
# `.(data)` placeholder, `call(".", as.name("data"))` -- and never as a free
# `data` symbol. blockr substitutes only `.()` terms for these blocks and does
# NOT wrap the expression in `with(args, ...)`, so a bare `data` works in the
# app (the runtime env binds it) yet breaks every EXPORT: blockr.outline emits
# the symbol verbatim, it resolves up the search path to `utils::data` (a
# function), and each downstream block fails with "no applicable method for
# 'filter' applied to an object of class 'function'".

# Resolve a bquoted expression the way blockr.core does and evaluate it.
eval_bquoted <- function(expr, df) {
  expr <- do.call(bquote, list(expr, list(data = as.name("data"))))
  eval(expr, list(data = df))
}

# TRUE when the expression carries a `data` symbol that is NOT wrapped in the
# `.()` slot.
#
# Two things this must not be confused with. `all.vars()` cannot express it:
# the slot `.(data)` contains a `data` symbol too, so all.vars() reports both
# the correct and the broken form. And eval_bquoted() above passes either way
# -- the eval env binds `data` -- which is exactly why the defect is invisible
# at runtime and surfaces only once an export runs.
has_bare_data <- function(e) {
  if (is.name(e)) {
    return(identical(as.character(e), "data"))
  }
  if (!is.call(e) && !is.pairlist(e)) {
    return(FALSE)
  }
  if (is.call(e) && identical(e[[1L]], as.name(".")) && length(e) == 2L) {
    return(FALSE)
  }
  # as.list() keeps NULL elements: a function definition's srcref slot is NULL
  # whenever the code was parsed without srcrefs, which is what an INSTALLED
  # package does and load_all() does not.
  any(vapply(as.list(e), has_bare_data, logical(1L)))
}

# The data slot itself, for expected-expression comparisons.
data_slot <- function() {
  call(".", as.name("data"))
}
