# The expression a script compiles to: what is substituted, what is folded
# away, and what is left as written.

expr_of <- function(script, values = list(), data = iris, refs = "data") {
  p <- cb_parse(script)
  cb_expr(p, cb_specs(p, inputs = list(data = data)), values, data_name = refs)
}

# Fill the slots the way blockr.core does, then evaluate.
run_expr <- function(expr, ...) {
  dat <- list(...)
  expr <- do.call(
    bquote,
    list(expr, lapply(stats::setNames(nm = names(dat)), as.name), splice = TRUE)
  )
  eval(expr, list2env(dat, parent = baseenv()))
}

test_that("a function with no arguments keeps its body", {
  e <- expr_of("f <- function() nrow(data)\nf()")
  expect_identical(e[[2L]], quote(f <- function() nrow(.(data))))
  expect_identical(run_expr(e, data = iris), 150L)

  expect_identical(
    expr_of("n <- 7\nsapply(1:2, \\() n)"),
    quote(sapply(1:2, function() 7))
  )
})

test_that("a script that assigns to an input name reads its own value after", {
  script <- "data <- utils::head(data, 2)\nnrow(data)"
  e <- expr_of(script, refs = c(data = ".arg1"))
  expect_identical(e[[2L]], quote(data <- .(.arg1)))
  expect_identical(run_expr(e, .arg1 = iris), 2L)
  # The export fills the slot with the upstream block's name.
  exported <- do.call(bquote, list(e, list(.arg1 = quote(a))))
  expect_identical(
    eval(exported, list2env(list(a = iris), parent = baseenv())),
    2L
  )

  # A loop variable and an assignment inside a function count too.
  e <- expr_of("for (data in 1:3) NULL\ndata")
  expect_identical(run_expr(e, data = iris), 3L)
  e <- expr_of("f <- function() { data <- 1; data }\nf()")
  expect_identical(run_expr(e, data = iris), 1)

  # Other inputs are still substituted.
  e <- expr_of(
    "data <- utils::head(data, 1)\nnrow(z)",
    refs = c(data = "data", z = "z")
  )
  expect_identical(e[[length(e)]], quote(nrow(.(z))))
})

test_that("a call head that is itself a call is substituted", {
  e <- expr_of(
    "pick <- factor(\"mean\", c(\"mean\", \"median\"))\nmatch.fun(pick)(1:3)"
  )
  expect_identical(e, quote(match.fun("mean")(1:3)))
  expect_identical(run_expr(e, data = iris), 2)

  expect_identical(expr_of("data$f(1)"), quote(.(data)$f(1)))
  expect_identical(
    expr_of("fit$family$linkinv(eta)", refs = c(fit = ".arg1")),
    quote(.(.arg1)$family$linkinv(eta))
  )
})

test_that("an emptied multi-select stays empty", {
  script <- "x <- factor(c(\"a\", \"b\"), c(\"a\", \"b\", \"c\"))\nlength(x)"
  expect_identical(expr_of(script), bquote(length(.(c("a", "b")))))
  expect_identical(
    run_expr(expr_of(script, values = list(x = character())), data = NULL),
    0L
  )
  # A restored board hands the empty pick back as an empty list.
  expect_identical(
    run_expr(expr_of(script, values = list(x = list())), data = NULL),
    0L
  )
  # A single select has no empty state, so it reads the script's value.
  expect_identical(
    expr_of("x <- factor(\"a\", c(\"a\", \"b\"))\nx", values = list(x = list())),
    "a"
  )
})

test_that("a folded-away `if` in last position leaves NULL as the result", {
  script <- "show <- FALSE\nx <- utils::head(data, 2)\nif (show) x"
  e <- expr_of(script)
  expect_null(run_expr(e, data = iris))
  expect_identical(
    run_expr(expr_of(script, values = list(show = TRUE)), data = iris),
    utils::head(iris, 2)
  )

  # The same inside braces, and as the only statement.
  e <- expr_of("show <- FALSE\nf <- function() { 1; if (show) 2 }\nf()")
  expect_null(run_expr(e, data = iris))
  e <- expr_of("show <- FALSE\nif (show) 1")
  expect_false(is.null(e))
  expect_null(run_expr(e, data = iris))

  # A dropped statement that is not the last one still leaves.
  e <- expr_of("show <- FALSE\nif (show) print(1)\n2")
  expect_identical(e, 2)
})

test_that("pkg::name is left alone when name is a control", {
  e <- expr_of("n <- 3\nsapply(list(1:3), dplyr::n)")
  expect_identical(e, quote(sapply(list(1:3), dplyr::n)))
  expect_identical(
    expr_of("n <- 3\nf(pkg:::n, n)"),
    quote(f(pkg:::n, 3))
  )
})

test_that("values are written as code a person would write", {
  expect_identical(cb_literal(as.Date("2026-01-02")), quote(as.Date("2026-01-02")))
  expect_identical(cb_literal(factor(c("a", "b"))), c("a", "b"))
  expect_identical(cb_literal(6L), 6L)
  expect_identical(cb_literal(character()), character())
  expect_identical(cb_literal(TRUE), TRUE)
})

test_that("script_expr() writes the values in and renames the input", {
  expect_identical(
    script_expr("n <- 6\n\nutils::head(data, n)", list(n = 3)),
    quote(utils::head(data, 3))
  )
  expect_identical(
    script_expr(c("n <- 6", "utils::head(data, n)"), data = "adsl"),
    quote(utils::head(adsl, 6))
  )
  # what the block evaluates, with the input's name filled in
  script <- "keep <- TRUE\nx <- if (keep) data else NULL\nNROW(x) + nrow(data)"
  parsed <- cb_parse(script)
  block_expr <- cb_expr(
    parsed, cb_specs(parsed, list(data = iris)), list(), c(data = "data")
  )
  expect_identical(
    script_expr(script, data = "d"),
    do.call(bquote, list(block_expr, list(data = as.name("d"))))
  )
  expect_null(script_expr("utils::head(data,"))
  expect_null(script_expr("n <- 1"))
})

test_that("script_expr() takes a value for a declaration that needs the input", {
  script <- paste(
    "arm <- factor(\"A\", levels = unique(data$ARM))",
    "top <- 2L",
    "utils::head(data[data$ARM %in% arm, ], top)",
    sep = "\n"
  )
  # no value: the line stays code and runs with the rest
  expect_identical(
    script_expr(script, data = "d"),
    quote({
      arm <- factor("A", levels = unique(d$ARM))
      utils::head(d[d$ARM %in% arm, ], 2L)
    })
  )
  # a value: written in, and the declaration is gone
  expr <- script_expr(script, list(arm = "B", top = 1), data = "d")
  expect_identical(expr, quote(utils::head(d[d$ARM %in% "B", ], 1L)))
  d <- data.frame(ARM = c("A", "B", "B"), n = 1:3)
  expect_identical(eval(expr), d[2L, ])

  # a value as a saved board hands it back
  expect_identical(
    script_expr(script, list(arm = list("A", "B")), data = "d"),
    bquote(utils::head(d[d$ARM %in% .(c("A", "B")), ], 2L))
  )

  # a select whose choices are only partly known without the input
  script <- "g <- factor(\"all\", c(\"all\", sort(unique(data$g))))\nf(data, g)"
  expect_identical(script_expr(script), quote(f(data, "all")))
  expect_identical(script_expr(script, list(g = "west")), quote(f(data, "west")))
})

test_that("a NULL the script writes stays where it was written", {
  run <- function(script, values = list()) {
    eval(script_expr(script, values), new.env())
  }
  # the right-hand side of an assignment, with every assignment operator
  expect_identical(run("x <- NULL\nif (is.null(x)) 1 else 2"), 1)
  expect_identical(
    script_expr("x <- NULL\nis.null(x)"),
    quote({
      x <- NULL
      is.null(x)
    })
  )
  expect_true(run("x = NULL\nis.null(x)"))
  expect_true(run("x <<- NULL\nis.null(x)"))
  expect_true(run("{\n  y <- NULL\n  is.null(y)\n}"))

  # a function's body and the default of a formal
  expect_true(run("f <- function() NULL\nis.null(f())"))
  expect_identical(
    run("f <- function(a, b = NULL, k = NULL) NULL\nnames(formals(f))"),
    c("a", "b", "k")
  )
  expect_identical(run("(function(a, b = NULL) is.null(b))(1)"), TRUE)

  # the left side of `$`, and an argument in any position
  expect_true(run("is.null(NULL$a)"))
  expect_identical(run("length(list(NULL, 1, NULL))"), 3L)
  expect_identical(run("x <- list(a = NULL, b = 1)\nnames(x)"), c("a", "b"))

  # values that are empty but not NULL
  expect_identical(run("x <- c()\nlength(x)"), 0L)
  expect_identical(run("x <- list()\nlength(x)"), 0L)
  expect_identical(run("x <- character()\nx"), character())

  # next to a control, whose value is still written in
  expect_identical(
    run("k <- 2\nx <- NULL\nf <- function(k = NULL) k\nlist(x, f(), k)", list(k = 3)),
    list(NULL, NULL, 3)
  )
})

test_that("the block's own expression keeps a written NULL", {
  script <- "n <- 2\nx <- NULL\nf <- function(d = NULL) NULL\nutils::head(data, n)"
  parsed <- cb_parse(script)
  expr <- cb_expr(parsed, cb_specs(parsed, list(data = iris)), list(), c(data = "data"))
  expect_identical(eval_bquoted(expr, iris), utils::head(iris, 2))
  expect_identical(expr[[2L]], quote(x <- NULL))
  expect_identical(names(expr[[3L]][[3L]][[2L]]), "d")
  expect_null(expr[[3L]][[3L]][[3L]])
})
