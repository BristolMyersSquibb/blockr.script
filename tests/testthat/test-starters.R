# "Start from" in the editor: every starter is a script that runs as inserted.

run_starter <- function(script, data) {
  parsed <- cb_parse(script)
  expect_null(cb_problem(parsed))
  inputs <- list(data = data)
  refs <- if (is.null(data)) character() else c(data = "data")
  specs <- cb_specs(parsed, inputs)
  expr <- cb_expr(parsed, specs, data_name = refs)
  expr <- do.call(bquote, list(expr, list(data = as.name("data")), splice = TRUE))
  list(
    specs = specs,
    result = eval(expr, list2env(inputs[names(refs)], parent = baseenv()))
  )
}

starter <- function(label) {
  Find(function(s) identical(s$label, label), sb_starters())$script
}

test_that("every starter has a label, a hint and a script that parses", {
  for (s in sb_starters()) {
    expect_true(nzchar(s$label))
    expect_true(nzchar(s$hint))
    expect_null(cb_problem(cb_parse(s$script)), info = s$label)
  }
})

test_that("the Table starter runs with and without an input", {
  out <- run_starter(starter("Table"), iris)
  expect_identical(out$result, utils::head(iris, 10))
  expect_null(run_starter(starter("Table"), NULL)$result)
})

test_that("the Chart starter runs with and without an input", {
  skip_if_not_installed("ggplot2")
  out <- run_starter(starter("Chart"), iris)
  expect_identical(vapply(out$specs, `[[`, "", "kind"), c("select", "select"))
  expect_identical(out$specs[[1L]]$choices, names(iris))
  expect_s3_class(out$result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(out$result))

  out <- run_starter(starter("Chart"), NULL)
  expect_identical(out$specs[[2L]]$default, "cyl")
  expect_s3_class(out$result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(out$result))

  # one column: no second dropdown, the lines stay code and it still draws
  out <- run_starter(starter("Chart"), iris[, 1, drop = FALSE])
  expect_false(is.null(out$specs[[2L]]$error))
  expect_s3_class(out$result, "ggplot")
})

test_that("the page starters run with and without an input", {
  for (label in c("HTML page", "External page")) {
    for (data in list(iris, NULL)) {
      out <- run_starter(starter(label), data)
      expect_s3_class(out$result, "html_page")
    }
  }
})
