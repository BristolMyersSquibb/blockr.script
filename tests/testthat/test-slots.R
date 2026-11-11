test_that("every input reads by core's name, the first one also as data", {
  args <- list(function() 1, function() 2, function() 3)
  expect_identical(
    sb_slots(args),
    c(data = ".arg1", .arg1 = ".arg1", .arg2 = ".arg2", .arg3 = ".arg3")
  )

  # core counts the unnamed links only
  args <- list(events = function() 1, function() 2, subjects = function() 3)
  expect_identical(
    sb_slots(args),
    c(data = "events", events = "events", .arg1 = ".arg1",
      subjects = "subjects")
  )
  expect_identical(sb_arg_refs(args), c("events", ".arg1", "subjects"))

  inputs <- sb_inputs(args)
  expect_identical(names(inputs), c("data", "events", ".arg1", "subjects"))
  expect_identical(inputs$data, 1)
  expect_identical(inputs$events, 1)
  expect_identical(attr(inputs, "alias"), "events")
})

test_that("the first input gets one chip, called data, that shows its name", {
  info <- sb_input_info(sb_inputs(list(events = function() iris, function() 2)))
  expect_identical(vapply(info, `[[`, character(1L), "name"), c("data", ".arg1"))
  expect_identical(info[[1L]]$alias, "events")
  expect_null(info[[2L]]$alias)

  html <- as.character(sb_inputs_ui(info, "ed"))
  expect_match(html, "blockr-script-chip__alias", fixed = TRUE)
  expect_match(html, "= events", fixed = TRUE)
  expect_match(html, "aria-haspopup=\"menu\"", fixed = TRUE)

  # both names complete
  labels <- vapply(sb_completions(info), `[[`, character(1L), "label")
  expect_identical(labels[1:3], c("data", "events", ".arg1"))

  # a link named data has no second name
  info <- sb_input_info(sb_inputs(list(data = function() iris)))
  expect_null(info[[1L]]$alias)
  expect_no_match(as.character(sb_inputs_ui(info, "ed")), "chip__alias")
})

test_that("the expression fills both names of the first input", {
  parsed <- cb_parse("c(nrow(data), nrow(events), nrow(.arg1))")
  refs <- sb_slots(list(events = function() 1, function() 2))
  expr <- cb_expr(parsed, list(), data_name = refs)
  expect_identical(expr, quote(c(nrow(.(events)), nrow(.(events)), nrow(.(.arg1)))))
})

test_that("a link named data is data, wherever it sits (saved boards)", {
  args <- list(instruments = function() 1, data = function() 2)
  expect_identical(
    sb_slots(args),
    c(instruments = "instruments", data = "data")
  )
  inputs <- sb_inputs(args)
  expect_identical(names(inputs), c("data", "instruments"))
  expect_identical(inputs$data, 2)
})

test_that("no input gives data = NULL", {
  expect_identical(sb_slots(list()), stats::setNames(character(), character()))
  expect_identical(sb_inputs(list()), list(data = NULL))
  expect_length(sb_input_info(sb_inputs(list())), 0L)
})

test_that("the expression fills data from the first slot", {
  parsed <- cb_parse("n <- 3\n\nutils::head(data, n)")
  specs <- cb_specs(parsed, inputs = list(data = iris))
  expr <- cb_expr(parsed, specs, data_name = c(data = ".arg1"))
  expect_identical(deparse(expr), "utils::head(.(.arg1), 3)")
})

test_that("a script that reads data with no input binds it to NULL", {
  parsed <- cb_parse("is.null(data)")
  expr <- cb_expr(parsed, list(), data_name = stats::setNames(character(), character()))
  expect_identical(eval(expr), TRUE)
  expect_match(paste(deparse(expr), collapse = " "), "data <- NULL", fixed = TRUE)
})

test_that("prose reads data from the first slot, or NULL without one", {
  e <- prose_expr("`r nrow(data)` rows", c(data = ".arg1"))
  expect_match(paste(deparse(e), collapse = ""), "data = .(.arg1)", fixed = TRUE)

  e <- prose_expr("`r is.null(data)`", stats::setNames(character(), character()))
  expect_match(paste(deparse(e), collapse = ""), "data = NULL", fixed = TRUE)
})
