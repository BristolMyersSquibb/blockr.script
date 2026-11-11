# What a saved board holds for a script block, and what comes back from it.

test_that("control values survive a trip through JSON", {
  script <- paste(
    "n <- 6",
    "k <- 2L",
    "when <- as.Date(\"2026-01-01\")",
    "one <- factor(\"a\", c(\"a\", \"b\"))",
    "many <- factor(c(\"a\", \"b\"), c(\"a\", \"b\", \"c\"))",
    "none <- factor(\"a\", c(\"a\", \"b\"))",
    "txt <- \"x\"",
    "on <- TRUE",
    "list(n, k, when, one, many, txt, on)",
    sep = "\n"
  )
  values <- list(
    n = 7, k = 3L, when = as.Date("2026-03-04"), one = "b",
    many = character(), txt = "hello", on = FALSE
  )
  block <- new_script_block(script, values)
  json <- jsonlite::toJSON(blockr_ser(block), auto_unbox = TRUE, null = "null")
  back <- blockr.core::blockr_deser(
    jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  )
  expect_s3_class(back, "script_block")

  restored <- blockr_ser(back)$payload$values
  # JSON keeps no types: the date is a string, the empty pick a list
  expect_type(restored$when, "character")
  expect_length(restored$many, 0L)

  p <- cb_parse(script)
  specs <- cb_specs(p)
  expect_identical(
    eval(cb_expr(p, specs, restored)),
    eval(cb_expr(p, specs, values))
  )
  expect_identical(
    eval(cb_expr(p, specs, restored)),
    list(7, 3L, as.Date("2026-03-04"), "b", character(), "hello", FALSE)
  )
})

test_that("serialising with a live state writes that state, script as lines", {
  block <- new_script_block("n <- 6\n\nutils::head(data, n)")
  ser <- blockr_ser(
    block,
    state = list(script = "n <- 2\nutils::head(data, n)\n", values = list(n = 4))
  )
  expect_identical(ser$payload$script, c("n <- 2", "utils::head(data, n)", ""))
  expect_identical(ser$payload$values, list(n = 4))

  back <- blockr.core::blockr_deser(ser)
  expect_identical(blockr_ser(back)$payload, ser$payload)
})

test_that("a parse error is condensed to one line for the footer", {
  expect_null(parse_error("x <- 1"))
  expect_null(parse_error(""))
  expect_null(parse_error(NULL))
  msg <- parse_error("head(data,")
  expect_type(msg, "character")
  expect_identical(parse_error_brief(msg), "Line 2: unexpected end of input")
  expect_identical(parse_error_brief("  something else  "), "something else")

  # the default gate of the editor is "it parses"
  gate <- formals(setup_code_editor_server)$problem
  expect_null(eval(gate)("x <- 1"))
  expect_identical(eval(gate)("head(data,"), "Line 2: unexpected end of input")
})

test_that("cb_problem says why a script cannot be used", {
  expect_null(cb_problem(cb_parse("n <- 1\nn")))
  expect_match(cb_problem(cb_parse("f(")), "does not parse")
  expect_match(cb_problem(cb_parse("")), "is empty")
  expect_match(cb_problem(cb_parse("# only a comment")), "is empty")
  expect_match(cb_problem(cb_parse("n <- 1")), "has no result")
  expect_match(cb_problem(cb_parse("n <- 1"), brief = TRUE), "^no result")
  # a dotted line is code, so it is a result
  expect_null(cb_problem(cb_parse(".n <- 1")))
  expect_error(eval(cb_stop_expr("why")), "^why$")
})

test_that("a builder wrapped in memoise0 runs once", {
  runs <- 0L
  build <- memoise0(function() {
    runs <<- runs + 1L
    NULL
  })
  expect_null(build())
  expect_null(build())
  expect_identical(runs, 1L)
})

test_that("the blocks are registered with guidance that names the inputs", {
  expect_invisible(register_script_blocks())
  expect_true(all(
    c("script_block", "prose_block") %in% blockr.core::list_blocks()
  ))
  src <- paste(deparse(register_script_blocks), collapse = " ")
  expect_no_match(src, ".arg3", fixed = TRUE)
  expect_match(src, ".arg1", fixed = TRUE)
})
