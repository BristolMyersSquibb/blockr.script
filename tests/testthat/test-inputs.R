# The script block as part of a board: how its inputs are named, what it does
# with none, and what its exported code reads like.

script_board <- function(script, links = blockr.core::links(), ...) {
  blockr.core::new_board(
    blocks = c(
      a = blockr.core::new_static_block(iris[1:3, ]),
      b = blockr.core::new_static_block(iris[1:5, ]),
      c = blockr.core::new_static_block(iris[1:7, ]),
      s = new_script_block(script)
    ),
    links = links
  )
}

board_server_of <- function(board) {
  blockr.core::get_s3_method("board_server", board)
}

test_that("inputs are read as data, .arg1 and by link name", {
  board <- script_board(
    "c(data = nrow(data), arg1 = nrow(.arg1), z = nrow(z))",
    blockr.core::links(
      blockr.core::new_link("a", "s", "data"),
      blockr.core::new_link("b", "s"),
      blockr.core::new_link("c", "s", "z")
    )
  )
  shiny::testServer(board_server_of(board), {
    session$flushReact()
    expect_identical(
      rv$blocks$s$server$result(),
      c(data = 3L, arg1 = 5L, z = 7L)
    )
  }, args = list(x = board))
})

test_that("a single named data link is the usual case", {
  board <- script_board(
    "n <- 2\n\nutils::head(data, n)",
    blockr.core::links(blockr.core::new_link("c", "s", "data"))
  )
  shiny::testServer(board_server_of(board), {
    session$flushReact()
    expect_identical(rv$blocks$s$server$result(), utils::head(iris[1:7, ], 2))
  }, args = list(x = board))
})

test_that("with no input the script is the source", {
  board <- script_board("data.frame(x = 1:3, empty = is.null(data))")
  shiny::testServer(board_server_of(board), {
    session$flushReact()
    expect_identical(
      rv$blocks$s$server$result(),
      data.frame(x = 1:3, empty = TRUE)
    )
  }, args = list(x = board))
})

test_that("exported code names the upstream blocks, not the inputs", {
  board <- script_board(
    "n <- 2\n\nmerge(utils::head(data, n), z)",
    blockr.core::links(
      blockr.core::new_link("a", "s", "data"),
      blockr.core::new_link("c", "s", "z")
    )
  )
  shiny::testServer(board_server_of(board), {
    session$flushReact()
    exprs <- lapply(rv$blocks, function(b) b$server$expr())
    code <- blockr.core:::export_wrapped_code(exprs, rv$board)
    line <- grep("^s <- ", strsplit(code, "\n")[[1L]], value = TRUE)
    expect_identical(line, "s <- local(merge(utils::head(a, 2), c))")
  }, args = list(x = board))
})

test_that("the chips describe each connected input", {
  info <- sb_input_info(list(data = iris[1:3, 1:2], z = sb_unavailable))
  expect_identical(vapply(info, `[[`, character(1L), "name"), c("data", "z"))
  expect_identical(info[[1L]]$dims, "3 \u00d7 2")
  expect_identical(info[[1L]]$cols[[1L]]$type, "dbl")
  expect_identical(info[[2L]]$dims, "waiting")

  # an unconnected data port has no chip
  expect_length(sb_input_info(list(data = NULL)), 0L)
})

test_that("a non-syntactic link name is inserted in backticks", {
  expect_identical(sb_symbol("ae_events"), "ae_events")
  expect_identical(sb_symbol(".arg2"), ".arg2")
  expect_identical(sb_symbol("ae events"), "`ae events`")
})

describe_cols <- function(d, field) {
  vapply(
    d$cols,
    function(cl) if (is.null(cl[[field]])) NA_character_ else cl[[field]],
    character(1L)
  )
}

test_that("a data frame chip shows rows by columns and lists its columns", {
  d <- sb_describe(iris, "data")
  expect_identical(d$dims, "150 \u00d7 5")
  expect_identical(d$what, "5 columns")
  expect_identical(describe_cols(d, "name"), names(iris))
  expect_identical(describe_cols(d, "type")[[5L]], "fct")
  expect_true(all(is.na(describe_cols(d, "insert"))))
})

test_that("a dm chip counts tables and lists each table, then its columns", {
  skip_if_not_installed("dm")
  d <- sb_describe(dm::dm(flowers = iris, cars = mtcars), "data")
  expect_identical(d$dims, "dm \u00b7 2 tables")
  expect_identical(d$what, "2 tables, 16 columns")
  expect_identical(describe_cols(d, "name")[c(1L, 2L, 7L)], c("flowers", "Sepal.Length", "cars"))
  expect_identical(describe_cols(d, "type")[c(1L, 7L)], c("tbl", "tbl"))
  expect_identical(describe_cols(d, "insert")[c(1L, 7L)], c("data$flowers", "data$cars"))
  expect_identical(d$cols[[2L]]$label, "flowers")
  expect_identical(d$cols[[2L]]$of, "data$flowers")

  # a column dropdown from a dm reads the column through its table
  line <- sb_control_templates(list(c(list(name = "data"), d)))[[1L]]$line
  expect_identical(
    line,
    paste0(
      "pick <- factor(data$flowers$Species[1], ",
      "levels = sort(unique(data$flowers$Species)))"
    )
  )
})

test_that("a model chip counts observations and lists coefficients first", {
  d <- sb_describe(lm(mpg ~ wt, data = mtcars), "fit")
  expect_identical(d$dims, "lm \u00b7 32 obs")
  expect_identical(d$what, "2 coefs, 12 elements")
  expect_identical(describe_cols(d, "name")[1:3], c("(Intercept)", "wt", "coefficients"))
  expect_identical(describe_cols(d, "type")[1:2], c("coef", "coef"))
  expect_identical(
    describe_cols(d, "insert")[1:3],
    c("coef(fit)[[\"(Intercept)\"]]", "coef(fit)[[\"wt\"]]", "fit$coefficients")
  )
  expect_identical(d$cols[[2L]]$label, "-5.344")
  expect_identical(d$cols[[which(describe_cols(d, "name") == "model")]]$type, "df")

  # coefficients are no names to complete
  comp <- vapply(sb_completions(list(c(list(name = "fit"), d))), `[[`, "", "label")
  expect_false("wt" %in% comp)
  expect_true("coefficients" %in% comp)
})

test_that("a list chip counts elements and lists them as x$element", {
  d <- sb_describe(list(a = 1:3, `odd name` = "x", 4), ".arg1")
  expect_identical(d$dims, "list \u00b7 3 elements")
  expect_identical(d$what, "2 elements")
  expect_identical(describe_cols(d, "insert"), c(".arg1$a", ".arg1$`odd name`"))
  expect_identical(describe_cols(d, "type"), c("int", "chr"))

  # a list without names, or a test result that is no model
  expect_identical(sb_describe(list(1, 2), "data")$cols, list())
  tt <- sb_describe(stats::t.test(1:10), "data")
  expect_identical(tt$dims, "htest \u00b7 10 elements")
  expect_false("coef" %in% describe_cols(tt, "type"))
})

test_that("a vector or matrix chip shows type and size, with nothing to search", {
  v <- sb_describe(c(1.5, 2), "v")
  expect_identical(v$dims, "dbl \u00b7 2")
  expect_identical(v$what, "")
  expect_length(v$cols, 0L)
  expect_identical(sb_describe(letters, "v")$dims, "chr \u00b7 26")
  expect_identical(sb_describe(matrix(1:6, 2), "m")$dims, "int \u00b7 2 \u00d7 3")
  expect_identical(sb_describe(mean, "f")$dims, "function")
})
