test_that("new_prose_block constructs a text block", {
  b <- new_prose_block("hello")
  expect_s3_class(b, "prose_block")
  expect_s3_class(b, "text_block")
})

test_that("prose block evaluates inline R against its data input", {
  b <- new_prose_block(
    "Rows: `r nrow(data)`; mean mpg `r round(mean(data$mpg), 1)`"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "Rows: 32; mean mpg 20.1"
      )
    },
    args = list(
      x = b,
      data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(mtcars)))
    )
  )
})

test_that("plain markdown (no inline R) passes through", {
  b <- new_prose_block("## Title\n\nSome **bold** text.")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "## Title\n\nSome **bold** text."
      )
    },
    args = list(
      x = b,
      data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(mtcars)))
    )
  )
})

test_that("text state is a reactiveVal (external_ctrl contract)", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      expect_true(inherits(session$returned$state$text, "reactiveVal"))
      expect_identical(session$returned$state$text(), "init")
    },
    args = list(`...args` = list(data = shiny::reactive(mtcars)))
  )
})

test_that("UI edits flow into state (text input -> reactiveVal)", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      session$setInputs(text = "## Edited\n\n`r nrow(data)` rows")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "## Edited\n\n`r nrow(data)` rows"
      )
    },
    args = list(`...args` = list(data = shiny::reactive(mtcars)))
  )
})

test_that("external (AI) writes reach state, and re-render through result()", {
  b <- new_prose_block("init")

  # External-control write into the state reactiveVal updates it.
  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      session$returned$state$text("Total rows: `r nrow(data)`")
      session$flushReact()
      expect_identical(
        session$returned$state$text(),
        "Total rows: `r nrow(data)`"
      )
    },
    args = list(`...args` = list(data = shiny::reactive(mtcars)))
  )

  # A block restored with that markdown renders it (round-trip via ctor).
  restored <- new_prose_block("Total rows: `r nrow(data)`")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", restored),
    {
      session$flushReact()
      expect_identical(
        as.character(session$returned$result()),
        "Total rows: 32"
      )
    },
    args = list(
      x = restored,
      data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(mtcars)))
    )
  )
})

# Capture custom messages: assign over the root MockShinySession method
# (the blockr.dplyr test-ready-handshake.R pattern).
capture_messages <- function(session) {
  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()
  root <- session$rootScope()
  root$sendCustomMessage <- function(type, message) {
    sent$msgs <- c(sent$msgs, list(list(type = type, message = message)))
    invisible(NULL)
  }
  sent
}

last_of <- function(sent, type) {
  hits <- Filter(function(m) identical(m$type, type), sent$msgs)
  if (length(hits)) hits[[length(hits)]]$message else NULL
}

test_that("inline expressions evaluate individually; one failure is local", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(
        prose_exprs = list("nrow(data)", "mean(no_such_object)")
      )
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_true(vals[["nrow(data)"]]$ok)
      expect_identical(vals[["nrow(data)"]]$value, "32")
      expect_false(vals[["mean(no_such_object)"]]$ok)
      expect_match(vals[["mean(no_such_object)"]]$value, ".+")
    },
    args = list(`...args` = list(data = shiny::reactive(mtcars)))
  )
})

test_that("the value preview is dormant (ok = NA) before data arrives", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(prose_exprs = list("nrow(data)"))
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_true(is.na(vals[["nrow(data)"]]$ok))
    },
    args = list(`...args` = list(data = shiny::reactive(NULL)))
  )
})

test_that("evaluation keeps indentation and newlines", {
  b <- new_prose_block("- a\n  - b\n\nRows: `r nrow(data)`\n")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        paste(as.character(session$returned$result()), collapse = "\n"),
        "- a\n  - b\n\nRows: 32\n"
      )
    },
    args = list(
      x = b,
      data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(mtcars)))
    )
  )
})

test_that("braces are plain text", {
  b <- new_prose_block("::: {.callout-note}\nhi\n:::")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", b),
    {
      session$flushReact()
      expect_identical(
        paste(as.character(session$returned$result()), collapse = "\n"),
        "::: {.callout-note}\nhi\n:::"
      )
    },
    args = list(
      x = b,
      data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(mtcars)))
    )
  )
})

test_that("inline_r writes values in and leaves code alone", {
  d <- list(data = mtcars)
  expect_identical(inline_r("n = `r nrow(data)`", d), "n = 32")
  expect_identical(inline_r("`r 1 + 1` and `code`", d), "2 and `code`")
  expect_identical(
    inline_r("```r\n`r nrow(data)`\n```\n`r 2 * 2`", d),
    "```r\n`r nrow(data)`\n```\n4"
  )
  expect_identical(inline_r("`r letters[1:3]`"), "a, b, c")
  expect_error(inline_r("`r no_such_thing`"), "no_such_thing")
})

test_that("a prose block needs no input", {
  b <- new_prose_block("Just text.")
  expect_identical(blockr.core:::block_min_args(b), 0L)
})

# As part of a board: how the inputs are named, and what the export reads like.

prose_board <- function(text, links = blockr.core::links()) {
  blockr.core::new_board(
    blocks = c(
      a = blockr.core::new_static_block(iris[1:3, ]),
      b = blockr.core::new_static_block(iris[1:5, ]),
      c = blockr.core::new_static_block(iris[1:7, ]),
      p = new_prose_block(text)
    ),
    links = links
  )
}

prose_board_server <- function(board) {
  blockr.core::get_s3_method("board_server", board)
}

test_that("inputs are read as data, .arg1 and by link name", {
  board <- prose_board(
    "`r nrow(data)` `r nrow(.arg1)` `r nrow(z)`",
    blockr.core::links(
      blockr.core::new_link("a", "p", "data"),
      blockr.core::new_link("b", "p"),
      blockr.core::new_link("c", "p", "z")
    )
  )
  shiny::testServer(prose_board_server(board), {
    session$flushReact()
    expect_identical(rv$blocks$p$server$result(), "3 5 7")
  }, args = list(x = board))
})

test_that("with no input the text computes on its own", {
  board <- prose_board("Plain, `r 1 + 1`.")
  shiny::testServer(prose_board_server(board), {
    session$flushReact()
    expect_identical(rv$blocks$p$server$result(), "Plain, 2.")
  }, args = list(x = board))
})

test_that("exported code names the upstream blocks the text uses", {
  board <- prose_board(
    "`r nrow(data)` and `r nrow(z)`",
    blockr.core::links(
      blockr.core::new_link("a", "p", "data"),
      blockr.core::new_link("b", "p"),
      blockr.core::new_link("c", "p", "z")
    )
  )
  shiny::testServer(prose_board_server(board), {
    session$flushReact()
    exprs <- lapply(rv$blocks, function(b) b$server$expr())
    code <- blockr.core:::export_wrapped_code(exprs, rv$board)
    code <- gsub("\\s+", " ", code)
    expect_match(
      code,
      paste0(
        "p <- local(blockr.script::inline_r(\"`r nrow(data)` and `r nrow(z)`\", ",
        "list(data = a, z = c)))"
      ),
      fixed = TRUE
    )
  }, args = list(x = board))
})

test_that("the expression lists only the inputs the text uses", {
  e <- prose_expr("`r nrow(data)`, `r .arg2$x`", c("data", ".arg1", ".arg2"))
  expect_identical(
    e,
    quote(blockr.script::inline_r("`r nrow(data)`, `r .arg2$x`",
      list(data = .(data), .arg2 = .(.arg2))))
  )
  expect_identical(
    prose_expr("no code", "data"),
    quote(blockr.script::inline_r("no code", list()))
  )
})

test_that("with no input the preview shows a value", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(prose_exprs = list("1 + 1", "is.null(data)"))
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_true(vals[["1 + 1"]]$ok)
      expect_identical(vals[["1 + 1"]]$value, "2")
      # `data` is NULL, not utils::data
      expect_identical(vals[["is.null(data)"]]$value, "TRUE")
    },
    args = list(`...args` = list())
  )
})

test_that("only an expression that names a waiting input is dormant", {
  b <- new_prose_block("x")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(prose_exprs = list("nrow(data)", "nrow(z)", "1 + 1"))
      session$flushReact()

      vals <- last_of(sent, "prose-values")$values
      expect_identical(vals[["nrow(data)"]]$value, "32")
      expect_true(is.na(vals[["nrow(z)"]]$ok))
      expect_identical(vals[["1 + 1"]]$value, "2")
    },
    args = list(`...args` = list(
      data = shiny::reactive(mtcars),
      z = shiny::reactive(stop("not ready"))
    ))
  )
})

test_that("a commit from the editor is not sent back to it", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      sent <- capture_messages(session)
      session$setInputs(text = "typed by the user")
      session$flushReact()
      expect_identical(session$returned$state$text(), "typed by the user")
      expect_null(last_of(sent, "prose-set"))

      # A write from outside is sent, with a version.
      session$returned$state$text("external")
      session$flushReact()
      expect_identical(last_of(sent, "prose-set")$markdown, "external")
      expect_identical(last_of(sent, "prose-set")$version, 1L)

      # Also when it restores the text of the last commit.
      session$setInputs(text = "second commit", text_version = 1L)
      session$returned$state$text("external")
      session$flushReact()
      expect_identical(last_of(sent, "prose-set")$version, 2L)
    },
    args = list(`...args` = list())
  )
})

test_that("a commit that is older than an external write is dropped", {
  b <- new_prose_block("init")

  shiny::testServer(
    blockr.core:::block_expr_server(b),
    {
      session$flushReact()
      # The commit was made before the write reached the editor.
      session$returned$state$text("external write")
      session$setInputs(text = "stale user text", text_version = 0L)
      session$flushReact()
      expect_identical(session$returned$state$text(), "external write")

      session$setInputs(text = "stale again", text_version = 0L)
      session$flushReact()
      expect_identical(session$returned$state$text(), "external write")

      # A commit made after the editor took the write goes through.
      session$setInputs(text = "fresh", text_version = 1L)
      session$flushReact()
      expect_identical(session$returned$state$text(), "fresh")
    },
    args = list(`...args` = list())
  )
})

test_that("code between double backticks is shown, not evaluated", {
  d <- list(data = mtcars)
  expect_identical(
    inline_r("show `` `r nrow(data)` `` literally", d),
    "show `` `r nrow(data)` `` literally"
  )
  expect_identical(inline_r("``r 1`` and `r 1 + 1`", d), "``r 1`` and 2")
  expect_identical(inline_r_codes("`` `r a` `` `r b`"), "b")
  expect_identical(inline_r("`` `r a` `` and ```r b``` and `r 3`"), "`` `r a` `` and ```r b``` and 3")
  # Markdown pairs the lone backtick with the next one, so no code is left.
  expect_identical(inline_r("a ` lone tick and `r 1 + 1`"), "a ` lone tick and `r 1 + 1`")
  expect_identical(inline_r("a `` lone pair and `r 1 + 1`"), "a `` lone pair and 2")
})

test_that("a fence closes only on its own marker", {
  four <- "````\n```\n`r stop('inner')`\n```\n`r stop('still inside')`\n````\n`r 1 + 1`"
  expect_identical(
    inline_r(four),
    sub("`r 1 + 1`", "2", four, fixed = TRUE)
  )
  tilde <- "```\n~~~\n`r stop('inside')`\n```\n`r 2 + 2`"
  expect_identical(inline_r(tilde), sub("`r 2 + 2`", "4", tilde, fixed = TRUE))
  # A fence line with an info string opens but does not close.
  info <- "```r\n```r\n`r stop('inside')`\n```\n`r 3`"
  expect_identical(inline_r(info), sub("`r 3`", "3", info, fixed = TRUE))
  # An open fence runs to the end.
  expect_identical(inline_r("~~~\n`r stop('x')`"), "~~~\n`r stop('x')`")
  expect_identical(inline_r_lines("a\n```\nb\n```\nc")$fence, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("CRLF line ends are kept", {
  expect_identical(inline_r("a `r 1 + 1`\r\n```\r\n`r x`\r\n```\r\nb `r 3`"),
    "a 2\r\n```\r\n`r x`\r\n```\r\nb 3")
})

test_that("code that spans lines is left alone", {
  expect_identical(inline_r("a `r 1 +\n1` b"), "a `r 1 +\n1` b")
})

test_that("inline values are short vectors", {
  expect_identical(inline_r("`r 1e6`"), "1000000")
  expect_identical(inline_r("`r 100000`"), "100000")
  expect_identical(inline_r("`r c(1, 2.5, 10)`"), "1, 2.5, 10")
  expect_identical(inline_r("`r 1e-20`"), "1e-20")
  expect_identical(inline_r("`r pi`"), "3.141593")
  expect_identical(inline_r("`r NA`"), "NA")
  expect_identical(inline_r("`r factor('a')`"), "a")
  expect_identical(inline_r("`r as.Date('2026-01-02')`"), "2026-01-02")
  expect_identical(inline_r("`r letters[1:3]`"), "a, b, c")
  expect_identical(inline_r("[`r NULL`]"), "[]")
  expect_identical(inline_r("`r 1:30`"), paste(1:30, collapse = ", "))
  expect_error(inline_r("`r 1:101`"), "101 values")
  expect_error(
    inline_r("`r head(data, 2)`", list(data = mtcars)),
    "short vector, not a data.frame"
  )
  expect_error(inline_r("`r mean`"), "short vector, not a function")
  expect_error(inline_r("`r list(1, 2)`"), "short vector, not a list")
})

test_that("the preview shortens a long value and marks it", {
  expect_identical(
    inline_value(1:30, 80L),
    paste0(substr(paste(1:20, collapse = ", "), 1L, 79L), "\u2026")
  )
  expect_identical(inline_value(1:3, 80L), "1, 2, 3")
  expect_no_error(inline_value(1:500, 80L))
})

test_that("several expressions run in order, an empty one has its own message", {
  expect_identical(inline_r("`r x <- 2; x * 3`"), "6")
  expect_identical(inline_r("`r 1; 2`"), "2")
  expect_error(inline_r("`r # note`"), "holds no expression")
  # A backtick ends the code, so the rest does not parse.
  expect_error(inline_r("`r data$`my col``"))
})

test_that("data is bound in every case", {
  expect_identical(inline_r("`r is.null(data)`"), "TRUE")
  expect_identical(inline_r("`r is.null(data)`", list(x = 1)), "TRUE")
  expect_identical(inline_r("`r nrow(data)`", list(data = mtcars)), "32")
})

test_that("prose_expr with code that does not parse, and with data unconnected", {
  expect_identical(
    prose_expr("`r nrow(`", "data"),
    quote(blockr.script::inline_r("`r nrow(`", list()))
  )
  expect_identical(
    prose_expr("`r is.null(data)`", character()),
    quote(blockr.script::inline_r("`r is.null(data)`", list(data = NULL)))
  )
  expect_identical(prose_names("`` `r a` `` `r b + c`"), c("+", "b", "c"))
})
