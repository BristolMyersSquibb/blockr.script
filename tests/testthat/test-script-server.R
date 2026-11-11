# The block's server: what reaches the state and the expression when the
# script, the controls or the inputs change.

script_server <- function(block, expr, data = shiny::reactive(datasets::iris)) {
  shiny::testServer(
    block[["expr_server"]],
    expr,
    args = list(`...args` = list(data = data)),
    env = parent.frame()
  )
}

# What the hidden-panel gate does to a block's input.
gated <- function(open, value = datasets::iris) {
  shiny::reactive({
    if (!open()) {
      shiny::req(FALSE)
    }
    if (is.function(value)) value() else value
  })
}

test_that("a control that changes kind does not end the session", {
  block <- new_script_block("d <- \"hello\"\nnchar(d)")
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    session$setInputs(cb_d = "abc")
    expect_identical(session$returned$state$values(), list(d = "abc"))

    session$returned$state$script("d <- as.Date(\"2026-01-01\")\nformat(d)")
    session$flushReact()
    expect_identical(
      session$returned$expr(),
      quote(format(as.Date("2026-01-01")))
    )

    # the date picker reports the script's date: the leftover text goes
    session$setInputs(cb_d = "2026-01-01")
    expect_identical(session$returned$state$values(), list())
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("an integer default and a stored integer are not rewritten", {
  block <- new_script_block("n <- 6L\n\nutils::head(data, n)")
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    session$setInputs(cb_n = 6)
    expect_identical(session$returned$state$values(), list())
    expect_identical(session$returned$expr(), quote(utils::head(.(data), 6L)))
    session$setInputs(cb_n = 4)
    expect_identical(session$returned$state$values(), list(n = 4L))
  }, args = list(`...args` = list(data = shiny::reactive(iris))))

  # JSON hands a stored 7 back as an integer
  block <- new_script_block("n <- 6\n\nutils::head(data, n)", list(n = 7L))
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    session$setInputs(cb_n = 7)
    expect_identical(session$returned$state$values(), list(n = 7L))
    expect_identical(session$returned$expr(), quote(utils::head(.(data), 7)))
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("a script that cannot be used gives the block its reason as error", {
  block <- new_script_block("n <- 3\n\nutils::head(data, n)")
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    good <- session$returned$expr()

    session$returned$state$script("head(data,")
    session$flushReact()
    expect_false(identical(session$returned$expr(), good))
    expect_error(eval(session$returned$expr()), "does not parse. Line 2")

    session$returned$state$script("n <- 6")
    session$flushReact()
    expect_error(eval(session$returned$expr()), "has no result")

    session$returned$state$script("n <- 6\nn")
    session$flushReact()
    expect_identical(session$returned$expr(), 6)
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("a block restored with an unusable script loads and reports it", {
  suppressWarnings(
    ser <- blockr_ser(new_script_block("n <- 6", values = list(n = 2)))
  )
  expect_warning(block <- blockr.core::blockr_deser(ser), "has no result")
  expect_s3_class(block, "script_block")

  shiny::testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_error(eval(session$returned$expr()), "has no result")
      expect_identical(session$returned$state$values(), list(n = 2))
    },
    args = list(
      x = block,
      data = list(
        `...args` = reactives::reactive_exprs(data = shiny::reactive(iris))
      )
    )
  )
})

test_that("Run and Cmd-Enter do not commit a script that cannot be used", {
  script <- "n <- 3\n\nutils::head(data, n)"
  block <- new_script_block(script)
  shiny::testServer(block[["expr_server"]], {
    session$setInputs(tray_opened = TRUE)

    # The Run button and Cmd-Enter both send the text to `fn_exec`.
    session$setInputs(fn_code = "n <- 6", fn_exec = "n <- 6")
    expect_identical(session$returned$state$script(), script)
    expect_match(output$footer_ui$html, "no result")
    expect_match(output$footer_ui$html, "disabled")

    session$setInputs(fn_code = "head(data,", fn_exec = "head(data,")
    expect_identical(session$returned$state$script(), script)
    expect_match(output$footer_ui$html, "does not parse: Line 2")

    session$setInputs(fn_code = "utils::head(data, 2)")
    expect_match(output$footer_ui$html, "Edited, not run")
    # the button names the editor whose text it runs, and is no Shiny input
    expect_match(output$footer_ui$html, "data-editor=\"[^\"]*fn_code\"")
    expect_no_match(output$footer_ui$html, "action-button[^>]*btn--run")
    session$setInputs(fn_exec = "utils::head(data, 2)")
    expect_identical(session$returned$state$script(), "utils::head(data, 2)")
    # the user's own Run opens no review
    expect_no_match(output$footer_ui$html, "Accept all")
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("Cmd-Enter on unchanged code does not hide the next outside write", {
  script <- "n <- 3\n\nutils::head(data, n)"
  block <- new_script_block(script)
  shiny::testServer(block[["expr_server"]], {
    session$setInputs(tray_opened = TRUE, fn_code = script)
    session$setInputs(fn_exec = script)

    session$returned$state$script("utils::head(data, 1)")
    session$flushReact()
    expect_match(output$footer_ui$html, "Accept all")
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("the review of an outside write opens and closes", {
  script <- "n <- 3\n\nutils::head(data, n)"
  open_review <- function(session) {
    session$returned$state$script(paste0(session$returned$state$script(), "\n"))
    session$flushReact()
  }
  block <- new_script_block(script)
  shiny::testServer(block[["expr_server"]], {
    session$setInputs(tray_opened = TRUE, fn_code = script)
    expect_no_match(output$footer_ui$html, "Accept all")

    # an outside write opens it, with the number of edits once reported
    open_review(session)
    expect_match(output$footer_ui$html, "Review change")
    expect_match(output$footer_ui$html, "Reject all")
    session$setInputs(fn_nedits = 2)
    expect_match(output$footer_ui$html, "2 edits")
    session$setInputs(fn_nedits = 1)
    expect_match(output$footer_ui$html, "1 edit<")

    # Accept all closes it and keeps the new script
    session$setInputs(accept_all = 1)
    expect_no_match(output$footer_ui$html, "Accept all")
    expect_identical(session$returned$state$script(), paste0(script, "\n"))

    # Reject all closes it; the client then runs the original
    open_review(session)
    expect_match(output$footer_ui$html, "Accept all")
    session$setInputs(reject_all = 1)
    expect_no_match(output$footer_ui$html, "Accept all")
    session$setInputs(fn_exec = script, fn_code = script)
    expect_identical(session$returned$state$script(), script)
    # a run the editor committed is not an outside write
    expect_no_match(output$footer_ui$html, "Accept all")

    # the client reports the last hunk resolved
    open_review(session)
    expect_match(output$footer_ui$html, "Accept all")
    session$setInputs(fn_review_done = 1)
    expect_no_match(output$footer_ui$html, "Accept all")
  }, args = list(`...args` = list(data = shiny::reactive(iris))))
})

test_that("a script written while the input cannot be read gets its controls", {
  open <- shiny::reactiveVal(TRUE)
  block <- new_script_block("n <- 3\n\nutils::head(data, n)")
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    expect_identical(session$returned$expr(), quote(utils::head(.(data), 3)))

    open(FALSE)
    session$flushReact()
    session$returned$state$script(paste(
      "n <- nrow(data) / 2",
      "k <- 4",
      "pick <- factor(\"setosa\", levels(data$Species))",
      "utils::head(data[data$Species == pick, ], n + k)",
      sep = "\n"
    ))
    session$flushReact()
    # `n` is code now, `k` and `pick` are controls read from the last data
    expect_identical(
      session$returned$expr(),
      quote({
        n <- nrow(.(data)) / 2
        utils::head(.(data)[.(data)$Species == "setosa", ], n + 4)
      })
    )
    shown <- session$returned$expr()
    open(TRUE)
    session$flushReact()
    expect_identical(session$returned$expr(), shown)
  }, args = list(`...args` = list(data = gated(open))))
})

test_that("nothing is published before the input has been read once", {
  open <- shiny::reactiveVal(FALSE)
  block <- new_script_block(
    "pick <- factor(data$Species[1], levels(data$Species))\n\nsubset(data, Species == pick)",
    values = list(pick = "virginica")
  )
  seen <- list()
  shiny::testServer(block[["expr_server"]], {
    shiny::observe(seen[[length(seen) + 1L]] <<- session$returned$expr())
    session$flushReact()
    expect_length(seen, 0L)
    expect_error(session$returned$expr(), class = "shiny.silent.error")
    expect_identical(session$returned$state$values(), list(pick = "virginica"))

    session$setInputs(tray_opened = TRUE)
    expect_match(output$footer_ui$html, "waiting for an input")

    open(TRUE)
    session$flushReact()
    expect_identical(
      seen,
      list(quote(subset(.(data), Species == "virginica")))
    )
    expect_identical(session$returned$state$values(), list(pick = "virginica"))
  }, args = list(`...args` = list(data = gated(open))))
})

test_that("a dropdown's choices follow the pick in an earlier control", {
  d <- data.frame(Species = c("x", "y"), Site = c("b", "a"))
  block <- new_script_block(paste(
    "col <- factor(\"Species\", names(data))",
    "val <- factor(character(0), sort(unique(data[[col]])))",
    "data[data[[col]] %in% val, ]",
    sep = "\n"
  ))
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    expect_match(output$dynamic_params$html, "[&quot;x&quot;,&quot;y&quot;]", fixed = TRUE)
    session$setInputs(cb_val = list("x"))

    session$setInputs(cb_col = "Site")
    expect_match(output$dynamic_params$html, "[&quot;a&quot;,&quot;b&quot;]", fixed = TRUE)
    # the pick from the other column is not a choice any more
    expect_identical(session$returned$state$values()$val, "x")
    expect_identical(session$returned$expr()[[3L]][[3L]], character())
  }, args = list(`...args` = list(data = shiny::reactive(d))))
})

test_that("a pick whose level left the data falls back to the script's value", {
  d <- shiny::reactiveVal(data.frame(g = c("a", "b")))
  block <- new_script_block(
    "grp <- factor(\"a\", sort(unique(data$g)))\n\ndata[data$g == grp, , drop = FALSE]",
    values = list(grp = "b")
  )
  shiny::testServer(block[["expr_server"]], {
    session$flushReact()
    expect_identical(
      session$returned$expr(),
      quote(.(data)[.(data)$g == "b", , drop = FALSE])
    )
    d(data.frame(g = c("a", "c")))
    session$flushReact()
    expect_identical(
      session$returned$expr(),
      quote(.(data)[.(data)$g == "a", , drop = FALSE])
    )
    expect_match(output$dynamic_params$html, "data-selected=\"&quot;a&quot;\"", fixed = TRUE)
    # the rebuilt control reports the script's value; the stale pick goes
    session$setInputs(cb_grp = "a")
    expect_identical(session$returned$state$values(), list())
  }, args = list(`...args` = list(data = d)))
})

test_that("new rows update a chip's size without building the chips again", {
  d <- shiny::reactiveVal(iris[1:3, 1:2])
  block <- new_script_block("utils::head(data, 2)")
  shiny::testServer(block[["expr_server"]], {
    session$setInputs(tray_opened = TRUE)
    chips <- output$inputs_ui$html
    expect_match(chips, "3 \u00d7 2", fixed = TRUE)
    expect_identical(output$chip_dims_1, "3 \u00d7 2")

    # The chips are an output of the row above the editor. Were they built
    # again, their markup would hold the new size.
    d(iris[1:7, 1:2])
    session$flushReact()
    expect_identical(output$inputs_ui$html, chips)
    expect_identical(output$chip_dims_1, "7 \u00d7 2")

    # a new column is a new chip list
    d(iris[1:7, 1:3])
    session$flushReact()
    expect_match(output$inputs_ui$html, "Petal.Length", fixed = TRUE)
    expect_match(output$inputs_ui$html, "7 \u00d7 3", fixed = TRUE)
  }, args = list(`...args` = list(data = d)))
})

test_that("an input that cannot be read keeps what its chip lists", {
  shape <- sb_split_info(sb_input_info(list(data = iris)))$shape
  waiting <- sb_split_info(sb_input_info(list(data = sb_unavailable)), shape)
  expect_identical(waiting$shape, shape)
  expect_identical(waiting$dims, "waiting")
  expect_null(shape[[1L]]$dims)
})

test_that("a control named like an input is named in the footer", {
  p <- cb_parse("z <- 3\nutils::head(data, z)")
  specs <- cb_specs(p, list(data = iris, z = iris))
  expect_match(
    sb_rest_label(specs, p, 2L, list(), c("data", "z")),
    "z is an input and a control"
  )
  expect_identical(
    cb_expr(p, specs, data_name = c(data = "data", z = "z")),
    quote(utils::head(.(data), 3))
  )
})
