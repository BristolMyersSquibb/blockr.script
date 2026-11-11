# What the tray offers while a script is written: names that are no input,
# completions, "+ Control" lines, glyph hover text, and building it lazily.

test_that("a package data frame that is no input is flagged", {
  marks <- sb_foreign_frames("x <- 1\n\nhead(cars, x)", "data")
  expect_length(marks, 1L)
  expect_identical(marks[[1L]]$name, "cars")
  expect_identical(marks[[1L]]$pkg, "datasets")
  expect_identical(marks[[1L]]$line, 3L)
  expect_identical(c(marks[[1L]]$from, marks[[1L]]$to), c(6L, 10L))
  expect_identical(
    marks[[1L]]$title,
    paste(
      "cars is not an input; it comes from package datasets.",
      "Write datasets::cars if you mean it."
    )
  )
})

test_that("inputs, assigned names and qualified names are not flagged", {
  # an input called cars
  expect_length(sb_foreign_frames("head(cars)", c("data", "cars")), 0L)
  # assigned in the script, or a function formal
  expect_length(sb_foreign_frames("cars <- data\nhead(cars)", "data"), 0L)
  expect_length(sb_foreign_frames("f <- function(cars) cars\nf(data)", "data"), 0L)
  # qualified, or a list element
  expect_length(sb_foreign_frames("head(datasets::cars)", "data"), 0L)
  expect_length(sb_foreign_frames("data$cars", "data"), 0L)
  # a column inside a data-masking call is not a data frame on the path
  expect_length(sb_foreign_frames("subset(data, Species == 'a')", "data"), 0L)
  # a function, not a data frame
  expect_length(sb_foreign_frames("head(data)", "data"), 0L)
  # does not parse
  expect_length(sb_foreign_frames("head(cars,", "data"), 0L)
})

test_that("the footer names a foreign data frame", {
  p <- cb_parse("head(cars)")
  foreign <- sb_foreign_frames("head(cars)", "data")
  expect_match(sb_rest_label(list(), p, 1L, foreign), "^cars is not an input")
})

test_that("completions say what a name is", {
  info <- sb_input_info(list(data = data.frame(a = 1, b = "x")))
  comp <- sb_completions(info)
  labels <- vapply(comp, `[[`, character(1L), "label")
  expect_identical(labels, c("data", "a", "b"))
  # no row count, so new rows do not change the list
  expect_identical(comp[[1L]]$detail, "input · 2 columns")
  expect_identical(comp[[3L]]$detail, "chr · data")
})

test_that("+ Control lines become the controls they promise", {
  info <- sb_input_info(list(data = iris))
  tmpl <- sb_control_templates(info)
  expect_identical(tmpl[[1L]]$label, "Dropdown from a column")
  title <- tmpl[[length(tmpl)]]
  expect_identical(title$label, "Title sentence")
  for (t in tmpl[-length(tmpl)]) {
    script <- paste0(t$line, "\n\nutils::head(data)")
    specs <- cb_specs(cb_parse(script), list(data = iris))
    expect_length(specs, 1L)
    expect_null(specs[[1L]]$error)
  }
  # the sentence names the script's first control
  specs <- cb_specs(cb_parse("n <- 3\nutils::head(data, n)"), list(data = iris))
  expect_identical(sb_first_control(specs), "n")
  lines <- strsplit(sb_control_templates(info, "n")[[8L]]$line, "\n")[[1L]]
  expect_identical(lines, c(
    "#| title: Results for {@n}",
    "#| subtitle: {n} rows",
    "#| caption: Source: your note here"
  ))
  # and the sentence reads all three
  tmpl <- sb_sentence_templates(c(lines, "utils::head(data, n)"))
  expect_setequal(names(Filter(Negate(is.null), tmpl)),
                  c("title", "subtitle", "caption"))

  # no input, no column to read
  expect_false(
    "Dropdown from a column" %in%
      vapply(sb_control_templates(list()), `[[`, character(1L), "label")
  )
})

test_that("a glyph's hover text gives the rule", {
  specs <- cb_specs(cb_parse(
    "pick <- factor(\"setosa\", levels(data$Species))\nutils::head(data)"
  ), list(data = iris))
  title <- cb_editor_marks(specs)[[1L]]$title
  expect_match(title, "^pick: dropdown, 3 choices\\. A factor is a dropdown")

  syn <- cb_syntactic_marks("n <- 5\nutils::head(data, n)")[[1L]]$title
  expect_match(syn, "^n: number box\\. A bare number is a number box")
})

test_that("nothing about the inputs is built before the tray opens", {
  block <- new_script_block("utils::head(data, 2)")
  shiny::testServer(
    blockr.core:::block_expr_server(block),
    {
      session$flushReact()
      expect_error(output$inputs_ui)

      session$setInputs(tray_opened = TRUE)
      expect_match(output$inputs_ui$html, "blockr-script-chip")
      expect_match(output$inputs_ui$html, "+ Control", fixed = TRUE)
    },
    args = list(`...args` = list(data = function() iris))
  )
})

test_that("a name inside a data-masking call is not flagged", {
  # `sleep` and `cars` are data frames in datasets, and columns here
  expect_length(sb_foreign_frames("subset(data, sleep > 20)", "data"), 0L)
  expect_length(
    sb_foreign_frames("dplyr::filter(data, is.na(cars), sleep > 2)", "data"),
    0L
  )
  expect_length(
    sb_foreign_frames("data |> dplyr::mutate(x = sleep * 2)", "data"),
    0L
  )
  # also through a variable the script made from an input
  expect_length(
    sb_foreign_frames("tmp <- utils::head(data)\nsubset(tmp, sleep > 20)", "data"),
    0L
  )
  # a column of an input is never flagged, wherever it stands
  expect_length(sb_foreign_frames("data[order(sleep), ]", "data"), 1L)
  expect_length(sb_foreign_frames("data[order(sleep), ]", "data", "sleep"), 0L)
})

test_that("a data frame passed as an argument is still flagged", {
  names_of <- function(x) vapply(x, `[[`, character(1L), "name")
  expect_identical(names_of(sb_foreign_frames("merge(data, cars)", "data")), "cars")
  expect_identical(
    names_of(sb_foreign_frames("rbind(data, y = cars)", "data")),
    "cars"
  )
  expect_identical(
    names_of(sb_foreign_frames("if (nrow(data) > 3) head(cars)", "data")),
    "cars"
  )
  expect_identical(
    names_of(sb_foreign_frames("data$x + cars$speed", "data")),
    "cars"
  )
  # every read of a flagged name is marked
  marks <- sb_foreign_frames("head(cars)\nsubset(data, x > nrow(cars))", "data")
  expect_identical(vapply(marks, `[[`, integer(1L), "line"), c(1L, 2L))
})

test_that("each distinct name is looked up once", {
  rm(list = ls(sb_frame_cache), envir = sb_frame_cache)
  asked <- character()
  testthat::local_mocked_bindings(
    sb_frame_package = function(name, path = search()) {
      asked <<- c(asked, name)
      if (identical(name, "cars")) "datasets" else NA_character_
    }
  )
  script <- "head(cars, x)\nnrow(cars) + x + y\nhead(cars)"
  marks <- sb_foreign_frames(script, "data")
  expect_length(marks, 3L)
  expect_setequal(asked, c("cars", "x", "y"))
  expect_length(asked, 3L)

  # and not again for the next keystroke
  sb_foreign_frames(paste0(script, "\nz"), "data")
  expect_identical(asked[-(1:3)], "z")
  rm(list = ls(sb_frame_cache), envir = sb_frame_cache)
})

test_that("the lookup finds data frames of attached packages only", {
  expect_identical(sb_frame_package("cars"), "datasets")
  expect_identical(sb_frame_package("head"), NA_character_)
  expect_identical(sb_frame_package("no_such_name_here"), NA_character_)
  expect_identical(
    unname(sb_frame_packages(c("cars", "head", "cars"))),
    c("datasets", NA, "datasets")
  )
})

test_that("the footer and the underline come from one scan of the script", {
  scans <- 0L
  real <- sb_foreign_frames
  testthat::local_mocked_bindings(
    sb_foreign_frames = function(...) {
      scans <<- scans + 1L
      real(...)
    }
  )
  block <- new_script_block("head(cars)")
  shiny::testServer(
    block[["expr_server"]],
    {
      session$setInputs(tray_opened = TRUE)
      expect_match(output$footer_ui$html, "cars is not an input")
      expect_identical(scans, 1L)

      # typing scans the editor's text, not the script again
      session$setInputs(fn_code = "head(cars) ")
      output$footer_ui
      expect_identical(scans, 2L)
    },
    args = list(`...args` = list(data = shiny::reactive(iris)))
  )
})
