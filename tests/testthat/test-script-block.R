# Test script_block: the script-with-declared-inputs block.

testServer <- shiny::testServer

specs_for <- function(script, data = iris) {
  cb_specs(cb_parse(script), list(data = data))
}

expr_for <- function(script, values = list(), data = iris) {
  p <- cb_parse(script)
  cb_expr(p, cb_specs(p, list(data = data)), values)
}


# ---- classification -------------------------------------------------------

test_that("a plain-value assignment is an input, anything else is code", {
  p <- cb_parse(paste(
    "n <- 6",
    'label <- "total"',
    "desc <- TRUE",
    'when <- as.Date("2026-01-01")',
    'pick <- factor("a", c("a", "b"))',
    'keep <- c("a", "b")',
    "neg <- -1",
    "tmp <- data |> dplyr::filter(TRUE)",
    "half <- nrow(data) / 2",
    "twice <- 6L * 2",
    "f <- function(x) x + 1",
    "utils::head(tmp)",
    sep = "\n"
  ))

  expect_true(p$ok)
  is_input <- vapply(p$stmts, `[[`, logical(1L), "input")
  names(is_input) <- vapply(p$stmts, `[[`, character(1L), "name")

  expect_equal(
    names(is_input)[is_input],
    c("n", "label", "desc", "when", "pick", "keep", "neg")
  )
  expect_equal(sum(!is_input), 5L)
})

test_that("input lines carry their source line number", {
  p <- cb_parse("n <- 6\n\nlabel <- \"x\"\n\nutils::head(data, n)")
  lines <- vapply(p$stmts, `[[`, integer(1L), "line")
  expect_equal(lines, c(1L, 3L, 5L))
})

test_that("a parse error is reported, not thrown", {
  p <- cb_parse("data |> dplyr::filter(")
  expect_false(p$ok)
  expect_type(p$error, "character")
})


# ---- widgets --------------------------------------------------------------

test_that("a factor is a select whose levels are the choices", {
  s <- specs_for('x <- factor("setosa", unique(data$Species))')[[1L]]
  expect_equal(s$kind, "select")
  expect_equal(s$choices, c("setosa", "versicolor", "virginica"))
  expect_false(s$multiple)
  expect_equal(s$default, "setosa")
})

test_that("multiplicity comes from the declaration's length", {
  s <- specs_for('x <- factor(c("setosa", "virginica"), unique(data$Species))')[[1L]]
  expect_true(s$multiple)
  expect_equal(s$default, c("setosa", "virginica"))
  # ... and stays multiple when the live value narrows to one pick, or the
  # control would change type under the user.
  expect_true(specs_for('x <- factor(c("a", "b"), c("a", "b", "c"))')[[1L]]$multiple)
})

test_that("a factor also carries a fixed option list, no data needed", {
  s <- specs_for('m <- factor("pearson", c("pearson", "spearman"))')[[1L]]
  expect_equal(s$kind, "select")
  expect_equal(s$choices, c("pearson", "spearman"))
})

test_that("literals map to their widgets", {
  kinds <- vapply(
    specs_for('n <- 6\nlabel <- "total"\ndesc <- TRUE\nd <- as.Date("2026-01-01")'),
    `[[`, character(1L), "kind"
  )
  expect_equal(kinds, c("number", "text", "flag", "date"))
})

test_that("a bare vector is the value, so the pool is that vector", {
  s <- specs_for('keep <- c("a", "b")')[[1L]]
  expect_equal(s$kind, "select")
  expect_true(s$multiple)
  expect_equal(s$choices, c("a", "b"))
  expect_equal(s$default, c("a", "b"))
})

test_that("a declaration can read a helper line above it, lazily", {
  # The pipeline in between must NOT be evaluated to get there.
  script <- paste(
    ".lv <- unique(data$Species)",
    '.boom <- stop("should never run")',
    'x <- factor("setosa", .lv)',
    sep = "\n"
  )
  s <- specs_for(script)
  expect_length(s, 1L)
  expect_equal(s[[1L]]$choices, c("setosa", "versicolor", "virginica"))
})

test_that("a declaration that errors becomes a reported spec, not a crash", {
  s <- specs_for('x <- factor("a", stop("boom"))')[[1L]]
  expect_true(is.na(s$kind))
  expect_type(s$error, "character")

  # A mistyped column yields a factor with no levels, which would otherwise be
  # an empty dropdown hiding the mistake.
  empty <- specs_for('x <- factor("a", unique(data$nope))')[[1L]]
  expect_true(is.na(empty$kind))
  expect_match(empty$error, "no choices")
})


# ---- annotations ----------------------------------------------------------

test_that("the #| annotation carries what a value cannot say", {
  expect_equal(cb_annotation("n <- 6  #| number(min = 1, max = 50)"),
               list(min = 1, max = 50))
  expect_equal(cb_annotation("n <- 6  #| min = 1, max = 50"),
               list(min = 1, max = 50))
  expect_equal(cb_annotation('n <- 6  #| label = "Rows"'), list(label = "Rows"))
  expect_equal(cb_annotation("n <- 6"), list())
  expect_equal(cb_annotation("n <- 6  #| nonsense ="), list())
})

test_that("annotation values reach the spec", {
  s <- specs_for('n <- 6   #| number(min = 1, max = 50, label = "Rows")')[[1L]]
  expect_equal(s$min, 1)
  expect_equal(s$max, 50)
  expect_equal(s$label, "Rows")
})


# ---- the expression -------------------------------------------------------

test_that("values are spliced in as literals and the data slot is used", {
  e <- expr_for(
    'species <- factor("setosa", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  )
  expect_equal(e, bquote(dplyr::filter(.(data_slot()), Species == "setosa")))
  expect_false(has_bare_data(e))
})

test_that("the expression carries no assignment, so no local() wrapper", {
  # This is the whole point: blockr.code wraps in local() when it finds an
  # assignment, which is what made the function block's export unreadable.
  e <- expr_for("n <- 6\n\nutils::head(data, n)")
  expect_equal(e, bquote(utils::head(.(data_slot()), 6)))
  expect_false(any(all.names(e) %in% c("<-", "=", "function")))
})

test_that("live values override the declaration's defaults", {
  script <- 'species <- factor("setosa", unique(data$Species))\nn <- 6\n\ndplyr::slice_head(dplyr::filter(data, Species %in% species), n = n)'
  e <- expr_for(script, values = list(species = c("setosa", "virginica"), n = 3))
  expect_equal(
    e,
    bquote(dplyr::slice_head(
      dplyr::filter(.(data_slot()), Species %in% .(c("setosa", "virginica"))),
      n = 3
    ))
  )
})

test_that("the expression evaluates to the same thing the script means", {
  e <- expr_for("n <- 3\n\nutils::head(data, n)")
  expect_equal(eval_bquoted(e, iris), utils::head(iris, 3))
})

test_that("a checkbox folds its branch away", {
  script <- paste(
    "desc <- TRUE",
    "",
    "dplyr::arrange(data, if (desc) dplyr::desc(Sepal.Length) else Sepal.Length)",
    sep = "\n"
  )
  expect_equal(
    expr_for(script, values = list(desc = TRUE)),
    bquote(dplyr::arrange(.(data_slot()), dplyr::desc(Sepal.Length)))
  )
  expect_equal(
    expr_for(script, values = list(desc = FALSE)),
    bquote(dplyr::arrange(.(data_slot()), Sepal.Length))
  )
})

test_that("an else-less branch is dropped from a + chain, not left as NULL", {
  script <- paste(
    "trend <- TRUE",
    "",
    "ggplot2::ggplot(data) + ggplot2::geom_point() + if (trend) ggplot2::geom_smooth()",
    sep = "\n"
  )
  on_expr <- expr_for(script, values = list(trend = TRUE))
  expect_true("geom_smooth" %in% all.names(on_expr))

  off_expr <- expr_for(script, values = list(trend = FALSE))
  expect_false("geom_smooth" %in% all.names(off_expr))
  expect_false("if" %in% all.names(off_expr))
  expect_equal(
    off_expr,
    bquote(ggplot2::ggplot(.(data_slot())) + ggplot2::geom_point())
  )
})

test_that("a Date is emitted as as.Date(), not as a structure() blob", {
  e <- expr_for('cutoff <- as.Date("2026-01-01")\n\ndplyr::filter(data, when > cutoff)')
  expect_equal(
    e,
    bquote(dplyr::filter(.(data_slot()), when > as.Date("2026-01-01")))
  )
  expect_false("structure" %in% all.names(e))
})

test_that("only value positions are substituted", {
  # `n` as an argument NAME and as the head of dplyr::n() must survive; only
  # the value reference is replaced.
  e <- expr_for("n <- 6\n\ndplyr::summarise(data, n = dplyr::n(), k = n)")
  expect_equal(
    e,
    bquote(dplyr::summarise(.(data_slot()), n = dplyr::n(), k = 6))
  )
})

test_that("a name the body assigns stays code and offers no control", {
  script <- "n <- 6\n\nn <- nrow(data)\nutils::head(data, n)"
  p <- cb_parse(script)
  expect_equal(attr(cb_demoted(p), "reason"), c("assigned", "", ""))

  e <- cb_expr(p, cb_specs(p, list(data = iris)), list())
  # The declaration is kept as a statement, so the script still means what it
  # says; nothing is silently rewritten.
  expect_true("<-" %in% all.names(e))
  expect_equal(eval_bquoted(e, iris), utils::head(iris, nrow(iris)))
})

test_that("a dotted name is scaffolding, whatever it holds", {
  script <- paste(
    '.keep <- c("Sepal.Length", "Nope")',
    ".keep <- intersect(.keep, names(data))",
    "",
    "col <- factor(.keep, levels = .keep)",
    "",
    "data[, as.character(col), drop = FALSE]",
    sep = "\n"
  )
  p <- cb_parse(script)
  specs <- cb_specs(p, list(data = iris))
  expect_equal(vapply(specs, `[[`, character(1L), "name"), "col")

  # `.keep <- f(.keep)` is ordinary R and reads the OLD `.keep`; without
  # carrying the previous binding in, the line reads itself and `col` dies of
  # infinite recursion.
  expect_null(specs[[1L]]$error)
  expect_equal(specs[[1L]]$choices, "Sepal.Length")

  # The editor paints exactly the lines that became controls.
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 4)
})

test_that("a dotted name is never a control, whatever it is assigned", {
  script <- paste(
    ".n <- 6",
    '.pick <- factor("setosa", unique(data$Species))',
    'label <- "total"',
    "utils::head(data, .n)",
    sep = "\n"
  )
  expect_equal(vapply(specs_for(script), `[[`, character(1L), "name"), "label")
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 3)

  # Still an ordinary variable: the body reads it and the line survives.
  e <- expr_for(script)
  expect_identical(e[[2L]], quote(.n <- 6))
  expect_equal(eval_bquoted(e, iris), utils::head(iris, 6))
})

test_that("controls can sit below a preamble that derives from the data", {
  # Work out the grouping column from the data, then declare the controls
  # under it. Position does not decide anything: the declarations are the
  # factor lines, wherever they are.
  script <- paste(
    "group_var <- names(data)[[5L]]",
    "if (!group_var %in% names(data)) {",
    "  group_var <- names(data)[[1L]]",
    "}",
    "group_levels <- sort(unique(as.character(data[[group_var]])))",
    "",
    'stats <- factor("n", levels = c("n", "mean"))',
    "`Pool members` <- factor(character(0), levels = group_levels)",
    "",
    "data[data[[group_var]] %in% as.character(`Pool members`), , drop = FALSE]",
    sep = "\n"
  )
  s <- specs_for(script)
  expect_equal(vapply(s, `[[`, character(1L), "name"), c("stats", "Pool members"))
  expect_equal(s[[2L]]$choices, c("setosa", "versicolor", "virginica"))
  expect_true(s[[2L]]$multiple)
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), c(7, 8))
})

test_that("a preamble statement a declaration depends on is run", {
  # `if` cannot be bound lazily, so it has to happen before the declaration
  # below it is evaluated. Otherwise the fallback the author wrote never
  # applies and the control comes back as an error.
  script <- paste(
    'group_var <- "NOPE"',
    "if (!group_var %in% names(data)) {",
    '  group_var <- "Species"',
    "}",
    "pick <- factor(character(0), levels = unique(data[[group_var]]))",
    "data[data$Species %in% as.character(pick), ]",
    sep = "\n"
  )
  s <- specs_for(script)
  # `group_var` is a literal, so it is a control in its own right, and the
  # `if` reassigns it, so it is demoted.
  expect_equal(vapply(s, `[[`, character(1L), "name"), "pick")
  expect_null(s[[1L]]$error)
  expect_equal(s[[1L]]$choices, c("setosa", "versicolor", "virginica"))
})

test_that("a choice list the data decides is a factor over a private helper", {
  # The common shape: candidate columns written down, narrowed to what this
  # data has, offered as a multi-select. The narrowing line is scaffolding, so
  # it starts with a dot; the factor is the control.
  script <- paste(
    '.vars <- intersect(names(data), c("Species", "Petal.Width", "NOPE"))',
    "vars <- factor(.vars, levels = .vars)",
    "data[, as.character(vars), drop = FALSE]",
    sep = "\n"
  )
  s <- specs_for(script)
  expect_length(s, 1L)
  expect_equal(s[[1L]]$kind, "select")
  expect_true(s[[1L]]$multiple)
  # intersect() keeps the order of its first argument, so the choices are in
  # column order rather than the order they were written down.
  expect_equal(s[[1L]]$choices, c("Petal.Width", "Species"))
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 2)

  expect_equal(
    eval_bquoted(expr_for(script, list(vars = "Species")), iris),
    iris[, "Species", drop = FALSE]
  )
})

test_that("a call that is not on the value list is an ordinary line", {
  # No allowlist of "pool-shaped" calls: intersect() and unique() are code like
  # anything else. The line keeps its meaning, gets no control, and above all
  # is not dropped from the body -- `nrow(dedup)` must still have a `dedup`.
  script <- "dedup <- unique(data)\nnrow(dedup)"
  expect_length(specs_for(script), 0L)
  expect_length(cb_syntactic_marks(script), 0L)
  e <- expr_for(script)
  expect_identical(e[[2L]], bquote(dedup <- unique(.(data_slot()))))
  expect_equal(eval_bquoted(e, iris), nrow(unique(iris)))
})

test_that("a dotted helper gets no control and no band", {
  script <- paste(
    ".lv <- unique(data$Species)",
    'pick <- factor("setosa", .lv)',
    "data[data$Species == pick, ]",
    sep = "\n"
  )
  s <- specs_for(script)
  expect_equal(vapply(s, `[[`, character(1L), "name"), "pick")
  expect_equal(vapply(cb_syntactic_marks(script), `[[`, numeric(1L), "line"), 2)
  # It is the convention working, so the footer does not comment on it.
  expect_match(sb_rest_label(s, cb_parse(script)), "^1 control")
})

test_that("a name declared twice keeps only the last control", {
  script <- paste(
    "n <- 6",
    "n <- 10",
    "utils::head(data, n)",
    sep = "\n"
  )
  p <- cb_parse(script)
  expect_equal(attr(cb_demoted(p), "reason"), c("redeclared", "", ""))

  s <- cb_specs(p, list(data = iris))
  expect_length(s, 1L)
  expect_equal(s[[1L]]$line, 2)
  expect_equal(s[[1L]]$default, 10)
  expect_match(sb_rest_label(s, p), "n declared twice")
  # The first line is not a control, so it has to survive as code.
  expect_identical(expr_for(script)[[2L]], quote(n <- 6))
})

test_that("an assignment target is not substituted, but its index is", {
  expect_identical(cb_subst(quote(n <- n + 1), list(n = 6)), quote(n <- 6 + 1))
  expect_identical(cb_subst(quote(x[n] <- n), list(n = 6)), quote(x[6] <- 6))
})

test_that("multiple body statements are wrapped in a single braced expression", {
  e <- expr_for("n <- 2\n\ntmp <- utils::head(data, n)\nnrow(tmp)")
  expect_identical(e[[1L]], quote(`{`))
  expect_equal(eval_bquoted(e, iris), 2L)
})

test_that("a script with no body yields no expression", {
  expect_null(expr_for("n <- 6"))
})


# ---- coercion -------------------------------------------------------------

test_that("values coming back from widgets are coerced by the declaration", {
  num <- specs_for("n <- 6")[[1L]]
  expect_equal(cb_coerce("12", num), 12)
  expect_null(cb_coerce("NA", num))

  flag <- specs_for("x <- TRUE")[[1L]]
  expect_identical(cb_coerce("FALSE", flag), FALSE)

  sel <- specs_for('x <- factor(c("a", "b"), c("a", "b", "c"))')[[1L]]
  # A JS array arrives as a list.
  expect_equal(cb_coerce(list("a", "c"), sel), c("a", "c"))
})

test_that("a restored value survives the trip back into the expression", {
  script <- 'species <- factor("setosa", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  # A saved board hands the value back as a list (JSON array of one).
  e <- expr_for(script, values = list(species = list("virginica")))
  expect_equal(
    e,
    bquote(dplyr::filter(.(data_slot()), Species == "virginica"))
  )
})


# ---- editor marks ---------------------------------------------------------

test_that("marks are derived without evaluating anything", {
  marks <- cb_syntactic_marks(
    'x <- factor("a", unique(data$nope))\nn <- 6\ntmp <- data |> utils::head()'
  )
  expect_length(marks, 2L)
  expect_equal(vapply(marks, `[[`, integer(1L), "line"), c(1L, 2L))
  expect_equal(vapply(marks, `[[`, character(1L), "kind"), c("select", "number"))
})

test_that("marks are empty while the script does not parse", {
  expect_length(cb_syntactic_marks("data |> dplyr::filter("), 0L)
})


# ---- the block ------------------------------------------------------------

test_that("new_script_block creates a valid block", {
  blk <- new_script_block()
  expect_s3_class(blk, "script_block")
  expect_s3_class(blk, "block")
  expect_identical(attr(blk, "expr_type"), "bquoted")
})

test_that("the block's expression and state come out of the script", {
  script <- "n <- 3\n\nutils::head(data, n)"
  block <- new_script_block(script = script)
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 3))
      )
      expect_equal(session$returned$state$script(), script)
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(datasets::iris))))
  )
})

test_that("knob positions are part of the state, so a board restores them", {
  script <- "n <- 3\n\nutils::head(data, n)"
  block <- new_script_block(script = script, values = list(n = 7))
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(session$returned$state$values(), list(n = 7))
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 7))
      )
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(datasets::iris))))
  )
})

test_that("a control reporting its default leaves the state alone", {
  # Every state change makes blockr.core evaluate the block again. A grid
  # control reports its value once it is drawn; writing the script's own
  # default into `values` then cost every such block a second evaluation on
  # load.
  block <- new_script_block(script = "n <- 3\n\nutils::head(data, n)")
  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      session$setInputs(cb_n = 3)
      expect_identical(session$returned$state$values(), list())

      session$setInputs(cb_n = 5)
      expect_identical(session$returned$state$values(), list(n = 5))

      # Back to the default after a move is a move, and is kept.
      session$setInputs(cb_n = 3)
      expect_identical(session$returned$state$values(), list(n = 3))
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("the editor footer is built only once the tray has been opened", {
  block <- new_script_block(script = "n <- 3\n\nutils::head(data, n)")
  testServer(
    block[["expr_server"]],
    {
      session$flushReact()
      expect_error(output$footer_ui, class = "shiny.silent.error")

      session$setInputs(tray_opened = TRUE)
      expect_match(output$footer_ui$html, "1 control")
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("an external write of the script re-derives the expression", {
  # The path blockr.ai's external_ctrl takes. `session$setInputs` does not
  # reach the block's inner module namespace in testServer, so drive the state
  # reactive directly -- which is what an external write does anyway.
  block <- new_script_block(script = "n <- 3\n\nutils::head(data, n)")
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 3))
      )

      session$returned$state$script("n <- 5\n\nutils::head(data, n)")
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 5))
      )

      # A declaration that disappears takes its stored value with it.
      session$returned$state$script("utils::head(data, 2)")
      session$flushReact()
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 2))
      )
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(datasets::iris))))
  )
})

test_that("the block evaluates end to end through blockr", {
  block <- new_script_block(
    script = 'species <- factor("versicolor", unique(data$Species))\n\ndplyr::filter(data, Species == species)'
  )
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "data.frame")
      expect_true(all(as.character(result$Species) == "versicolor"))
      expect_equal(nrow(result), 50L)
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(datasets::iris))))
  )
})

test_that("the footer says how many controls the script produced", {
  p <- cb_parse("n <- 6\n\nutils::head(data, n)")
  expect_match(sb_rest_label(cb_specs(p, list(data = iris)), p), "^1 control \u00b7 1 input$")

  p0 <- cb_parse("utils::head(data)")
  expect_match(sb_rest_label(cb_specs(p0, list(data = iris)), p0), "^no controls")
})

# ---- hiding a panel must not disturb the expression ------------------------

test_that("a declaration that reads the data survives the data going away", {
  # The visibility gate makes `data()` THROW while a block's panel is hidden,
  # not merely go stale. Recomputing the specs against that absence produced an
  # error spec, cb_expr() dropped the declaration as unusable, and the emitted
  # expression silently lost its substitution -- referring to a symbol nothing
  # defines. Coming back restored the good expression, so the block re-evaluated
  # on every tab switch.
  script <- paste(
    "pick <- factor(data$Species[1], levels(data$Species))",
    "",
    "subset(data, Species == pick)",
    sep = "\n"
  )
  block <- new_script_block(script = script)

  on_screen <- shiny::reactiveVal(TRUE)
  data_slot <- shiny::reactive({
    if (!on_screen()) {
      shiny::req(FALSE)     # what the gate does to a hidden block
    }
    datasets::iris
  })

  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      shown <- session$returned$expr()
      expect_true(is.call(shown))
      # the declaration is inlined as a literal, not left as a bare symbol
      expect_false(any(grepl("pick", deparse(shown), fixed = TRUE)))
      expect_true(any(grepl("setosa", deparse(shown), fixed = TRUE)))

      on_screen(FALSE)      # panel hidden: data() now throws
      session$flushReact()
      expect_equal(session$returned$expr(), shown)

      on_screen(TRUE)       # and shown again
      session$flushReact()
      expect_equal(session$returned$expr(), shown)
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = data_slot)))
  )
})
test_that("specs are kept, not degraded, when the data is unavailable", {
  # The unit the fix turns on: cb_specs() against NULL yields an error spec, so
  # the block must hold the last good one instead of adopting it.
  script <- "pick <- factor(data$Species[1], levels(data$Species))\n\nsubset(data, Species == pick)"
  p <- cb_parse(script)

  good <- cb_specs(p, list(data = datasets::iris))
  degraded <- cb_specs(p, list(data = NULL))

  expect_length(good, 1L)
  expect_null(good[[1L]]$error)
  expect_false(is.na(good[[1L]]$kind))

  # this is what the block must never adopt
  expect_length(degraded, 1L)
  expect_false(is.null(degraded[[1L]]$error))

  # and what it costs if it does: the substitution disappears
  expect_true(any(grepl("setosa", deparse(cb_expr(p, good, list())), fixed = TRUE)))
  expect_true(any(grepl("pick", deparse(cb_expr(p, degraded, list())), fixed = TRUE)))
})

test_that("building the controls runs the header, never the body", {
  # The point of `data` being in scope for a declaration is that a select's
  # choices can come from the data (`factor("F", unique(data$SEX))`). Paying
  # for that with a second run of the block's pipeline would be a bad trade:
  # the pipeline is the expensive part, and it already runs once as the
  # block's expr.
  #
  # Counted through options() rather than a helper function on purpose:
  # cb_specs() evaluates in blockr.core::eval_env(), whose parent is
  # baseenv(), so nothing from the test's own scope would be reachable.
  on.exit(options(cb_body = NULL, cb_helper = NULL), add = TRUE)
  options(cb_body = 0L, cb_helper = 0L)
  bump <- "options(cb_%s = getOption('cb_%s') + 1L)"

  script <- paste(
    "sex <- factor('F', levels = sort(unique(data$SEX)))",
    paste0(".unused <- { ", sprintf(bump, "helper", "helper"), "; c('F', 'M') }"),
    paste0("out <- { ", sprintf(bump, "body", "body"), "; 42 }"),
    "out",
    sep = "\n"
  )

  specs <- cb_specs(
    cb_parse(script),
    list(data = data.frame(SEX = c("F", "M", "F"), stringsAsFactors = FALSE))
  )

  # The declaration was evaluated, and its choices came off the data.
  expect_length(specs, 1L)
  expect_identical(specs[[1L]]$kind, "select")
  expect_identical(specs[[1L]]$choices, c("F", "M"))

  # Nothing else was. The body never ran, and the helper no declaration reads
  # stayed unforced behind its lazy binding (cb_delay).
  expect_identical(getOption("cb_body"), 0L)
  expect_identical(getOption("cb_helper"), 0L)
})

test_that("a helper a declaration actually reads is forced, once", {
  on.exit(options(cb_helper = NULL), add = TRUE)
  options(cb_helper = 0L)

  script <- paste(
    ".lv <- { options(cb_helper = getOption('cb_helper') + 1L); c('a', 'b') }",
    "pick <- factor('a', levels = .lv)",
    "also <- factor('b', levels = .lv)",
    "nrow(data)",
    sep = "\n"
  )

  specs <- cb_specs(cb_parse(script), list(data = data.frame(x = 1)))

  expect_identical(vapply(specs, `[[`, character(1L), "kind"),
                   c("select", "select"))
  # Read by two declarations, evaluated once: the active binding caches.
  expect_identical(getOption("cb_helper"), 1L)
})


# ---- scoping --------------------------------------------------------------

test_that("a formal shadows a control of the same name inside the function", {
  # `function(n)` binds `n` for its body, so the body's `n` is the argument,
  # not the knob. Substituting it would rewrite the lambda.
  e <- expr_for("n <- 3\nsapply(1:2, function(n) n + 1)")
  expect_identical(e, quote(sapply(1:2, function(n) n + 1)))

  # ... including through the backslash lambda, and in a default.
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, \\(n) n + 1)"),
    quote(sapply(1:2, function(n) n + 1))
  )
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, function(x, k = n) x + k)"),
    quote(sapply(1:2, function(x, k = 3) x + k))
  )

  # A formal that does NOT clash leaves the substitution alone.
  expect_identical(
    expr_for("n <- 3\nsapply(1:2, \\(x) x + n)"),
    quote(sapply(1:2, function(x) x + 3))
  )
})

test_that("a formal named data shadows the block's data slot", {
  # The bug this guards: every group returning nrow() of the whole table.
  e <- expr_for("vapply(split(data, data$cyl), function(data) nrow(data), 1L)")
  expect_identical(
    e,
    quote(vapply(split(.(data), .(data)$cyl), function(data) nrow(data), 1L))
  )
})

test_that("a loop variable demotes the declaration it collides with", {
  # Plain R leaves `n` at 2 after the loop, so the script cannot mean a knob.
  p <- cb_parse("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)")
  expect_identical(attr(cb_demoted(p), "reason"), c("assigned", "", ""))

  e <- expr_for("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)")
  expect_identical(e[[2L]], quote(n <- 3))
  expect_identical(e[[3L]], quote(for (n in 1:2) print(n)))

  expect_match(
    sb_rest_label(specs_for("n <- 3\nfor (n in 1:2) print(n)\nhead(data, n)"), p),
    "n assigned in the body"
  )
})


# ---- construction ---------------------------------------------------------

test_that("the constructor warns about a script that cannot be used", {
  expect_warning(
    blk <- new_script_block(script = "head(data,"),
    "does not parse. Line 2"
  )
  expect_s3_class(blk, "script_block")
  expect_warning(new_script_block(script = "n <- 6"), "has no result")
  expect_warning(new_script_block(script = ""), "is empty")
  expect_no_warning(new_script_block())
})

test_that("the footer reports a parse failure of an external write", {
  expect_match(
    sb_rest_label(list(), cb_parse("head(data,")),
    "does not parse"
  )
})

test_that("an empty factor declares a multi-select with nothing picked", {
  # "Pick any number of these, starting from none" has no other spelling: the
  # declaration's length is the only signal, so zero cannot mean single.
  s <- specs_for('x <- factor(character(0), c("a", "b", "c"))')[[1L]]

  expect_identical(s$kind, "select")
  expect_true(s$multiple)
  expect_identical(s$choices, c("a", "b", "c"))
})


# ---- params grid ----------------------------------------------------------

test_that("every field says what kind of control it is", {
  # The band shares out width by kind, so the markup has to carry it. Guessing
  # from the contents would mean `:has()`, which restyles a whole Shiny page.
  html <- as.character(cb_params_ui(
    specs_for('x <- factor(c("a"), c("a", "b"))\nn <- 10\ndesc <- TRUE\nt <- "hi"'),
    identity
  ))

  expect_match(html, "blockr-script-field--select", fixed = TRUE)
  expect_match(html, "blockr-script-field--number", fixed = TRUE)
  expect_match(html, "blockr-script-field--flag", fixed = TRUE)
  expect_match(html, "blockr-script-field--text", fixed = TRUE)
})


test_that("the band steps down the ladder its contents need", {
  # A band with a select is laid out for the select: at a half-width panel that
  # is two columns where the knob ladder would still be showing four.
  expect_identical(sb_grid_track(c("select", "number", "flag"))$class, "blockr-script-band--wide")
  expect_identical(sb_grid_track(c("text", "number"))$class, "blockr-script-band--wide")
  expect_identical(sb_grid_track(c("number", "flag", "date"))$class, "blockr-script-band--knobs")

  # The counts stay clamped to the field count, so a two-field band is never
  # widened into a row with an empty trailing track.
  expect_identical(sb_grid_track(rep("number", 7L))$style,
                   "--blockr-script-cols:4; --blockr-script-cols-3:3; --blockr-script-cols-2:2;")
  expect_identical(sb_grid_track(c("number", "flag"))$style,
                   "--blockr-script-cols:2; --blockr-script-cols-3:2; --blockr-script-cols-2:2;")
})


test_that("a checkbox field carries a spacer where its label row would be", {
  # A checkbox labels itself beside the box, so without the spacer it starts a
  # label's height above the fields either side of it.
  html <- as.character(cb_params_ui(specs_for("desc <- TRUE"), identity))

  expect_match(html, "blockr-script-field-label--spacer", fixed = TRUE)
})


test_that("a generated multi-select asks for one row of tags", {
  # The grid sizes a row to its tallest field, so a select that wraps its tags
  # is what makes the whole band tall. `data-single-line` is how the field
  # asks blockr.dplyr's select to keep one row and count the rest on a chip.
  spec <- specs_for('x <- factor(c("a", "b"), c("a", "b", "c"))')[[1L]]
  html <- as.character(cb_input_ui(spec, identity))

  expect_match(html, 'data-single-line="true"', fixed = TRUE)
  expect_match(html, 'data-multiple="true"', fixed = TRUE)
})


# ---- script as lines ------------------------------------------------------

test_that("lines and text are the same script, both ways round", {
  expect_identical(cb_script_text(c("a", "b")), "a\nb")
  expect_identical(cb_script_text("a\nb"), "a\nb")
  expect_identical(cb_script_lines("a\nb"), c("a", "b"))

  # A trailing newline survives, which a bare strsplit() would eat.
  for (script in c("", "a", "a\nb", "a\nb\n", "a\n\nb", "\n", "a\n\n")) {
    expect_identical(cb_script_text(cb_script_lines(script)), script)
  }
})

test_that("the constructor takes a script written as lines", {
  lines <- c("n <- 3", "", "utils::head(data, n)")
  block <- new_script_block(script = lines)
  expect_s3_class(block, "script_block")
  expect_identical(blockr_ser(block)$payload$script, lines)

  # And still takes the one string every saved board holds today.
  expect_identical(
    blockr_ser(new_script_block(script = paste(lines, collapse = "\n")))$payload,
    blockr_ser(block)$payload
  )
})

test_that("a saved board writes the script as lines", {
  block <- new_script_block(script = "n <- 6\n\nutils::head(data, n)")
  ser <- blockr_ser(block)
  expect_identical(ser$payload$script, c("n <- 6", "", "utils::head(data, n)"))

  # A one-line script has nothing to split, so it stays a plain JSON string.
  one <- blockr_ser(new_script_block(script = "utils::head(data, 2)"))
  expect_identical(one$payload$script, "utils::head(data, 2)")
})

test_that("both serialized forms deserialize to the same block", {
  script <- "n <- 6\n\nutils::head(data, n)"
  ser <- blockr_ser(new_script_block(script = script, values = list(n = 4)))

  # What boards saved before the change hold.
  legacy <- ser
  legacy$payload$script <- script

  expect_identical(blockr_ser(blockr.core::blockr_deser(legacy)), ser)
  expect_identical(blockr_ser(blockr.core::blockr_deser(ser)), ser)
})

test_that("an external write of lines is collapsed into the script", {
  block <- new_script_block(script = "n <- 3\n\nutils::head(data, n)")
  testServer(
    blockr.core::get_s3_method("block_server", block),
    {
      session$flushReact()

      session$returned$state$script(c("n <- 5", "", "utils::head(data, n)"))
      session$flushReact()

      expect_identical(
        session$returned$state$script(),
        "n <- 5\n\nutils::head(data, n)"
      )
      expect_equal(
        session$returned$expr(),
        bquote(utils::head(.(data_slot()), 5))
      )
    },
    args = list(x = block, data = list(`...args` = reactives::reactive_exprs(data = shiny::reactive(datasets::iris))))
  )
})

test_that("an assignment whose right side folds away binds NULL", {

  # `x <- if (FALSE) 1` is valid R that assigns NULL. Pruning the dropped
  # branch with `e[[i]] <- NULL` removed the argument instead of setting it,
  # leaving a one-argument `<-` that deparses as `x <- NULL` and throws
  # `incorrect number of arguments to "<-"` when the block runs. The script
  # was fine, so no edit to it could help.
  out <- cb_fold(quote(x <- if (FALSE) 1))

  expect_length(out, 3L)
  expect_null(eval(out))
  expect_equal(eval(quote(x), environment()), NULL)
})

test_that("a dropped argument elsewhere becomes NULL, not a missing argument", {

  out <- cb_fold(quote(f(a, if (FALSE) b)))

  expect_length(out, 3L)
  expect_null(out[[3L]])

  # The shapes that absorb a dropped operand keep doing so.
  expect_equal(cb_fold(quote(p + if (FALSE) g())), quote(p))
  expect_equal(cb_fold(quote(if (TRUE) 1 else 2)), 1)
})
