test_that("is_html_renderable accepts HTML objects and rejects plain values", {
  expect_true(is_html_renderable(htmltools::tags$b("x")))
  expect_true(is_html_renderable(htmltools::tagList("x")))
  expect_true(is_html_renderable(htmltools::HTML("<b>x</b>")))
  expect_true(is_html_renderable(structure(list(), class = "htmlwidget")))
  expect_true(is_html_renderable(html_page("x")))
  for (x in list("text", 1, 1L, TRUE, list(1), mtcars, mean, NULL, factor("a"),
                 quote(a), quote(a + b), new.env())) {
    expect_false(is_html_renderable(x))
  }
  expect_false(is_html_renderable(structure(list(), class = "zz_no_method")))
})

test_that("is_html_renderable ignores the class default and the search path", {
  expect_false(is_html_renderable(structure(list(), class = "default")))

  # A method in an attached environment does not count.
  attached <- attach(
    list(as.tags.zz_attached = function(x, ...) htmltools::tags$b("x")),
    name = "zz_blockr_script_test"
  )
  on.exit(detach("zz_blockr_script_test"), add = TRUE)
  expect_false(is_html_renderable(structure(list(), class = "zz_attached")))

  # A method sourced into the global environment does.
  assign("as.tags.zz_global", function(x, ...) htmltools::tags$b("x"), globalenv())
  on.exit(rm("as.tags.zz_global", envir = globalenv()), add = TRUE)
  expect_true(is_html_renderable(structure(list(), class = "zz_global")))
})

test_that("gt tables are HTML, a kable is not until it is rendered", {
  skip_if_not_installed("gt")
  expect_true(is_html_renderable(gt::gt(head(mtcars, 2))))
  skip_if_not_installed("knitr")
  expect_false(is_html_renderable(knitr::kable(head(mtcars, 2), format = "html")))
})

# Render a result the way the block does and return the HTML of its output.
rendered <- function(result) {
  out <- NULL
  shiny::testServer(
    function(input, output, session) {
      output$result <- render_dynamic_output(result, NULL, session)
    },
    {
      session$flushReact()
      value <- output$result
      out <<- if (is.null(value)) NULL else paste(as.character(value$html), collapse = "")
    }
  )
  out
}

test_that("NULL renders nothing", {
  expect_null(rendered(NULL))
})

test_that("values without a branch print as text, in the text class", {
  html <- rendered(1:3)
  expect_match(html, "<pre class=\"blockr-script-text-output\">[1] 1 2 3</pre>", fixed = TRUE)
  expect_false(grepl("style=", html, fixed = TRUE))
  expect_match(rendered("a <b>"), "[1] \"a &lt;b&gt;\"", fixed = TRUE)
  expect_match(rendered(list(a = 1)), "$a", fixed = TRUE)
  fit <- lm(mpg ~ wt, mtcars)
  expect_match(rendered(fit), "Coefficients", fixed = TRUE)
})

test_that("the text class travels with its style sheet", {
  deps <- htmltools::findDependencies(text_output(1))
  expect_identical(deps[[1]]$name, "blockr-script-css")
})

test_that("printed output renders as the text it is", {
  out <- eval_with_plot_capture(quote(cat("hello\n", "world\n")), new.env())
  expect_s3_class(out, "script_output")
  expect_match(rendered(out), ">hello\n world</pre>", fixed = TRUE)
})

test_that("HTML objects render through as.tags", {
  expect_match(rendered(htmltools::tags$b("bold")), "<b>bold</b>", fixed = TRUE)
  expect_match(rendered(html_page("x")), "<iframe class=\"blockr-html-page\"", fixed = TRUE)
})

test_that("an as.tags method that fails falls back to text", {
  assign("as.tags.zz_broken", function(x, ...) stop("no"), globalenv())
  assign("print.zz_broken", function(x, ...) cat("printed zz\n"), globalenv())
  on.exit(rm("as.tags.zz_broken", "print.zz_broken", envir = globalenv()))
  html <- rendered(structure(list(), class = "zz_broken"))
  expect_match(html, "<pre class=\"blockr-script-text-output\">printed zz</pre>", fixed = TRUE)
})

test_that("an HTML kable renders as a table, a markdown kable as text", {
  skip_if_not_installed("knitr")
  html <- rendered(knitr::kable(head(mtcars, 2), format = "html"))
  expect_match(html, "<table", fixed = TRUE)
  expect_false(grepl("&lt;table", html, fixed = TRUE))
  expect_match(rendered(knitr::kable(head(mtcars, 2), format = "pipe")), "<pre", fixed = TRUE)
})

test_that("plots of every kind render as a plot output", {
  rec <- eval_with_plot_capture(quote(plot(1:3)), new.env())
  expect_s3_class(rec, "recordedplot")
  expect_match(rendered(rec), "shiny-plot-output", fixed = TRUE)

  skip_if_not_installed("ggplot2")
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_match(rendered(gg), "shiny-plot-output", fixed = TRUE)
  expect_match(rendered(grid::rectGrob()), "shiny-plot-output", fixed = TRUE)
  expect_match(rendered(grid::grobTree(grid::rectGrob())), "shiny-plot-output", fixed = TRUE)
  expect_match(rendered(structure(list(), class = "gtable")), "shiny-plot-output", fixed = TRUE)
})

test_that("a lattice plot renders as a plot and writes no Rplots.pdf", {
  skip_if_not_installed("lattice")
  dir <- tempfile()
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old))
  tr <- eval_with_plot_capture(quote(lattice::xyplot(mpg ~ wt, mtcars)), new.env())
  expect_s3_class(tr, "trellis")
  expect_match(rendered(tr), "shiny-plot-output", fixed = TRUE)
  expect_false(file.exists("Rplots.pdf"))
})

test_that("a gt table gets the house style, a gtsummary table becomes gt", {
  skip_if_not_installed("gt")
  tab <- gt::gt(head(mtcars, 2))
  styled <- gt_house_style(tab)
  opts <- styled[["_options"]]
  value <- function(name) opts$value[[which(opts$parameter == name)]]
  expect_identical(value("table_width"), "100%")
  expect_identical(value("table_font_names"), "inherit")
  expect_identical(value("table_font_size"), "inherit")
  # Anything that is not a gt table comes back as it was.
  expect_identical(gt_house_style(mtcars), mtcars)
  expect_match(rendered(tab), "gt_table", fixed = TRUE)

  skip_if_not_installed("gtsummary")
  html <- rendered(gtsummary::tbl_summary(mtcars[c("mpg", "cyl")]))
  expect_match(html, "gt_table", fixed = TRUE)
})

test_that("a data frame goes to blockr.ui's table", {
  seen <- NULL
  local_mocked_bindings(
    html_table_result = function(result, block, session) {
      seen <<- result
      shiny::renderUI("table")
    },
    .package = "blockr.ui"
  )
  expect_match(rendered(head(mtcars, 2)), "table", fixed = TRUE)
  expect_identical(seen, head(mtcars, 2))
})

test_that("block_result_output dispatches on the result", {
  expect_identical(
    environment(block_result_output.default)$render_dynamic_output,
    render_dynamic_output
  )
  registerS3method(
    "block_result_output", "zz_mine",
    function(result, block, session, ...) shiny::renderUI("mine"),
    envir = asNamespace("blockr.script")
  )
  out <- NULL
  shiny::testServer(
    function(input, output, session) {
      output$a <- block_result_output(structure(list(), class = "zz_mine"), NULL, session)
      output$b <- block_result_output(1:2, NULL, session)
    },
    {
      session$flushReact()
      out <<- c(as.character(output$a$html), as.character(output$b$html))
    }
  )
  expect_match(out[1], "mine", fixed = TRUE)
  expect_match(out[2], "[1] 1 2", fixed = TRUE)
})

test_that("eval_with_plot_capture returns the value, a plot or the printed text", {
  env <- list2env(list(data = mtcars, .arg1 = iris))
  expect_identical(eval_with_plot_capture(quote(nrow(data)), env), 32L)
  # dot-prefixed inputs are in scope
  expect_identical(eval_with_plot_capture(quote(nrow(.arg1)), env), 150L)
  # a string the script returns is a value, not printed output
  expect_identical(eval_with_plot_capture(quote("text"), env), "text")
  # an invisible value is no result
  expect_null(eval_with_plot_capture(quote(x <- 1), env))
  expect_null(eval_with_plot_capture(quote(invisible(1)), env))
  # printed output is the result when nothing else is
  out <- eval_with_plot_capture(
    quote({
      print(1:2)
      cat("done\n")
    }),
    env
  )
  expect_identical(unclass(out), "[1] 1 2\ndone\n")
  expect_output(print(out), "^\\[1\\] 1 2\ndone$")
  # a value wins over printed output
  value <- eval_with_plot_capture(
    quote({
      cat("log\n")
      5
    }),
    env
  )
  expect_identical(value, 5)
  # numbers are not rounded by a trip through text
  expect_identical(eval_with_plot_capture(quote(x), list2env(list(x = 1 / 3))), 1 / 3)
  expect_identical(eval_with_plot_capture(bquote(.(1 / 3) * 3), env), 1)
  # the script does not write into the environment it was given
  expect_false(exists("x", envir = env, inherits = FALSE))
})

test_that("eval_with_plot_capture prefers a ggplot, then a drawn plot", {
  env <- new.env()
  both <- eval_with_plot_capture(
    quote({
      plot(1:3)
      7
    }),
    env
  )
  expect_s3_class(both, "recordedplot")
  skip_if_not_installed("ggplot2")
  gg <- eval_with_plot_capture(
    quote({
      plot(1:3)
      ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg))
    }),
    env
  )
  expect_s3_class(gg, "ggplot")
})

test_that("eval_with_plot_capture signals the original error and the warnings", {
  env <- new.env()
  err <- tryCatch(
    eval_with_plot_capture(
      quote(stop(structure(
        class = c("zz_error", "error", "condition"),
        list(message = "boom", call = quote(f(x)))
      ))),
      env
    ),
    error = identity
  )
  expect_s3_class(err, "zz_error")
  expect_identical(conditionMessage(err), "boom")
  expect_identical(conditionCall(err), quote(f(x)))
  expect_error(eval_with_plot_capture(quote(stop("plain")), env), "plain")

  expect_warning(
    expect_identical(
      eval_with_plot_capture(
        quote({
          warning("careful")
          7
        }),
        env
      ),
      7
    ),
    "careful"
  )
  w <- NULL
  withCallingHandlers(
    eval_with_plot_capture(
      quote(warning(structure(
        class = c("zz_warning", "warning", "condition"),
        list(message = "w", call = NULL)
      ))),
      env
    ),
    warning = function(cnd) {
      w <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(w, "zz_warning")
})

test_that("asset versions follow the file contents", {
  v <- js_version("sentence.js")
  expect_match(v, paste0("^", gsub(".", "\\.", utils::packageVersion("blockr.script"), fixed = TRUE), "\\.[0-9]+$"))
  expect_false(identical(v, js_version("gear-editor.js")))
  expect_identical(script_tools_dep()$version, js_version("script-tools.js"))
  expect_identical(script_css_dep()$version, js_version("script-block.css", "css"))
  deps <- prose_block_dep()
  expect_identical(deps[[1]]$version, js_version("prose-block.js"))
  expect_identical(deps[[2]]$version, js_version("prose-block.css", "css"))
})
