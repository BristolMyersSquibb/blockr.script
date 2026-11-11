test_that("html mode fills the template and exposes the data", {
  p <- html_page(
    "<h1>{{title}}</h1><ul>{{#rows}}<li>{{name}}</li>{{/rows}}</ul>",
    data = data.frame(name = c("a", "b")),
    title = "T"
  )
  expect_s3_class(p, "html_page")
  expect_match(p$srcdoc, "<h1>T</h1><ul><li>a</li><li>b</li></ul>", fixed = TRUE)
  expect_match(p$srcdoc, "window.blockr={data:[{\"name\":\"a\"}", fixed = TRUE)
  expect_identical(p$sandbox, "allow-scripts")
})

test_that("a closing script tag in the data cannot end the data script", {
  p <- html_page("x", data = data.frame(v = "</script><b>"))
  expect_false(grepl("\"</script>", p$srcdoc, fixed = TRUE))
  expect_match(p$srcdoc, "\\u003c\\/script\\u003e", fixed = TRUE)
})

test_that("a page with its own head keeps it", {
  p <- html_page("<html><head><title>t</title></head><body>b</body></html>")
  expect_match(p$srcdoc, "^<html><head><meta charset")
  expect_match(p$srcdoc, "<title>t</title></head><body>b</body>", fixed = TRUE)
})

test_that("no data gives blockr.data = null", {
  p <- html_page("x")
  expect_match(p$srcdoc, "window.blockr={data:null}", fixed = TRUE)
})

test_that("url mode fills placeholders from data and dots, encoded", {
  u <- html_page(
    url = "https://x.org/s/{id}?site={site}",
    data = data.frame(id = c("10 01", "2"), site = "Basel"),
    site = "Lyon"
  )
  expect_identical(u$url, "https://x.org/s/10%2001?site=Lyon")
  expect_identical(u$height, "600px")
  expect_error(html_page(url = "https://x.org/{nope}"), "No value")
})

test_that("exactly one of html and url", {
  expect_error(html_page(), "exactly one")
  expect_error(html_page("x", url = "y"), "exactly one")
})

test_that("rows beyond max_rows are dropped with a warning", {
  expect_warning(
    p <- html_page("{{nrow}}", data = data.frame(i = 1:10), max_rows = 3),
    "first 3"
  )
  expect_match(p$srcdoc, "<body>3</body>", fixed = TRUE)
})

test_that("as.tags draws a sandboxed iframe", {
  html <- as.character(htmltools::as.tags(html_page("x")))
  expect_match(html, "<iframe class=\"blockr-html-page\"", fixed = TRUE)
  expect_match(html, "sandbox=\"allow-scripts\"", fixed = TRUE)
  expect_match(html, "data-auto", fixed = TRUE)
  expect_true(is_html_renderable(html_page("x")))
})

# The JSON the page reads as `blockr.data`, parsed back.
page_data <- function(p) {
  m <- regmatches(
    p$srcdoc, regexpr("window\\.blockr=\\{data:.*?\\};</script>", p$srcdoc, perl = TRUE)
  )
  json <- sub("^window\\.blockr=\\{data:", "", sub("\\};</script>$", "", m))
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

test_that("the data script survives quotes, backslashes and tags", {
  nasty <- 'say "hi" \\ </script><img src=x onerror=alert(1)> <!--<script> a\u2028b & c'
  for (tpl in c("x", "<html><head></head><body>x</body></html>")) {
    p <- html_page(tpl, data = data.frame(v = nasty))
    # Nothing in the data script can end it or open a comment.
    script <- regmatches(
      p$srcdoc, regexpr("window\\.blockr=.*?</script>", p$srcdoc, perl = TRUE)
    )
    inner <- sub("</script>$", "", script)
    expect_false(grepl("[<>&]", inner))
    expect_false(grepl("\u2028", inner, fixed = TRUE))
    expect_identical(page_data(p)[[1]]$v, nasty)
  }
})

test_that("a head with attributes is found, a header is not a head", {
  p <- html_page("<html><head lang=\"en\"><title>t</title></head></html>")
  expect_match(p$srcdoc, "<head lang=\"en\"><meta charset", fixed = TRUE)
  p <- html_page("<header>Top</header><p>x</p>")
  expect_match(p$srcdoc, "^<!doctype html><html><head><meta charset")
  expect_match(p$srcdoc, "<body><header>Top</header><p>x</p></body>", fixed = TRUE)
})

test_that("columns with a dot or a space are read under safe names", {
  d <- data.frame(Sepal.Length = 5.1, `my col` = "q", ok = 1, check.names = FALSE)
  expect_no_warning(
    p <- html_page("{{#rows}}{{Sepal_Length}}|{{my_col}}|{{ok}}{{/rows}}", data = d)
  )
  expect_match(p$srcdoc, "<body>5.1|q|1</body>", fixed = TRUE)
  # A template that writes the real name is told what to write.
  expect_warning(
    html_page("{{#rows}}{{Sepal.Length}}{{{ my col }}}{{/rows}}", data = d),
    "`\\{\\{Sepal_Length\\}\\}` for `Sepal.Length`, `\\{\\{my_col\\}\\}` for `my col`"
  )
  # blockr.data keeps the real names and gets no copies.
  expect_named(page_data(p)[[1]], c("Sepal.Length", "my col", "ok"))
  # An existing column is not replaced by a copy.
  d2 <- data.frame(a.b = "dot", a_b = "under", check.names = FALSE)
  expect_no_warning(p <- html_page("{{#rows}}{{a_b}}{{/rows}}", data = d2))
  expect_match(p$srcdoc, "<body>under</body>", fixed = TRUE)
})

test_that("rows are only built when the template uses them", {
  d <- data.frame(a.b = 1:3)
  local_mocked_bindings(
    html_page_template_data = function(...) stop("rows were built")
  )
  p <- html_page("<p>{{nrow}} x {{ncol}}</p>", data = d)
  expect_match(p$srcdoc, "<p>3 x 1</p>", fixed = TRUE)
  expect_length(page_data(p), 3L)
  expect_error(html_page("{{#rows}}x{{/rows}}", data = d), "rows were built")
})

test_that("js_data = FALSE leaves blockr.data out", {
  p <- html_page("{{#rows}}{{v}}{{/rows}}", data = data.frame(v = "zzz"), js_data = FALSE)
  expect_match(p$srcdoc, "window.blockr={data:null}", fixed = TRUE)
  expect_match(p$srcdoc, "<body>zzz</body>", fixed = TRUE)
  expect_error(html_page("x", js_data = NA), "js_data")
})

test_that("max_rows defaults to 1000 and accepts NULL and Inf", {
  expect_identical(formals(html_page)$max_rows, 1000L)
  d <- data.frame(i = 1:1001)
  expect_warning(p <- html_page("{{nrow}}", data = d), "first 1000")
  expect_no_warning(html_page("{{nrow}}", data = d, max_rows = NULL))
  expect_no_warning(html_page("{{nrow}}", data = d, max_rows = Inf))
  expect_error(html_page("x", max_rows = NA), "max_rows")
  expect_error(html_page("x", max_rows = 1:2), "max_rows")
})

test_that("values in the template and in blockr.data", {
  d <- data.frame(
    n = c(1.5, NA), i = c(Inf, NaN), f = factor(c("a", "b")),
    d = as.Date("2026-01-02") + 0:1,
    t = as.POSIXct("2026-01-02 03:04:05", tz = "UTC") + 0:1
  )
  d$l <- list(1:2, "x")
  p <- html_page("{{#rows}}[{{n}} {{f}} {{d}}]{{/rows}}", data = d)
  expect_match(p$srcdoc, "[1.5 a 2026-01-02][NA b 2026-01-03]", fixed = TRUE)
  js <- page_data(p)
  expect_identical(js[[1]]$n, 1.5)
  expect_null(js[[2]]$n)
  expect_null(js[[1]]$i)
  expect_identical(js[[2]]$f, "b")
  expect_identical(js[[1]]$d, "2026-01-02")
  # Date-times carry their zone.
  expect_identical(js[[1]]$t, "2026-01-02T03:04:05Z")
  expect_identical(js[[1]]$l, list(1L, 2L))
})

test_that("zero rows, zero columns and a column named rows", {
  p <- html_page("{{nrow}}/{{ncol}}{{#rows}}x{{/rows}}", data = mtcars[0, ])
  expect_match(p$srcdoc, "<body>0/11</body>", fixed = TRUE)
  expect_identical(page_data(p), list())
  p <- html_page("{{nrow}}/{{ncol}}", data = data.frame())
  expect_match(p$srcdoc, "<body>0/0</body>", fixed = TRUE)
  p <- html_page("{{#rows}}<{{{rows}}}>{{/rows}}", data = data.frame(rows = c("a", "b")))
  expect_match(p$srcdoc, "<body><a><b></body>", fixed = TRUE)
})

test_that("data that is a list or a vector goes to blockr.data, a function stops", {
  p <- html_page("{{nrow}}", data = list(a = 1, b = "x"))
  expect_match(p$srcdoc, "<body>0</body>", fixed = TRUE)
  expect_identical(page_data(p), list(a = 1L, b = "x"))
  expect_identical(page_data(html_page("x", data = 1:3)), list(1L, 2L, 3L))
  expect_length(page_data(html_page("x", data = matrix(1:4, 2))), 2L)
  expect_error(html_page("x", data = mean), "data frame, a list or a vector")
  expect_error(html_page("x", data = quote(a)), "data frame, a list or a vector")
})

test_that("names in dots that the template sets itself stop", {
  expect_error(html_page("x", rows = "mine"), "`rows`")
  expect_error(html_page("x", nrow = 1, ncol = 2), "`nrow`, `ncol`")
  # A URL has no such names.
  expect_identical(
    html_page(url = "https://x.org/{rows}", rows = "r")$url, "https://x.org/r"
  )
  expect_error(html_page("x", NULL, NULL, "unnamed"), "must be named")
})

test_that("sandbox must be a single string", {
  expect_error(html_page("x", sandbox = c("a", "b")), "sandbox")
  expect_error(html_page("x", sandbox = NA_character_), "sandbox")
  expect_identical(html_page("x", sandbox = "")$sandbox, "")
})

test_that("only web addresses are accepted as a URL", {
  expect_error(html_page(url = "javascript:alert(document.domain)"), "http")
  expect_error(html_page(url = "data:text/html,<b>x</b>"), "http")
  expect_error(html_page(url = "/admin"), "http")
  expect_error(html_page(url = "NA"), "http")
  expect_error(html_page(url = NA_character_), "single string")
  expect_error(html_page(url = c("https://a.org", "https://b.org")), "single string")
  # The check runs on the filled URL.
  expect_error(html_page(url = "{u}", u = "javascript:alert(1)"), "http")
  expect_error(
    html_page(url = "{link}", data = data.frame(link = "javascript:alert(1)")),
    "http"
  )
  expect_identical(html_page(url = "HTTPS://x.org/a")$url, "HTTPS://x.org/a")
})

test_that("URL placeholders stop on missing values and write numbers in full", {
  expect_error(html_page(url = "https://x.org/{id}", data = data.frame(id = NA)), "missing")
  expect_error(html_page(url = "https://x.org/{id}", id = NULL), "missing")
  expect_error(html_page(url = "https://x.org/{id}", id = character()), "missing")
  expect_identical(
    html_page(url = "https://x.org/{id}", data = data.frame(id = 1e6))$url,
    "https://x.org/1000000"
  )
  expect_identical(html_page(url = "https://x.org/{id}", id = 100000)$url, "https://x.org/100000")
  expect_identical(html_page(url = "https://x.org/{id}", id = 2.5)$url, "https://x.org/2.5")
  expect_identical(
    html_page(url = "https://x.org/{f}", data = data.frame(f = factor("b", c("a", "b"))))$url,
    "https://x.org/b"
  )
  # A missing value that the URL does not use is fine.
  expect_no_error(html_page(url = "https://x.org/", data = data.frame(id = NA)))
})

test_that("height is a CSS length", {
  expect_identical(html_page("x", height = 400)$height, "400px")
  expect_identical(html_page("x", height = "50vh")$height, "50vh")
  expect_error(html_page("x", height = "10px;position:fixed;inset:0"))
  expect_error(html_page("x", height = c(1, 2)), "single value")
  html <- as.character(htmltools::as.tags(html_page("x", height = 400)))
  expect_match(html, "height:400px;", fixed = TRUE)
  expect_false(grepl("data-auto", html, fixed = TRUE))
})

test_that("as.tags in URL mode draws the link and the frame", {
  u <- html_page(url = "https://x.org/a?b=1")
  html <- as.character(htmltools::as.tags(u))
  expect_match(html, "<a href=\"https://x.org/a?b=1\" target=\"_blank\"", fixed = TRUE)
  expect_match(html, "rel=\"noopener noreferrer\"", fixed = TRUE)
  expect_match(html, "src=\"https://x.org/a?b=1\"", fixed = TRUE)
  expect_match(html, "allow-scripts allow-same-origin", fixed = TRUE)
  expect_match(html, "referrerpolicy=\"no-referrer\"", fixed = TRUE)
  expect_match(html, "height:600px;", fixed = TRUE)
})

test_that("the height listener is a script file, not inline", {
  tags <- htmltools::as.tags(html_page("x"))
  expect_false(grepl("<script", as.character(tags), fixed = TRUE))
  deps <- htmltools::findDependencies(tags)
  expect_identical(deps[[1]]$name, "blockr-html-page")
  expect_true(file.exists(file.path(deps[[1]]$src$file, deps[[1]]$script)))
})

test_that("print opens nothing outside interactive use", {
  called <- FALSE
  op <- options(
    browser = function(url) called <<- TRUE,
    viewer = function(url, ...) called <<- TRUE
  )
  on.exit(options(op))
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_output(print(html_page("<b>x</b>")), "^<html_page> html, .* characters, height auto")
  expect_output(print(html_page(url = "https://x.org")), "<html_page> https://x.org, height 600px")
  expect_false(called)
})

test_that("a knitr document gets the frame", {
  skip_if_not_installed("knitr")
  out <- knitr::knit_print(html_page("<b>hello-page</b>"))
  expect_s3_class(out, "knit_asis")
  expect_match(out, "<iframe class=\"blockr-html-page\"", fixed = TRUE)
  expect_match(out, "hello-page", fixed = TRUE)
})

test_that("every starter parses and runs with and without an input", {
  for (st in sb_starters()) {
    expect_true(cb_parse(st$script)$ok, label = st$label)
    env <- new.env()
    env$data <- iris
    expect_no_error(eval(parse(text = st$script), env))
  }
  env <- new.env()
  env$data <- NULL
  html <- Filter(function(st) st$label == "HTML page", sb_starters())[[1]]
  expect_s3_class(eval(parse(text = html$script), env), "html_page")
})

test_that("on a script block the document travels apart from the frame", {
  sent <- list()
  session <- list(
    ns = shiny::NS("blk"),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  page <- html_page("<p>{{v}}</p>", data = data.frame(q = "say \"hi\""), v = 1)

  render <- block_result_output(page, NULL, session)
  expect_length(sent, 1L)
  msg <- sent[[1L]]$message
  expect_identical(sent[[1L]]$type, "blockr-html-page-doc")
  expect_identical(msg$id, "blk-html_page_frame")
  expect_identical(msg$html, page$srcdoc)

  # the frame names the document it waits for and does not carry it
  html <- as.character(html_page_tags(page, id = msg$id, doc = msg$doc))
  expect_match(html, sprintf("data-doc=\"%s\"", msg$doc), fixed = TRUE)
  expect_match(html, "id=\"blk-html_page_frame\"", fixed = TRUE)
  expect_no_match(html, "srcdoc", fixed = TRUE)
  expect_lt(nchar(html), 400L)

  # every render has its own label
  block_result_output(page, NULL, session)
  expect_false(identical(sent[[2L]]$message$doc, msg$doc))

  # anywhere else the frame carries its document
  expect_match(as.character(htmltools::as.tags(page)), "srcdoc=", fixed = TRUE)
})

test_that("a URL page on a script block sends no document", {
  session <- list(
    ns = shiny::NS("blk"),
    sendCustomMessage = function(...) stop("no message expected")
  )
  render <- block_result_output(html_page(url = "https://example.com"), NULL, session)
  expect_true(is.function(render))
})
