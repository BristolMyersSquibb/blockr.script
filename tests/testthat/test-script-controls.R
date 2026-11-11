# Which lines become controls, what each control is, and how a value coming
# back from the browser or from a saved board is read.

specs_of <- function(script, data = iris, values = list()) {
  cb_specs(cb_parse(script), inputs = list(data = data), values = values)
}

spec_of <- function(script, ...) {
  specs_of(script, ...)[[1L]]
}

is_input_line <- function(line) {
  cb_parse(line)$stmts[[1L]]$input
}

test_that("a value that cannot be read as the declared type is no value", {
  date <- spec_of("d <- as.Date(\"2026-01-01\")")
  expect_null(cb_coerce("abc", date))
  expect_null(cb_coerce("", date))
  expect_identical(cb_coerce("2026-02-03", date), as.Date("2026-02-03"))

  flag <- spec_of("f <- TRUE")
  expect_null(cb_coerce("abc", flag))
  expect_identical(cb_coerce("FALSE", flag), FALSE)

  # so the script's own value applies
  p <- cb_parse("d <- as.Date(\"2026-01-01\")\nformat(d)")
  expect_identical(
    cb_expr(p, cb_specs(p), list(d = "abc")),
    quote(format(as.Date("2026-01-01")))
  )
  p <- cb_parse("f <- TRUE\nif (f) 1 else 2")
  expect_identical(cb_expr(p, cb_specs(p), list(f = "abc")), 1)
})

test_that("a number keeps the type the script wrote", {
  int <- spec_of("n <- 6L")
  expect_identical(cb_coerce(7, int), 7L)
  expect_identical(cb_coerce("7", int), 7L)
  # a fraction typed into an integer control stays a fraction
  expect_identical(cb_coerce(7.5, int), 7.5)

  dbl <- spec_of("n <- 6")
  expect_identical(cb_coerce(7L, dbl), 7)
})

test_that("as.POSIXct() is code, not a control", {
  expect_false(is_input_line("t <- as.POSIXct(\"2026-01-01 10:00\")"))
  expect_length(cb_syntactic_marks("t <- as.POSIXct(\"2026-01-01\")\nt"), 0L)
})

test_that("c() is a control only when every element is a literal", {
  expect_true(is_input_line("keep <- c(\"a\", \"b\")"))
  expect_true(is_input_line("lim <- c(1, -2)"))
  expect_true(is_input_line("lim <- c(lo = 1, hi = 2)"))
  expect_false(is_input_line("rng <- c(min(data$x), max(data$x))"))
  expect_false(is_input_line("ids <- c(data$id)"))
  expect_false(is_input_line("both <- c(a, \"b\")"))

  # c() of one literal is that literal
  expect_identical(spec_of("n <- c(5)")$kind, "number")
  expect_identical(cb_syntactic_marks("n <- c(5)\nn")[[1L]]$kind, "number")
})

test_that("a date control holds exactly one date", {
  s <- spec_of("d <- as.Date(c(\"2026-01-01\", \"2026-02-01\"))")
  expect_true(is.na(s$kind))
  expect_match(s$error, "one date")
})

test_that("a factor value that is not a level is reported", {
  s <- spec_of("x <- factor(\"zzz\", c(\"a\", \"b\"))")
  expect_true(is.na(s$kind))
  expect_match(s$error, "not one of the choices")
  # an empty pick is a multi-select with nothing chosen, not an error
  expect_null(spec_of("x <- factor(character(0), c(\"a\", \"b\"))")$error)
})

test_that("an empty text box is no value when the script wrote NA", {
  s <- spec_of("x <- NA_character_")
  expect_identical(s$kind, "text")
  expect_null(cb_coerce("", s))
  expect_identical(cb_coerce("a", s), "a")
  expect_identical(cb_coerce("", spec_of("x <- \"a\"")), "")
})

test_that("a control name with a space gives an input id without one", {
  expect_identical(cb_input_id("n"), "cb_n")
  expect_identical(cb_input_id("top_n.x"), "cb_top_n.x")
  id <- cb_input_id("Pool members")
  expect_false(grepl("[^A-Za-z0-9_.-]", id))
  expect_false(identical(id, cb_input_id("Pool_members")))
  expect_false(identical(id, cb_input_id("Pool-members")))

  s <- spec_of("`Pool members` <- factor(\"a\", c(\"a\", \"b\"))")
  html <- as.character(cb_input_ui(s, shiny::NS("blk")))
  expect_match(html, paste0("id=\"blk-", id, "\""), fixed = TRUE)
  expect_match(html, paste0("for=\"blk-", id, "\""), fixed = TRUE)
  expect_match(html, "Pool members", fixed = TRUE)
})

test_that("a helper line sees the names as they were when it was written", {
  script <- paste(
    ".x <- 1",
    ".y <- .x + 1",
    ".x <- 100",
    "k <- factor(\"a\", letters[seq_len(.y)])",
    "j <- factor(\"a\", letters[seq_len(.x / 25)])",
    "k",
    sep = "\n"
  )
  s <- specs_of(script)
  expect_identical(s[[1L]]$choices, c("a", "b"))
  expect_identical(s[[2L]]$choices, c("a", "b", "c", "d"))
})

test_that("a later declaration reads the value of an earlier control", {
  script <- paste(
    "col <- factor(\"Species\", names(data))",
    "val <- factor(character(0), sort(unique(as.character(data[[col]]))))",
    "data[data[[col]] %in% val, ]",
    sep = "\n"
  )
  expect_identical(
    specs_of(script)[[2L]]$choices,
    c("setosa", "versicolor", "virginica")
  )
  d <- data.frame(Species = c("x", "y"), Site = c("b", "a"))
  expect_identical(
    specs_of(script, d, values = list(col = "Site"))[[2L]]$choices,
    c("a", "b")
  )
  # a stored value that is no longer a choice is not used
  expect_identical(
    specs_of(script, d, values = list(col = "Gone"))[[2L]]$choices,
    c("x", "y")
  )
})

test_that("an annotation belongs to the statement it ends", {
  # on the last line of a declaration that spans lines
  s <- spec_of("n <- c(\n  5\n)  #| number(min = 1, max = 9)\nn")
  expect_identical(s$max, 9)

  # two declarations on a line: the comment is the second one's
  s <- specs_of("n <- 6; m <- 7  #| label = \"Rows\"\nn + m")
  expect_null(s[[1L]]$label)
  expect_identical(s[[2L]]$label, "Rows")

  # `#|` inside a string is text
  s <- spec_of("x <- \"a #| label = 'L'\"\nx")
  expect_null(s$label)
  expect_identical(s$default, "a #| label = 'L'")

  # an annotation on the line above belongs to nothing
  s <- spec_of("#| label = \"Up\"\nn <- 6\nn")
  expect_null(s$label)
})

test_that("a sentence line inside a string is not a sentence line", {
  p <- cb_parse("x <- \"\n#| title: hi\n\"\n#| subtitle: real\nnchar(x)")
  tpl <- sb_sentence_templates(p$comments)
  expect_null(tpl$title)
  expect_identical(tpl$subtitle, "real")

  # a trailing comment is an annotation, not a sentence line
  p <- cb_parse("n <- 3  #| title: no\nn")
  expect_null(sb_sentence_templates(p$comments)$title)

  # a script that does not parse keeps its sentence
  p <- cb_parse("#| title: Still here\nhead(data,")
  expect_identical(sb_sentence_templates(p$comments)$title, "Still here")
})

test_that("a .() the script writes itself is found", {
  expect_identical(cb_parse("with(data, .(Species))")$dots, 1L)
  expect_identical(cb_parse("n <- 1\n\nf(..(x))")$dots, 3L)
  expect_length(cb_parse("x <- list(. = 1)\nx$.")$dots, 0L)
  expect_length(cb_parse("utils::head(data)")$dots, 0L)

  p <- cb_parse("with(data, .(Species))")
  expect_match(
    sb_rest_label(list(), p),
    "line 1 calls .(), which blockr evaluates before the script runs",
    fixed = TRUE
  )
  expect_identical(attr(sb_rest_label(list(), p), "kind"), "warn")
})

test_that("a stored pick that left the choices is not used", {
  s <- spec_of("g <- factor(\"a\", c(\"a\", \"c\"))")
  expect_identical(cb_value(s, list(g = "b")), "a")
  expect_identical(cb_value(s, list(g = "c")), "c")
  expect_identical(cb_value(s, list()), "a")

  m <- spec_of("g <- factor(c(\"a\", \"c\"), c(\"a\", \"c\", \"d\"))")
  expect_identical(cb_value(m, list(g = c("b", "d"))), "d")
  expect_identical(cb_value(m, list(g = "b")), character())

  # the control shows what the expression uses
  html <- as.character(cb_input_ui(s, identity, "b"))
  expect_match(html, "data-selected=\"&quot;a&quot;\"", fixed = TRUE)
})
