# The sentence: `#| title:` lines whose `{@arg}` words are the controls.

sentence_script <- c(
  'arm <- factor("b", levels = c("a", "b", "c"))',
  "top_n <- 3  #| number(min = 1, max = 5, label = \"Number of rows\")",
  'serious <- TRUE  #| label = "serious only"',
  'when <- as.Date("2024-01-31")',
  "extra <- 2",
  "#| title: Top {@top_n} rows in {@arm}",
  "#| subtitle: {n} rows in, [{@serious}, ]cut {@when}",
  "utils::head(data, top_n)"
)

specs_of <- function(lines, data = iris) {
  cb_specs(cb_parse(paste(lines, collapse = "\n")), list(data = data))
}

html_of <- function(x) {
  if (is.null(x)) "" else as.character(x)
}


# ---- reading the lines ------------------------------------------------------

test_that("the #| title, subtitle and caption lines are read", {
  tpl <- sb_sentence_templates(c(
    "x <- 1",
    "#| title: Top {@n}",
    "  #|subtitle:   by {@arm}  ",
    "x"
  ))
  expect_equal(tpl$title, "Top {@n}")
  expect_equal(tpl$subtitle, "by {@arm}")
  expect_null(tpl$caption)
  expect_true(sb_has_sentence(tpl))
})

test_that("a script without the lines has no sentence", {
  tpl <- sb_sentence_templates(c("n <- 3", "#| number(min = 1)", "head(data, n)"))
  expect_false(sb_has_sentence(tpl))
})

test_that("ui_subtitle adds to the subtitle, ui_title replaces the title", {
  tpl <- sb_sentence_templates(c(
    "#| title: Report title",
    "#| ui_title: Face title",
    "#| subtitle: Base",
    "#| ui_subtitle: [{@extra}]"
  ))
  expect_equal(tpl$title, "Face title")
  expect_equal(tpl$subtitle, "Base [{@extra}]")
})


# ---- the template walk ------------------------------------------------------

test_that("a clause leaves with an empty token and offers its control", {
  parts <- title_template_parts(
    "Rows[, {@flag}]", data.frame(), list(flag = "")
  )
  expect_equal(vapply(parts, `[[`, "", "text"), "Rows")
  expect_equal(attr(parts, "offers"), "flag")

  parts <- title_template_parts(
    "Rows[, {@flag}]", data.frame(), list(flag = "serious only")
  )
  expect_equal(paste0(vapply(parts, `[[`, "", "text"), collapse = ""),
               "Rows, serious only")
  expect_length(attr(parts, "offers"), 0L)
})

test_that("data tokens read the input and are not slots", {
  d <- data.frame(x = 1:4)
  attr(d$x, "label") <- "The x"
  parts <- title_template_parts("{n} rows of {label(x)}", d)
  expect_equal(
    paste0(vapply(parts, `[[`, "", "text"), collapse = ""),
    "4 rows of The x"
  )
  parts <- title_template_parts("{n} rows", d)
  expect_null(parts[[1L]]$arg)
})


# ---- which controls become words --------------------------------------------

test_that("a named select, number and checkbox are words; a date is not", {
  specs <- specs_of(sentence_script)
  tpl <- sb_sentence_templates(sentence_script)
  expect_setequal(sb_slot_arg_names(specs, tpl), c("arm", "top_n", "serious"))
})

test_that("a checkbox prints its label, or nothing when off", {
  specs <- specs_of(sentence_script)
  args <- sb_title_args(specs, list(serious = TRUE))
  expect_equal(args$serious, "serious only")
  expect_equal(sb_title_args(specs, list(serious = FALSE))$serious, "")
  expect_equal(args$when, "2024-01-31")
})

test_that("a multi-select prints its values joined", {
  specs <- specs_of(c(
    'sev <- factor(c("MILD", "SEVERE"), levels = c("MILD", "MODERATE", "SEVERE"))',
    "data"
  ))
  expect_equal(sb_title_args(specs)$sev, "MILD, SEVERE")
  expect_equal(sb_title_args(specs, list(sev = character()))$sev, "")
})


# ---- a pick on a word -------------------------------------------------------

test_that("a pick is coerced the way the grid would coerce it", {
  specs <- specs_of(sentence_script)
  vals <- sb_apply_slot(list(arg = "top_n", value = "4"), specs, list())
  expect_identical(vals$top_n, 4)
  vals <- sb_apply_slot(list(arg = "top_n", value = "99"), specs, list())
  expect_identical(vals$top_n, 5)
  vals <- sb_apply_slot(list(arg = "serious", value = FALSE), specs, list())
  expect_false(vals$serious)
  vals <- sb_apply_slot(list(arg = "arm", value = "c"), specs, list())
  expect_identical(vals$arm, "c")
  expect_identical(
    sb_apply_slot(list(arg = "nope", value = 1), specs, list(a = 1)),
    list(a = 1)
  )
})

test_that("a multi pick of nothing is an empty value, not a missing one", {
  specs <- specs_of(c(
    'sev <- factor(c("MILD", "SEVERE"), levels = c("MILD", "MODERATE", "SEVERE"))',
    "data"
  ))
  vals <- sb_apply_slot(list(arg = "sev", value = list()), specs, list())
  expect_identical(vals$sev, character())
})


# ---- the band ---------------------------------------------------------------

test_that("the band marks each word with its control", {
  specs <- specs_of(sentence_script)
  tpl <- sb_sentence_templates(sentence_script)
  html <- html_of(sb_sentence_ui(tpl, specs, list(), iris, "blk-slot"))
  expect_match(html, 'class="blockr-script-sentence"')
  expect_match(html, 'data-sb-input="blk-slot"')
  expect_match(html, 'data-slot-arg="arm"[^>]*>b</span>')
  expect_match(html, 'data-slot-arg="top_n"[^>]*>3</span>')
  expect_match(html, "150 rows in, ")
  expect_match(html, "cut 2024-01-31")
  expect_no_match(html, 'data-slot-arg="when"')
})

test_that("a checkbox switched off leaves an offer chip", {
  specs <- specs_of(sentence_script)
  tpl <- sb_sentence_templates(sentence_script)
  html <- html_of(sb_sentence_ui(tpl, specs, list(serious = FALSE), iris, "s"))
  expect_match(html, 'class="blockr-slot-offer"[^>]*data-slot-arg="serious"')
  expect_match(html, "\\+ serious only")
  expect_no_match(html, "serious only, ")
})

# ---- label_off ---------------------------------------------------------------

flag_script <- function(value = "TRUE", tier = "ui_subtitle") {
  c(
    paste0(
      "all_pts <- ", value, "  #| label = \"including all patients column\", ",
      "label_off = \"Add all patients column\""
    ),
    paste0("#| ", tier, ": [{@all_pts}. ]"),
    "data"
  )
}

band_of <- function(lines, values = list()) {
  html_of(sb_sentence_ui(
    sb_sentence_templates(lines), specs_of(lines), values, iris, "s"
  ))
}

test_that("label_off is read off the annotation and falls back to label", {
  s <- specs_of(flag_script())[[1L]]
  expect_identical(s$label_off, "Add all patients column")
  expect_identical(c(cb_label_off(s)), "Add all patients column")
  expect_true(attr(cb_label_off(s), "own"))
  plain <- specs_of('serious <- TRUE  #| label = "serious only"\ndata')[[1L]]
  expect_identical(c(cb_label_off(plain)), "serious only")
  expect_false(attr(cb_label_off(plain), "own"))
})

test_that("on, a checkbox word says its label in text and ARIA label", {
  html <- band_of(flag_script("TRUE"))
  expect_match(
    html,
    paste0(
      'class="blockr-slot"[^>]*data-slot-arg="all_pts"[^>]*',
      'title="Change including all patients column" ',
      'aria-label="including all patients column">',
      "including all patients column</span>\\."
    )
  )
  expect_no_match(html, "blockr-slot-offer")
  expect_no_match(html, ">\\+ Add all patients column<")
})

test_that("off, the chip says label_off in text, tooltip and ARIA label", {
  html <- band_of(flag_script("FALSE"))
  expect_match(
    html,
    paste0(
      'class="blockr-slot-offer"[^>]*data-slot-arg="all_pts"[^>]*',
      'title="Add all patients column" ',
      'aria-label="Add all patients column">',
      "\\+ Add all patients column</span>"
    )
  )
  expect_no_match(html, 'class="blockr-slot"')
  expect_no_match(html, ">including all patients column<")
})

test_that("label_off works in the title, subtitle and ui_subtitle", {
  for (tier in c("title", "subtitle", "ui_subtitle")) {
    lines <- flag_script("TRUE", tier)
    expect_match(band_of(lines), ">including all patients column</span>", info = tier)
    expect_match(
      band_of(lines, list(all_pts = FALSE)), ">\\+ Add all patients column</span>",
      info = tier
    )
  }
})

test_that("any kind takes label_off for its chip", {
  lines <- c(
    'sites <- factor(character(0), levels = c("Basel", "Bern"))  #| label = "Sites", label_off = "Limit to sites"',
    "#| subtitle: Subjects[ at {@sites}]",
    "data"
  )
  off <- band_of(lines)
  expect_match(off, 'title="Limit to sites" aria-label="Limit to sites">\\+ Limit to sites</span>')
  on <- band_of(lines, list(sites = c("Basel", "Bern")))
  expect_match(on, 'title="Change Sites" aria-label="Sites: Basel, Bern">Basel, Bern</span>')
})

test_that("without label_off a chip reads as before, with an ARIA label", {
  html <- band_of(sentence_script, list(serious = FALSE))
  expect_match(
    html,
    'title="Set serious only" aria-label="serious only">\\+ serious only</span>'
  )
})

test_that("a value word carries its label in the ARIA label", {
  html <- band_of(sentence_script)
  expect_match(
    html,
    'title="Change Number of rows" aria-label="Number of rows: 3">3</span>'
  )
})

test_that("the slot spec carries label_off", {
  specs <- specs_of(flag_script())
  slots <- sb_slot_specs(specs, list(), "all_pts")
  expect_identical(slots$all_pts$label, "including all patients column")
  expect_identical(slots$all_pts$label_off, "Add all patients column")
})

test_that("the text of the sentence reads label while on and drops while off", {
  lines <- flag_script("TRUE", "subtitle")
  tpl <- sb_sentence_templates(lines)
  specs <- specs_of(lines)
  on <- sb_resolved_titles(tpl, specs = specs, data = iris)
  expect_identical(on$subtitle, "including all patients column.")
  off <- sb_resolved_titles(tpl, specs = specs, values = list(all_pts = FALSE),
                            data = iris)
  expect_null(off$subtitle)
})

test_that("a click toggles between label and label_off in the block", {
  block <- new_script_block(flag_script("TRUE"))
  shiny::testServer(
    blockr.core:::block_expr_server(block),
    {
      session$flushReact()
      expect_no_match(html_of(output$dynamic_params$html), "cb_all_pts")
      band <- html_of(output$sentence$html)
      expect_match(band, ">including all patients column</span>")
      expect_no_match(band, ">\\+ Add all patients column<")

      session$setInputs(slot = list(arg = "all_pts", value = FALSE, nonce = 1))
      expect_false(session$returned$state$values()$all_pts)
      band <- html_of(output$sentence$html)
      expect_match(band, ">\\+ Add all patients column</span>")
      expect_no_match(band, ">including all patients column<")

      session$setInputs(slot = list(arg = "all_pts", value = TRUE, nonce = 2))
      expect_true(session$returned$state$values()$all_pts)
      band <- html_of(output$sentence$html)
      expect_match(band, ">including all patients column</span>")
      expect_no_match(band, "blockr-slot-offer")
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("the slot specs say what each word opens", {
  specs <- specs_of(sentence_script)
  tpl <- sb_sentence_templates(sentence_script)
  slots <- sb_slot_specs(specs, list(), sb_slot_arg_names(specs, tpl))
  expect_equal(slots$arm$kind, "select")
  expect_equal(slots$arm$selected, "b")
  expect_equal(slots$top_n[c("kind", "value", "min", "max")],
               list(kind = "number", value = 3, min = 1, max = 5))
  expect_equal(slots$serious$kind, "flag")
  expect_null(slots$when)
})

test_that("a listed slot narrows its menu", {
  lines <- c(
    'arm <- factor("b", levels = c("a", "b", "c"))',
    "#| title: In {@arm in c, a}",
    "data"
  )
  specs <- specs_of(lines)
  tpl <- sb_sentence_templates(lines)
  html <- html_of(sb_sentence_ui(tpl, specs, list(), iris, "s"))
  expect_match(html, ">In <span")
  slots <- sb_slot_specs(specs, list(), "arm", listed = sb_template_slot_choices(tpl))
  expect_equal(unlist(slots$arm$options), c("c", "a", "b"))
})

test_that("no #| lines, no band", {
  lines <- c("n <- 3", "utils::head(data, n)")
  expect_null(
    sb_sentence_ui(sb_sentence_templates(lines), specs_of(lines), list(), iris, "s")
  )
})


# ---- in the block -----------------------------------------------------------

test_that("the grid drops the controls the sentence took, and a word sets them", {
  block <- new_script_block(sentence_script)
  shiny::testServer(
    blockr.core:::block_expr_server(block),
    {
      session$flushReact()
      grid <- html_of(output$dynamic_params$html)
      expect_no_match(grid, "cb_arm")
      expect_no_match(grid, "cb_top_n")
      expect_match(grid, "cb_when")
      expect_match(grid, "cb_extra")

      band <- html_of(output$sentence$html)
      expect_match(band, 'data-slot-arg="top_n"[^>]*>3</span>')

      session$setInputs(slot = list(arg = "top_n", value = "2", nonce = 1))
      expect_identical(session$returned$state$values()$top_n, 2)
      expect_match(
        paste(deparse(session$returned$expr()), collapse = ""),
        "head\\(.*, 2\\)"
      )
      expect_match(html_of(output$sentence$html), 'data-slot-arg="top_n"[^>]*>2</span>')
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("a script with no sentence keeps every control in the grid", {
  block <- new_script_block(c("n <- 3", "utils::head(data, n)"))
  shiny::testServer(
    blockr.core:::block_expr_server(block),
    {
      session$flushReact()
      expect_match(html_of(output$dynamic_params$html), "cb_n")
      expect_null(output$sentence$html)
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("a word with nothing to pick is plain text and its control stays in the grid", {
  lines <- c(
    'arm <- factor("b", levels = c("a", "b", "c"))',
    "n <- 2",
    "#| title: In {@arm in x, y} with {@n}",
    "utils::head(data, n)"
  )
  specs <- specs_of(lines)
  tpl <- sb_sentence_templates(lines)
  expect_equal(sb_slot_arg_names(specs, tpl), "n")
  html <- html_of(sb_sentence_ui(tpl, specs, list(), iris, "s"))
  expect_no_match(html, 'data-slot-arg="arm"')
  expect_match(html, ">In b with <span")

  block <- new_script_block(lines)
  shiny::testServer(
    blockr.core:::block_expr_server(block),
    {
      session$flushReact()
      grid <- html_of(output$dynamic_params$html)
      expect_match(grid, "cb_arm")
      expect_no_match(grid, "cb_n\\b")
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("a line break next to a dropped clause leaves no stray line", {
  d <- data.frame(v = 1)
  txt <- function(p) paste0(vapply(p, function(x) x$text, ""), collapse = "")
  tpl <- "A[ by {@x}]\\n[B {@y}]"

  second <- title_template_parts(tpl, d, list(x = "g", y = NULL))
  expect_equal(txt(second), "A by g")
  expect_equal(attr(second, "offers"), "y")

  first <- title_template_parts("[A {@x}]\\n[B {@y}]", d,
                                list(x = NULL, y = "h"))
  expect_equal(txt(first), "B h")

  both <- title_template_parts(tpl, d, list(x = "g", y = "h"))
  expect_equal(txt(both), "A by g\nB h")
})
