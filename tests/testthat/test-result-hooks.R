# What another package builds on: a block of its own class on the script
# block, a result class that fills the head row, and the sentence as text.

tiny_starters <- function() {
  list(
    list(label = "Rows", hint = "first rows", script = "utils::head(data, 3)"),
    list(label = "Tiny", script = c("n <- 2", "utils::head(data, n)"))
  )
}

# A block built on the script block, as a package would write it.
new_tiny_block <- function(script = tiny("data"), values = list(), ...) {
  new_script_block(
    script, values,
    starters = tiny_starters(), class = "tiny_block", block_metadata = list(),
    ...
  )
}

# Script code that returns its argument as a result of a class of its own.
tiny <- function(code) {
  sprintf("structure(list(data = %s), class = \"tiny_result\")", code)
}

# One method is registered for `tiny_result`, and each test says what it
# does.
tiny_method <- new.env(parent = emptyenv())
registerS3method(
  "block_result_output", "tiny_result",
  function(result, block, session, ...) {
    tiny_method$fn(result, block, session, ...)
  },
  envir = asNamespace("blockr.script")
)
set_tiny_method <- function(fn) {
  tiny_method$fn <- fn
}

block_args <- function(block, data = shiny::reactive(datasets::iris)) {
  list(
    x = block,
    data = list(`...args` = reactives::reactive_exprs(data = data))
  )
}

# The head state of the block a testServer() expression runs in.
head_of <- function(session) {
  sb_head(session, session$ns(""))
}


# 1. subclasses ---------------------------------------------------------------

test_that("a block built on the script block has both classes", {
  blk <- new_tiny_block()
  expect_identical(
    class(blk)[1:3], c("tiny_block", "script_block", "block")
  )
  expect_identical(class(new_script_block())[1:2], c("script_block", "block"))
  # `starters` and `class` are not state
  expect_identical(names(blockr_ser(blk)$payload)[1:2], c("script", "values"))
  expect_false(any(c("starters", "class") %in% names(blockr_ser(blk)$payload)))
})

test_that("a subclass is saved and restored through its own constructor", {
  blk <- new_tiny_block("n <- 2\nutils::head(data, n)", list(n = 4))
  ser <- blockr_ser(blk)
  expect_identical(ser$object[1:2], c("tiny_block", "script_block"))
  expect_identical(ser$payload$script, c("n <- 2", "utils::head(data, n)"))
  # the constructor on record is the subclass's own
  expect_identical(
    body(unserialize(ser$constructor$constructor)), body(new_tiny_block)
  )

  back <- blockr.core::blockr_deser(ser)
  expect_identical(class(back), class(blk))
  expect_identical(blockr_ser(back)[c("object", "payload")],
                   ser[c("object", "payload")])
  expect_identical(
    body(unserialize(blockr_ser(back)$constructor$constructor)),
    body(new_tiny_block)
  )
  expect_identical(blockr_ser(back)$payload$script, ser$payload$script)
  expect_equal(blockr_ser(back)$payload$values, list(n = 4))
  # the restored block has the starters its constructor gives
  expect_identical(
    environment(back[["expr_server"]])$starters,
    environment(blk[["expr_server"]])$starters
  )

  # a plain script block still restores through new_script_block()
  plain <- blockr_ser(new_script_block("utils::head(data, 2)"))
  expect_identical(plain$constructor$constructor, "new_script_block")
  expect_identical(plain$constructor$package, "blockr.script")
})

test_that("a constructor that goes through a helper is the one on record", {
  helper <- function(code, vals, ...) {
    new_script_block(code, vals, class = "helped_block",
                     block_metadata = list(), ...)
  }
  new_helped_block <- function(script = "utils::head(data, 2)",
                               values = list(), ...) {
    helper(script, values, ...)
  }
  blk <- new_helped_block("n <- 2\nutils::head(data, n)", list(n = 3))
  ser <- blockr_ser(blk)
  # not the helper, whose arguments are not the block's state
  expect_identical(
    body(unserialize(ser$constructor$constructor)), body(new_helped_block)
  )
  back <- blockr.core::blockr_deser(ser)
  expect_identical(class(back), class(blk))
  expect_identical(blockr_ser(back)$payload, ser$payload)
})

test_that("a class given at top level is saved with a constructor of its own", {
  # as a script or an app file calls it: no function around the call
  blk <- eval(
    bquote(.(new_script_block)(
      "utils::head(data, 2)", class = "bare_block", block_metadata = list(),
      starters = .(tiny_starters())
    )),
    globalenv()
  )
  expect_identical(class(blk)[1:2], c("bare_block", "script_block"))
  ser <- blockr_ser(blk)
  ctor <- unserialize(ser$constructor$constructor)
  expect_identical(names(formals(ctor)), c("script", "values", "..."))
  # it holds the class and the starters, and nothing else of the call
  expect_identical(sort(ls(environment(ctor))), c("class", "starters"))

  # (the class is in no registry, which blockr.core warns about)
  back <- suppressWarnings(blockr.core::blockr_deser(ser))
  expect_identical(class(back), class(blk))
  expect_identical(
    environment(back[["expr_server"]])$starters,
    environment(blk[["expr_server"]])$starters
  )
  # and the restored block saves as the first one did
  again <- suppressWarnings(blockr.core::blockr_deser(blockr_ser(back)))
  expect_identical(class(again), class(blk))
  expect_identical(blockr_ser(again)$payload, ser$payload)
})

test_that("the script block's server and methods run for a subclass", {
  blk <- new_tiny_block(paste0("n <- 2\n", tiny("utils::head(data, n)")))
  set_tiny_method(function(result, block, session, ...) {
    shiny::renderUI(paste("tiny", nrow(result$data)))
  })
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_s3_class(session$returned$result(), "tiny_result")
      expect_equal(
        session$returned$expr(),
        bquote(structure(
          list(data = utils::head(.(data_slot()), 2)), class = "tiny_result"
        ))
      )
      expect_match(as.character(output$result$html), "tiny 2", fixed = TRUE)
    },
    args = block_args(blk)
  )
})


# 2. starters -----------------------------------------------------------------

# A session that records the custom messages a block sends.
recording_session <- function() {
  session <- shiny::MockShinySession$new()
  sent <- list()
  unlockBinding("sendCustomMessage", session)
  session$sendCustomMessage <- function(type, message) {
    sent[[length(sent) + 1L]] <<- list(type = type, message = message)
  }
  list(session = session, sent = function() sent)
}

test_that("starters replace the built-in list and are checked", {
  rec <- recording_session()
  key <- sb_starters_key(rec$session, tiny_starters())
  got <- rec$sent()[[1L]]
  expect_identical(got$type, "blockr-script-tools")
  expect_identical(got$message$key, key)
  labels <- vapply(got$message$starters, `[[`, character(1L), "label")
  expect_true("Rows" %in% labels)
  expect_false("External page" %in% labels)

  # NULL keeps the built-in ones
  key_builtin <- sb_starters_key(rec$session)
  expect_false(identical(key_builtin, key))
  labels <- vapply(rec$sent()[[2L]]$message$starters, `[[`, character(1L), "label")
  expect_true("External page" %in% labels)

  checked <- sb_check_starters(tiny_starters())
  expect_identical(checked[[2L]]$hint, "")
  expect_identical(checked[[2L]]$script, "n <- 2\nutils::head(data, n)")
  expect_null(sb_check_starters(NULL))
  expect_error(new_script_block(starters = list(list(hint = "x"))), "label")
  expect_error(new_script_block(starters = "Table"), "list of starters")
})

test_that("a list of starters is sent once per session", {
  rec <- recording_session()
  key <- sb_starters_key(rec$session, tiny_starters())
  expect_identical(sb_starters_key(rec$session, tiny_starters()), key)
  expect_length(rec$sent(), 1L)
})

test_that("a block hands its starters to the editor row", {
  blk <- new_tiny_block()
  shiny::testServer(
    blk[["expr_server"]],
    {
      session$setInputs(tray_opened = TRUE)
      session$flushReact()
      html <- as.character(output$inputs_ui$html)
      expect_match(html, "data-starters-key=\"s1\"", fixed = TRUE)
      # The scripts themselves are not in the row.
      expect_false(grepl("first rows", html, fixed = TRUE))
      kept <- session$userData$blockr_script_starters
      expect_length(kept, 1L)
      labels <- vapply(kept[[1L]], `[[`, character(1L), "label")
      expect_true("Rows" %in% labels)
      expect_false("ggplot2 scatter" %in% labels)
    },
    args = list(`...args` = list(data = shiny::reactive(datasets::iris)))
  )
})

test_that("a chip's list is keyed by what the chip inserts, not in the row", {
  info <- sb_input_info(sb_inputs(list(events = function() iris, function() 2)))
  cols <- sb_tools_cols(info)
  expect_named(cols, c("data", ".arg1"))
  expect_identical(
    vapply(cols$data, `[[`, character(1L), "name"), names(iris)
  )
  expect_length(cols$.arg1, 0L)
  html <- as.character(sb_inputs_ui(info, "ed", starters_key = "s1"))
  expect_match(html, "data-insert=\"data\"", fixed = TRUE)
  expect_false(grepl("Sepal.Length", html, fixed = TRUE))
  # No input: an empty object, not an empty array.
  expect_identical(
    as.character(jsonlite::toJSON(sb_tools_cols(list()))), "{}"
  )
})


# 3. the tools slot -----------------------------------------------------------

test_that("the head row has a slot for tools, left of the gear", {
  ui <- as.character(new_script_block()[["expr_ui"]]("blk-expr"))
  slot <- regexpr("blk-expr-head_tools", ui, fixed = TRUE)
  gear <- regexpr("blk-expr-fn-gear-btn", ui, fixed = TRUE)
  sentence <- regexpr("blk-expr-sentence", ui, fixed = TRUE)
  expect_true(sentence > 0 && slot > sentence && gear > slot)
  expect_match(ui, "data-sb-block=\"blk-\"", fixed = TRUE)
})

test_that("a method fills the slot and another result class empties it", {
  calls <- 0L
  set_tiny_method(function(result, block, session, ...) {
    calls <<- calls + 1L
    result_head(
      session,
      tools = shiny::tags$button(id = session$ns("dl"), "get"),
      titles = list(title = "Tiny table")
    )
    shiny::renderUI("tiny")
  })
  blk <- new_tiny_block(paste0("n <- 2\n", tiny("utils::head(data, n)")))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      head <- head_of(session)
      expect_match(
        as.character(shiny::isolate(head$tools())), "proxy1-dl", fixed = TRUE
      )
      expect_match(
        as.character(output[["expr-head_tools"]]$html), "get", fixed = TRUE
      )
      expect_identical(shiny::isolate(head$titles()), list(title = "Tiny table"))

      # evaluated again with the same class: nothing is written again
      seen <- 0L
      obs <- shiny::observe({
        head$tools()
        seen <<- seen + 1L
      })
      session$flushReact()
      before <- calls
      session$returned$state$values(list(n = 3))
      session$flushReact()
      expect_gt(calls, before)
      expect_identical(seen, 1L)

      # the script returns a plain data frame: the slot and the title go
      session$returned$state$script("utils::head(data, 2)")
      session$flushReact()
      expect_null(shiny::isolate(head$tools()))
      expect_identical(shiny::isolate(head$titles()), list())
      expect_identical(seen, 2L)
      expect_null(result_titles(session)$title)
      obs$destroy()
    },
    args = block_args(blk)
  )
})

test_that("result_head() checks what it is given", {
  session <- shiny::MockShinySession$new()
  expect_error(result_head(session, titles = list(heading = "x")), "titles")
  expect_error(result_head(session, titles = list(title = 1)), "one string")
  expect_error(result_head(session, caption = NA), "caption")
  expect_null(result_head(session, titles = list(title = "", caption = "c")))
  head <- sb_head(session, session$ns(""))
  expect_identical(shiny::isolate(head$titles()), list(caption = "c"))
})


# 4. the sentence as text ------------------------------------------------------

test_that("titles resolve from the script, the method and neither", {
  specs <- cb_specs(cb_parse("n <- 3\nx <- 1"), list(data = iris))
  tpl <- sb_sentence_templates(c(
    "#| title: Top {@n} of {n} rows",
    "#| subtitle:"
  ))
  fallback <- list(title = "Method title", subtitle = "Sub", caption = "Cap")
  got <- sb_resolved_titles(tpl, fallback, specs, list(n = 5), iris)
  # a script line wins, an empty script line switches the line off, and a
  # line the script does not declare is the method's
  expect_identical(got$title, "Top 5 of 150 rows")
  expect_null(got$subtitle)
  expect_identical(got$caption, "Cap")
  expect_identical(got$filters, character())

  none <- sb_resolved_titles(sb_sentence_templates(character()))
  expect_identical(none, SB_NO_TITLES)

  # {@x in a, b} reads as the value, and a dropped clause leaves no text
  tpl <- sb_sentence_templates("#| title: By {@n in 3, 5}[, {@missing}]")
  expect_identical(
    sb_resolved_titles(tpl, list(), specs, list(), iris)$title, "By 3"
  )

  trail <- structure(iris, blockr_filters = c(a = "Sex: F", b = NA, c = ""))
  expect_identical(
    sb_resolved_titles(tpl, list(), specs, list(), trail)$filters, "Sex: F"
  )
})

test_that("the head row draws fallback titles as plain text", {
  tpl <- sb_sentence_templates("#| subtitle: {n} rows")
  ui <- as.character(sb_sentence_ui(
    tpl, list(), list(), iris, "slot",
    fallback = list(title = "Fallback {n}", subtitle = "not shown")
  ))
  expect_match(ui, "blockr-script-sentence__title\">Fallback {n}<", fixed = TRUE)
  expect_match(ui, "150 rows", fixed = TRUE)
  expect_false(grepl("not shown", ui, fixed = TRUE))

  # no script line and no fallback: no band, as before
  expect_null(sb_sentence_ui(sb_sentence_templates(character()), list(),
                             list(), iris, "slot"))
  # an empty script line switches a fallback off
  expect_null(sb_sentence_ui(
    sb_sentence_templates("#| title:"), list(), list(), iris, "slot",
    fallback = list(title = "off")
  ))
})

test_that("result_titles() follows the sentence", {
  set_tiny_method(function(result, block, session, ...) {
    result_head(session, titles = list(title = "unused", caption = "From method"))
    shiny::renderUI(paste("title:", result_titles(session)$title))
  })
  blk <- new_tiny_block(paste(
    "word <- \"first\"",
    "#| title: The {@word} {n}",
    tiny("data"),
    sep = "\n"
  ))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      # outside a reactive context it gives the current text
      expect_identical(result_titles(session)$title, "The first 150")
      expect_identical(result_titles(session)$caption, "From method")
      expect_match(
        as.character(output$result$html), "title: The first 150", fixed = TRUE
      )
      expect_match(
        as.character(output[["expr-sentence"]]$html), "From method",
        fixed = TRUE
      )

      # the title's word changes; the output that reads the title follows
      session$setInputs(`expr-slot` = list(arg = "word", value = "second"))
      session$flushReact()
      expect_identical(result_titles(session)$title, "The second 150")
      expect_match(
        as.character(output$result$html), "title: The second 150",
        fixed = TRUE
      )
    },
    args = block_args(blk)
  )
})

test_that("a result that draws the caption takes it out of the head row", {
  set_tiny_method(function(result, block, session, ...) {
    result_head(session, caption = FALSE)
    shiny::renderUI(result_titles(session)$caption)
  })
  blk <- new_tiny_block(paste(
    "src <- \"registry\"",
    "#| title: Rows",
    "#| caption: Source: {@src}",
    tiny("data"),
    sep = "\n"
  ))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      sentence <- as.character(output[["expr-sentence"]]$html)
      expect_match(sentence, "Rows", fixed = TRUE)
      expect_false(grepl("Source", sentence, fixed = TRUE))
      expect_match(
        as.character(output$result$html), "Source: registry", fixed = TRUE
      )
      # the control the caption names has no word now: it is in the grid
      expect_match(
        as.character(output[["expr-dynamic_params"]]$html), "cb_src",
        fixed = TRUE
      )
    },
    args = block_args(blk)
  )

  face <- sb_face_templates(
    sb_sentence_templates("#| caption: c"), list(caption = "f", title = "t"),
    caption = FALSE
  )
  expect_null(face$templates$caption)
  expect_identical(face$fallback, list(title = "t"))
})


test_that("result_titles() says which lines the script declares", {
  tpl <- sb_sentence_templates(c(
    "#| title: Top {@n in 3, 5} rows",
    "#| caption:"
  ))
  specs <- cb_specs(cb_parse("n <- 3\nx <- 1"), list(data = iris))
  fallback <- list(subtitle = "Sub", caption = "Cap")
  got <- sb_resolved_titles(tpl, fallback, specs, list(), iris)
  # declared and shown, declared and switched off, the method's own
  expect_identical(got$declared, c("title", "caption"))
  expect_identical(got$title, "Top 3 rows")
  expect_null(got$caption)
  expect_identical(got$subtitle, "Sub")
  # the lines as written
  expect_identical(
    got$templates, list(title = "Top {@n in 3, 5} rows", caption = "")
  )

  tpl <- sb_sentence_templates("#| caption: {filters}")
  got <- sb_resolved_titles(tpl, list(), list(), list(), iris)
  expect_identical(got$declared, "caption")
  expect_identical(got$templates$caption, "{filters}")
  # declared, and empty because the input has no trail
  expect_null(got$caption)

  none <- sb_resolved_titles(sb_sentence_templates(character()), fallback)
  expect_identical(none$declared, character())
  expect_identical(none$templates, list())
})

test_that("the lines for the app only can be left out", {
  lines <- c(
    "#| title: Plain {@word}",
    "#| ui_title: Screen {@word}",
    "#| subtitle: {n} rows",
    "#| ui_subtitle: click a word",
    "#| ui_caption: Only on screen"
  )
  plain <- sb_sentence_templates(lines, ui = FALSE)
  expect_identical(plain$title, "Plain {@word}")
  expect_identical(plain$subtitle, "{n} rows")
  expect_null(plain$caption)
  expect_identical(sb_sentence_templates(lines)$title, "Screen {@word}")
  expect_identical(
    sb_sentence_templates(character(), ui = FALSE)[SB_TIERS],
    list(title = NULL, subtitle = NULL, caption = NULL)
  )

  set_tiny_method(function(result, block, session, ...) {
    result_head(session, titles = list(caption = "From method"))
    shiny::renderUI(result_titles(session, ui = FALSE)$title)
  })
  blk <- new_tiny_block(paste(
    c("word <- \"first\"", lines, tiny("data")), collapse = "\n"
  ))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      screen <- result_titles(session)
      expect_identical(screen$title, "Screen first")
      expect_identical(screen$subtitle, "150 rows click a word")
      expect_identical(screen$caption, "Only on screen")
      expect_identical(screen$declared, c("title", "subtitle", "caption"))

      plain <- result_titles(session, ui = FALSE)
      expect_identical(plain$title, "Plain first")
      expect_identical(plain$subtitle, "150 rows")
      # no plain caption line: the method's title stands in
      expect_identical(plain$caption, "From method")
      expect_identical(plain$declared, c("title", "subtitle"))
      expect_identical(plain$templates$title, "Plain {@word}")

      # both forms follow a control, and so does an output that reads one
      session$setInputs(`expr-slot` = list(arg = "word", value = "second"))
      session$flushReact()
      expect_identical(result_titles(session)$title, "Screen second")
      expect_identical(result_titles(session, ui = FALSE)$title, "Plain second")
      expect_match(
        as.character(output$result$html), "Plain second", fixed = TRUE
      )
      expect_error(result_titles(session, ui = NA), "ui")
    },
    args = block_args(blk)
  )
})


# 5. sentence_data() ----------------------------------------------------------

test_that("sentence_data() gives the data frame the sentence reads", {
  expect_identical(sentence_data(iris), iris)
  expect_null(sentence_data(NULL))
  expect_null(sentence_data("text"))
  expect_null(sentence_data(list(a = 1)))
  expect_null(sentence_data(stats::lm(dist ~ speed, datasets::cars)))
})

test_that("a dm reads as its columns side by side, labels kept", {
  skip_if_not_installed("dm")
  adsl <- data.frame(ID = 1:3, ARM = factor(c("A", "B", "A")))
  attr(adsl$ARM, "label") <- "Treatment arm"
  adae <- data.frame(ID = c(1L, 1L), TERM = c("x", "y"))
  attr(adae$TERM, "label") <- "Reported term"
  attr(adae$ID, "label") <- "second table"
  out <- sentence_data(dm::dm(adsl, adae))

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("ID", "ARM", "TERM"))
  expect_identical(nrow(out), 0L)
  expect_identical(attr(out$ARM, "label"), "Treatment arm")
  expect_identical(attr(out$TERM, "label"), "Reported term")
  # the first table's column is the one kept
  expect_null(attr(out$ID, "label"))
  expect_identical(levels(out$ARM), c("A", "B"))

  tpl <- sb_sentence_templates("#| title: By {label(ARM)}, {n} rows")
  expect_identical(
    sb_resolved_titles(tpl, data = out)$title, "By Treatment arm, 0 rows"
  )
})

test_that("a method of another package decides what the sentence reads", {
  registerS3method(
    "sentence_data", "zz_bundle",
    function(x, script = NULL, ...) {
      structure(
        x$tables[[1L]],
        blockr_filters = paste("script has", nchar(script), "characters")
      )
    },
    envir = asNamespace("blockr.script")
  )
  bundle <- structure(list(tables = list(iris)), class = "zz_bundle")
  script <- "#| title: {n} rows, {filters}\nNROW(data$tables)"
  trail <- paste("script has", nchar(script), "characters")
  blk <- new_script_block(script)
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      got <- result_titles(session)
      expect_identical(got$title, paste("150 rows,", trail))
      expect_identical(got$filters, trail)
    },
    args = block_args(blk, shiny::reactive(bundle))
  )

  # a method that fails leaves the data tokens empty
  broken <- structure(list(tables = list()), class = "zz_bundle")
  expect_error(sentence_data(broken, "x"))
  expect_null(sb_sentence_data(broken, "x"))
})


# 6. the busy cue --------------------------------------------------------------

test_that("an evaluation says when it starts and when it is over", {
  sent <- list()
  flushed <- list()
  session <- list(
    ns = shiny::NS("blk"),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- c(list(type = type), message)
    },
    onFlushed = function(fun, once = TRUE) {
      flushed[[length(flushed) + 1L]] <<- fun
    }
  )
  sb_busy(session)
  expect_identical(
    sent,
    list(list(type = "blockr-script-busy", block = "blk-", busy = TRUE))
  )
  # the end goes out after the flush
  expect_length(flushed, 1L)
  flushed[[1L]]()
  expect_identical(sent[[2L]]$busy, FALSE)

  # outside Shiny there is nobody to tell
  expect_null(sb_busy(NULL))
})

test_that("the end is sent when the script fails", {
  blk <- new_script_block("stop(\"no\")")
  sent <- list()
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      # the messages the real sb_busy() sends from the block's session
      root <- session
      while (inherits(root, "session_proxy")) root <- .subset2(root, "parent")
      unlockBinding("sendCustomMessage", root)
      root$sendCustomMessage <- function(type, message) {
        if (identical(type, "blockr-script-busy")) {
          sent[[length(sent) + 1L]] <<- message
        }
      }
      session$flushReact()
      expect_null(session$returned$result())
      expect_identical(
        sent,
        list(
          list(block = session$ns(""), busy = TRUE),
          list(block = session$ns(""), busy = FALSE)
        )
      )
      # the block the message names is the one the head row is marked with
      expect_identical(sb_head_key(session$ns("expr-")), session$ns(""))
    },
    args = block_args(blk)
  )
  expect_length(sent, 2L)
})


# 7. a result updated in place --------------------------------------------------

# A result class with both methods, and each test says what they do.
live <- function(code) {
  sprintf("structure(list(data = %s), class = \"live_result\")", code)
}

new_live_block <- function(script = live("data"), values = list(), ...) {
  new_script_block(
    script, values, class = "live_block", block_metadata = list(), ...
  )
}

live_method <- new.env(parent = emptyenv())
registerS3method(
  "block_result_output", "live_result",
  function(result, block, session, ...) {
    live_method$draws <- live_method$draws + 1L
    live_method$draw(result, block, session, ...)
  },
  envir = asNamespace("blockr.script")
)
registerS3method(
  "block_result_update", "live_result",
  function(result, block, session, ...) {
    live_method$updates <- live_method$updates + 1L
    live_method$update(result, block, session, ...)
  },
  envir = asNamespace("blockr.script")
)
set_live_method <- function(draw = NULL, update = NULL) {
  live_method$draws <- 0L
  live_method$updates <- 0L
  live_method$draw <- if (is.null(draw)) {
    function(result, block, session, ...) {
      shiny::renderUI(paste("live", nrow(result$data)))
    }
  } else {
    draw
  }
  live_method$update <- if (is.null(update)) {
    function(result, block, session, ...) TRUE
  } else {
    update
  }
}
live_counts <- function() {
  c(draws = live_method$draws, updates = live_method$updates)
}

test_that("the first result is drawn and the next one updates it", {
  seen <- NULL
  set_live_method(
    draw = function(result, block, session, ...) {
      result_head(session, titles = list(title = "Live"))
      shiny::renderUI(paste("live", nrow(result$data)))
    },
    update = function(result, block, session, ...) {
      result_head(session, titles = list(title = "Live"))
      seen <<- nrow(result$data)
      TRUE
    }
  )
  blk <- new_live_block(paste0("n <- 2\n", live("utils::head(data, n)")))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 0L))
      expect_match(as.character(output$result$html), "live 2", fixed = TRUE)
      head <- head_of(session)
      expect_true(head$draw$now$shown)

      titles <- 0L
      obs <- shiny::observe({
        head$titles()
        titles <<- titles + 1L
      })
      session$flushReact()

      session$returned$state$values(list(n = 3))
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 1L))
      expect_identical(seen, 3L)
      # the head row is as the first render left it, and was not redrawn
      expect_identical(shiny::isolate(head$titles()), list(title = "Live"))
      expect_identical(titles, 1L)

      session$returned$state$values(list(n = 4))
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 2L))
      expect_identical(seen, 4L)
      obs$destroy()
    },
    args = block_args(blk)
  )
})

test_that("an update leaves the output as it is in the browser", {
  session <- shiny::MockShinySession$new()
  keep <- sb_keep_output()
  expect_s3_class(keep, "shiny.render.function")
  expect_error(
    shiny::isolate(keep(shinysession = session, name = "result")),
    class = "shiny.output.cancel"
  )
})

test_that("another result class is drawn from scratch", {
  set_live_method()
  set_tiny_method(function(result, block, session, ...) shiny::renderUI("tiny"))
  blk <- new_live_block(live("data"))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 0L))

      session$returned$state$script(tiny("data"))
      session$flushReact()
      expect_match(as.character(output$result$html), "tiny", fixed = TRUE)
      expect_identical(live_counts(), c(draws = 1L, updates = 0L))

      session$returned$state$script(live("utils::head(data, 4)"))
      session$flushReact()
      expect_identical(live_counts(), c(draws = 2L, updates = 0L))
      expect_match(as.character(output$result$html), "live 4", fixed = TRUE)
    },
    args = block_args(blk)
  )
})

test_that("a result after an error or a missing input is drawn from scratch", {
  set_live_method()
  rows <- shiny::reactiveVal(10L)
  there <- shiny::reactiveVal(TRUE)
  data <- shiny::reactive({
    shiny::req(there())
    utils::head(datasets::iris, rows())
  })
  blk <- new_live_block(paste0(
    "if (nrow(data) < 5) stop(\"too few rows\")\n", live("data")
  ))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      rows(20L)
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 1L))

      # the script fails, then returns again
      rows(2L)
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 1L))
      expect_null(head_of(session)$draw$now)
      rows(30L)
      session$flushReact()
      expect_identical(live_counts(), c(draws = 2L, updates = 1L))
      expect_match(as.character(output$result$html), "live 30", fixed = TRUE)

      # the input goes away, then comes back
      there(FALSE)
      session$flushReact()
      rows(40L)
      there(TRUE)
      session$flushReact()
      expect_identical(live_counts(), c(draws = 3L, updates = 1L))
      expect_match(as.character(output$result$html), "live 40", fixed = TRUE)

      # and the next one updates again
      rows(50L)
      session$flushReact()
      expect_identical(live_counts(), c(draws = 3L, updates = 2L))
    },
    args = block_args(blk, data)
  )
})

test_that("a method that declines, or fails, has the result drawn", {
  does <- new.env()
  does$update <- TRUE
  set_live_method(
    update = function(result, block, session, ...) {
      result_head(session, titles = list(title = "From the update"))
      if (identical(does$update, "fail")) stop("update failed")
      does$update
    }
  )
  blk <- new_live_block(paste0("n <- 2\n", live("utils::head(data, n)")))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      head <- head_of(session)
      session$returned$state$values(list(n = 3))
      session$flushReact()
      expect_identical(live_counts(), c(draws = 1L, updates = 1L))
      expect_identical(
        shiny::isolate(head$titles()), list(title = "From the update")
      )

      # declined: drawn from scratch, and what it asked of the head row is
      # not kept
      does$update <- FALSE
      session$returned$state$values(list(n = 4))
      session$flushReact()
      expect_identical(live_counts(), c(draws = 2L, updates = 2L))
      expect_identical(shiny::isolate(head$titles()), list())
      expect_match(as.character(output$result$html), "live 4", fixed = TRUE)

      # failed: core records the error and renders again, from scratch
      does$update <- "fail"
      session$returned$state$values(list(n = 5))
      session$flushReact()
      expect_identical(live_method$updates, 3L)
      drawn <- live_method$draws
      expect_gt(drawn, 2L)
      expect_match(as.character(output$result$html), "live 5", fixed = TRUE)

      does$update <- TRUE
      session$returned$state$values(list(n = 6))
      session$flushReact()
      expect_identical(live_counts(), c(draws = drawn, updates = 4L))
    },
    args = block_args(blk)
  )
})

test_that("an update reads the sentence and follows it", {
  title <- NULL
  set_live_method(update = function(result, block, session, ...) {
    title <<- result_titles(session)$title
    TRUE
  })
  blk <- new_live_block(paste(
    "word <- \"first\"",
    "#| title: The {@word} {n}",
    live("data"),
    sep = "\n"
  ))
  shiny::testServer(
    blockr.core::get_s3_method("block_server", blk),
    {
      session$flushReact()
      session$setInputs(`expr-slot` = list(arg = "word", value = "second"))
      session$flushReact()
      expect_gte(live_method$updates, 1L)
      expect_identical(live_method$draws, 1L)
      expect_identical(title, "The second 150")
    },
    args = block_args(blk)
  )
})

test_that("a result that never reached the browser is not updated", {
  set_live_method(draw = function(result, block, session, ...) {
    shiny::renderText("live")
  })
  blk <- new_live_block()
  result <- structure(list(data = datasets::iris), class = "live_result")
  session <- shiny::MockShinySession$new()

  blockr.core::block_render_trigger(blk, session)
  first <- blockr.core::block_output(blk, result, session)
  # evaluated again before Shiny ran the first function (a hidden output)
  blockr.core::block_render_trigger(blk, session)
  second <- blockr.core::block_output(blk, result, session)
  expect_identical(live_counts(), c(draws = 2L, updates = 0L))

  # The function has run, and its value is sent when the flush ends. An
  # update now would be in the browser before the HTML it is for, so a
  # second evaluation in the same flush is drawn too.
  expect_identical(shiny::isolate(second()), "live")
  expect_false(sb_head(session, session$ns(""))$draw$now$shown)
  blockr.core::block_render_trigger(blk, session)
  third <- blockr.core::block_output(blk, result, session)
  expect_identical(live_counts(), c(draws = 3L, updates = 0L))

  # the flush has gone out: the next result updates
  expect_identical(shiny::isolate(third()), "live")
  session$flushReact()
  expect_true(sb_head(session, session$ns(""))$draw$now$shown)
  blockr.core::block_render_trigger(blk, session)
  fourth <- blockr.core::block_output(blk, result, session)
  expect_identical(live_counts(), c(draws = 3L, updates = 1L))

  # a caller that leaves out block_render_trigger() never updates
  fifth <- blockr.core::block_output(blk, result, session)
  expect_identical(live_counts(), c(draws = 4L, updates = 1L))
})

test_that("a block added again under the same id starts from scratch", {
  set_live_method(draw = function(result, block, session, ...) {
    shiny::renderText("live")
  })
  blk <- new_live_block()
  result <- structure(list(data = datasets::iris), class = "live_result")
  root <- shiny::MockShinySession$new()
  start <- function() {
    session <- root$makeScope("block_a")
    shiny::withReactiveDomain(session, blk[["expr_server"]]("expr"))
    session
  }
  draw <- function(session) {
    blockr.core::block_render_trigger(blk, session)
    render <- shiny::withReactiveDomain(
      session, blockr.core::block_output(blk, result, session)
    )
    # an update leaves a function that cancels
    try(shiny::isolate(render()), silent = TRUE)
    root$flushReact()
  }

  session <- start()
  draw(session)
  head <- sb_head(session, "block_a-")
  expect_true(head$draw$now$shown)
  draw(session)
  expect_identical(live_counts(), c(draws = 1L, updates = 1L))

  # blockr.core removes a block by destroying its scope. Shiny then refuses
  # every reactive made in it, and the head state is gone with it.
  root$destroy("block_a")
  expect_null(root$userData$blockr_script_head[["head:block_a-"]])

  # the block's server starts again under the same id
  session <- expect_no_error(start())
  again <- sb_head(session, "block_a-")
  expect_false(identical(again, head))
  expect_null(shiny::isolate(again$tools()))
  expect_null(again$draw$now)
  draw(session)
  expect_identical(live_counts(), c(draws = 2L, updates = 1L))
})

test_that("a class without an update method is drawn as before", {
  render <- shiny::renderUI("tiny")
  set_tiny_method(function(result, block, session, ...) render)
  blk <- new_tiny_block()
  result <- structure(list(data = datasets::iris), class = "tiny_result")
  session <- shiny::MockShinySession$new()

  expect_false(sb_updates_in_place(result))
  expect_true(sb_updates_in_place(structure(list(), class = "live_result")))
  expect_false(block_result_update(result, blk, session))
  for (i in 1:2) {
    blockr.core::block_render_trigger(blk, session)
    # the method's own function, not a copy of it
    expect_identical(blockr.core::block_output(blk, result, session), render)
    expect_null(sb_head(session, session$ns(""))$draw$now)
  }
})
