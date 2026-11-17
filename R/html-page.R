#' A web page as a block result
#'
#' `html_page()` returns an object that draws as a sandboxed iframe. A script
#' block that ends with it shows the page on its card. Give it either `html`,
#' a page you write, or `url`, a page that lives elsewhere.
#'
#' @section HTML:
#' `html` goes into the frame as it is. To put values from R into it, build
#' the string in the script, with [paste0()], [sprintf()] or a package such
#' as glue. [htmltools::htmlEscape()] escapes a value that may hold `<` or
#' `&`.
#'
#' Scripts in the page read `data` as JSON from `blockr.data`, an array of
#' row objects for a data frame, or `null` without data. Missing values are
#' `null`. Date-times are written in UTC (`"2026-01-02T03:04:05Z"`).
#'
#' The data travels to the browser with the page, again each time the block
#' re-renders. `max_rows` limits it.
#'
#' The page runs in an iframe with `sandbox = "allow-scripts"`: its CSS and JS
#' cannot reach the board. The sandbox does not keep the page from sending its
#' data to other hosts. A script, an image or a style sheet in the page can
#' load from anywhere and pass along what the page holds. With
#' `height = "auto"` the iframe grows to fit the page, up to 20000 pixels.
#'
#' @section URL:
#' The URL has to start with `http://` or `https://`. A value from R goes in
#' with [paste0()]; [utils::URLencode()] with `reserved = TRUE` encodes it.
#' The other site has to allow framing: a site that sends
#' `X-Frame-Options: DENY` or `SAMEORIGIN`, or a CSP `frame-ancestors` that
#' does not list the board's host, stays blank. A link above the frame opens
#' the page in a new tab.
#'
#' The frame keeps `allow-same-origin`, because an external app needs its own
#' cookies and storage. The sandbox therefore only protects the board from
#' pages on another host. A URL on the board's own host can reach the board
#' and everything the viewer can do on it.
#'
#' @section Outside a board:
#' At the console, printing the object opens the page in the viewer. In a
#' script run with `Rscript` it prints one line. In an R Markdown or Quarto
#' document with HTML output the frame is part of the document.
#'
#' @param html The page, a single string or a character vector of lines.
#' @param url A URL.
#' @param data A data frame, a list or a vector that scripts in an `html`
#'   page read as `blockr.data`. `NULL` for none.
#' @param height Height of the frame: a number of pixels or a CSS length such
#'   as `"400px"` or `"50vh"`. `"auto"` fits an `html` page to its content; a
#'   URL page cannot be measured and defaults to `"600px"`.
#' @param sandbox The iframe's `sandbox` attribute, a single string. `NULL`
#'   takes the default for the mode.
#' @param max_rows Rows of `data` passed to the page. More are dropped with a
#'   warning. `Inf` for no limit.
#'
#' @return An object of class `html_page`.
#'
#' @examples
#' title <- "Hello"
#' html_page(paste0("<h1>", title, "</h1>"))
#'
#' bands <- data.frame(name = c("Mick", "John"), band = c("Stones", "Beatles"))
#' html_page(
#'   paste0(
#'     "<ul>",
#'     paste0("<li>", bands$name, ": ", bands$band, "</li>", collapse = ""),
#'     "</ul>"
#'   )
#' )
#'
#' html_page(
#'   "<ul id='l'></ul><script>
#'   blockr.data.forEach(function (r) {
#'     document.getElementById('l').insertAdjacentHTML(
#'       'beforeend', '<li>' + r.name + '</li>'
#'     );
#'   });
#'   </script>",
#'   data = bands
#' )
#'
#' q <- "blockr"
#' html_page(
#'   url = paste0("https://example.org/search?q=", utils::URLencode(q, TRUE))
#' )
#'
#' @export
html_page <- function(html = NULL, url = NULL, data = NULL,
                      height = "auto", sandbox = NULL, max_rows = 1000L) {

  if (is.null(html) == is.null(url)) {
    stop("Give exactly one of `html` and `url`.", call. = FALSE)
  }

  if (!(is.null(data) || is.data.frame(data) || is.list(data) ||
          is.atomic(data))) {
    stop(
      "`data` must be a data frame, a list or a vector, not ",
      class(data)[1L], ".", call. = FALSE
    )
  }

  if (is.null(max_rows)) {
    max_rows <- Inf
  }
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
        max_rows < 0) {
    stop("`max_rows` must be a single number.", call. = FALSE)
  }

  if (!is.null(sandbox) && !(is.character(sandbox) && length(sandbox) == 1L &&
                               !is.na(sandbox))) {
    stop("`sandbox` must be a single string.", call. = FALSE)
  }

  if (!identical(height, "auto")) {
    if (length(height) != 1L) {
      stop("`height` must be a single value.", call. = FALSE)
    }
    # A number becomes pixels. Anything that is not a CSS length stops here,
    # so nothing else can be written into the frame's style.
    height <- htmltools::validateCssUnit(height)
  }

  if (!is.null(url)) {
    if (!is.null(data)) {
      stop("`data` is only read by an `html` page.", call. = FALSE)
    }
    if (!is.character(url) || length(url) != 1L || is.na(url)) {
      stop("`url` must be a single string.", call. = FALSE)
    }
    # The URL goes into the frame's `src` and into a link. Anything but a web
    # address (`javascript:`, `data:`, a relative path) would run or load on
    # the board's own origin.
    if (!grepl("^https?://", url, ignore.case = TRUE)) {
      stop(
        "The URL must start with `http://` or `https://`: ", url,
        call. = FALSE
      )
    }
    if (identical(height, "auto")) {
      height <- "600px"
    }
    res <- list(
      url = url,
      height = height,
      sandbox = if (is.null(sandbox)) paste(
        "allow-scripts allow-same-origin allow-forms allow-popups",
        "allow-popups-to-escape-sandbox allow-downloads"
      ) else sandbox
    )
  } else {
    if (!is.character(html)) {
      stop("`html` must be a character vector.", call. = FALSE)
    }
    if (is.data.frame(data) && nrow(data) > max_rows) {
      warning(
        "`data` has ", nrow(data), " rows; the page gets the first ",
        max_rows, ".", call. = FALSE
      )
      data <- data[seq_len(max_rows), , drop = FALSE]
    }
    res <- list(
      srcdoc = html_page_document(paste(html, collapse = "\n"), data),
      height = height,
      sandbox = if (is.null(sandbox)) "allow-scripts" else sandbox
    )
  }

  structure(res, class = "html_page")
}

#' The page as it goes into `srcdoc`
#'
#' Two things go into the head ahead of the author's own: `blockr.data`, so a
#' script in the page can read it at any point, and a reporter that posts the
#' page height to the board for `height = "auto"`. A page without a `<head>`
#' gets a skeleton; one with a head keeps it and gets the two inserted.
#' @noRd
html_page_document <- function(body, data) {
  head <- paste0(
    "<meta charset=\"utf-8\">",
    "<style>body{margin:8px;font:14px/1.5 system-ui,-apple-system,",
    "sans-serif;color:#111827}</style>",
    "<script>window.blockr={data:", html_page_json(data), "};</script>",
    "<script>", html_page_height_js, "</script>"
  )

  # `<head>` or `<head lang="en">`, but not `<header>`. The head is put in
  # with substr() and paste0(): as a replacement string in sub() it would
  # lose every backslash of the JSON.
  m <- regexpr("<head(\\s[^>]*)?>", body, ignore.case = TRUE, perl = TRUE)
  if (m > 0L) {
    end <- m + attr(m, "match.length") - 1L
    paste0(substr(body, 1L, end), head, substr(body, end + 1L, nchar(body)))
  } else {
    paste0(
      "<!doctype html><html><head>", head, "</head><body>", body,
      "</body></html>"
    )
  }
}

# `data` as JSON that is safe inside a <script> element. `<`, `>` and `&`
# are written as unicode escapes, so no value can close the element
# ("</script>") or open a comment ("<!--<script"). U+2028 and U+2029 end a
# line in JavaScript before ES2019 and are escaped as well.
html_page_json <- function(data) {
  json <- if (is.null(data)) {
    "null"
  } else if (is.data.frame(data)) {
    jsonlite::toJSON(
      data, dataframe = "rows", na = "null", POSIXt = "ISO8601", UTC = TRUE,
      digits = NA
    )
  } else {
    jsonlite::toJSON(
      data, auto_unbox = TRUE, na = "null", POSIXt = "ISO8601", UTC = TRUE,
      digits = NA
    )
  }
  json <- enc2utf8(as.character(json))
  esc <- c(
    "<" = "\\u003c", ">" = "\\u003e", "&" = "\\u0026",
    "\u2028" = "\\u2028", "\u2029" = "\\u2029"
  )
  for (ch in names(esc)) {
    json <- gsub(ch, esc[[ch]], json, fixed = TRUE)
  }
  json
}

html_page_height_js <- paste(
  "(function(){var last=0;function post(){",
  "var b=document.body;if(!b)return;var cs=getComputedStyle(b);",
  "var h=b.getBoundingClientRect().bottom+parseFloat(cs.marginBottom||0);",
  "if(Math.abs(h-last)<2)return;last=h;",
  "parent.postMessage({blockrHtmlPage:true,height:h},'*');}",
  "addEventListener('load',post);",
  "addEventListener('DOMContentLoaded',function(){",
  "if(window.ResizeObserver)new ResizeObserver(post).observe(document.body);",
  "});})();"
)

#' The board side of the height report
#'
#' `inst/js/html-page.js` listens for the height a page posts and sizes its
#' frame. It is a file and not an inline script, so a board with a
#' `script-src` policy that forbids inline scripts keeps auto height.
#' @noRd
html_page_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-html-page",
    version = js_version("html-page.js"),
    src = system.file("js", package = "blockr.script"),
    script = "html-page.js"
  )
})

#' @exportS3Method htmltools::as.tags
as.tags.html_page <- function(x, ...) {
  html_page_tags(x)
}

#' The frame of an `html_page`
#'
#' With `id` and `doc`, the frame is written without its document: the block
#' sends that in a message of its own (see `block_result_output.html_page()`)
#' and `inst/js/html-page.js` puts it into the frame that names the same
#' `doc`. Without them the document is the frame's `srcdoc` attribute, which
#' works anywhere HTML does, such as a report.
#'
#' @param x An `html_page`.
#' @param id Element id of the frame.
#' @param doc Label of the document the frame waits for.
#' @noRd
html_page_tags <- function(x, id = NULL, doc = NULL) {
  auto <- identical(x$height, "auto")
  # The frame and the link carry their styles inline on purpose: the page
  # also has to draw in a report, where the package's style sheet is not
  # loaded.
  frame <- htmltools::tags$iframe(
    id = id,
    class = "blockr-html-page",
    src = x$url,
    srcdoc = if (is.null(doc)) x$srcdoc,
    `data-doc` = doc,
    sandbox = x$sandbox,
    `data-auto` = if (auto) NA,
    referrerpolicy = if (!is.null(x$url)) "no-referrer",
    style = paste0(
      "display:block;width:100%;border:0;",
      "height:", if (auto) "150px" else x$height, ";"
    )
  )
  link <- if (!is.null(x$url)) {
    htmltools::tags$div(
      style = "text-align:right;font-size:12px;margin-bottom:4px;",
      htmltools::tags$a(
        href = x$url, target = "_blank", rel = "noopener noreferrer",
        "Open in new tab"
      )
    )
  }
  htmltools::tagList(
    if (auto || !is.null(doc)) html_page_dep(),
    link,
    frame
  )
}

# Counts the documents sent, so every one has its own label.
html_page_sent <- new.env(parent = emptyenv())

# Draw an `html_page` on a script block.
#
# A page built from `html` can be large: the template's output plus the data
# as JSON. As a `srcdoc` attribute inside the output's HTML every quote in it
# becomes `&quot;`, which nearly doubles what travels to the browser. Here
# the document goes in a message of its own and the frame is drawn empty,
# labelled with the document it waits for. The label lets the browser pair
# the two whichever arrives first.
#
# A page built from `url` has no document and draws as it does anywhere.
#' @rdname block_result_output
#' @export
block_result_output.html_page <- function(result, block, session, ...) {
  if (is.null(result$srcdoc)) {
    return(shiny::renderUI(html_page_tags(result)))
  }
  html_page_sent$n <- sum(html_page_sent$n, 1L)
  id <- session$ns("html_page_frame")
  doc <- as.character(html_page_sent$n)
  session$sendCustomMessage(
    "blockr-html-page-doc",
    list(id = id, doc = doc, html = result$srcdoc)
  )
  shiny::renderUI(html_page_tags(result, id = id, doc = doc))
}

#' @export
format.html_page <- function(x, ...) {
  if (is.null(x$url)) {
    paste0(
      "<html_page> html, ", format(nchar(x$srcdoc), big.mark = ","),
      " characters, height ", x$height
    )
  } else {
    paste0("<html_page> ", x$url, ", height ", x$height)
  }
}

# At the console the page opens in the viewer. Anywhere else (Rscript, R CMD
# check, a log) it prints one line and opens nothing.
#' @export
print.html_page <- function(x, ...) {
  if (interactive()) {
    print(htmltools::browsable(htmltools::as.tags(x)))
  } else {
    cat(format(x), "\n", sep = "")
  }
  invisible(x)
}

# Registered for knitr's generic in .onLoad, so a report made from exported
# code shows the frame.
knit_print.html_page <- function(x, ...) {
  knitr::knit_print(htmltools::as.tags(x), ...)
}
