# A web page as a block result

`html_page()` returns an object that draws as a sandboxed iframe. A
script block that ends with it shows the page on its card. Give it
either `html`, a page you write, or `url`, a page that lives elsewhere.

## Usage

``` r
html_page(
  html = NULL,
  url = NULL,
  data = NULL,
  height = "auto",
  sandbox = NULL,
  max_rows = 1000L
)
```

## Arguments

- html:

  The page, a single string or a character vector of lines.

- url:

  A URL.

- data:

  A data frame, a list or a vector that scripts in an `html` page read
  as `blockr.data`. `NULL` for none.

- height:

  Height of the frame: a number of pixels or a CSS length such as
  `"400px"` or `"50vh"`. `"auto"` fits an `html` page to its content; a
  URL page cannot be measured and defaults to `"600px"`.

- sandbox:

  The iframe's `sandbox` attribute, a single string. `NULL` takes the
  default for the mode.

- max_rows:

  Rows of `data` passed to the page. More are dropped with a warning.
  `Inf` for no limit.

## Value

An object of class `html_page`.

## HTML

`html` goes into the frame as it is. To put values from R into it, build
the string in the script, with
[`paste0()`](https://rdrr.io/r/base/paste.html),
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) or a package such as
glue.
[`htmltools::htmlEscape()`](https://rstudio.github.io/htmltools/reference/htmlEscape.html)
escapes a value that may hold `<` or `&`.

Scripts in the page read `data` as JSON from `blockr.data`, an array of
row objects for a data frame, or `null` without data. Missing values are
`null`. Date-times are written in UTC (`"2026-01-02T03:04:05Z"`).

The data travels to the browser with the page, again each time the block
re-renders. `max_rows` limits it.

The page runs in an iframe with `sandbox = "allow-scripts"`: its CSS and
JS cannot reach the board. The sandbox does not keep the page from
sending its data to other hosts. A script, an image or a style sheet in
the page can load from anywhere and pass along what the page holds. With
`height = "auto"` the iframe grows to fit the page, up to 20000 pixels.

## URL

The URL has to start with `http://` or `https://`. A value from R goes
in with [`paste0()`](https://rdrr.io/r/base/paste.html);
[`utils::URLencode()`](https://rdrr.io/r/utils/URLencode.html) with
`reserved = TRUE` encodes it. The other site has to allow framing: a
site that sends `X-Frame-Options: DENY` or `SAMEORIGIN`, or a CSP
`frame-ancestors` that does not list the board's host, stays blank. A
link above the frame opens the page in a new tab.

The frame keeps `allow-same-origin`, because an external app needs its
own cookies and storage. The sandbox therefore only protects the board
from pages on another host. A URL on the board's own host can reach the
board and everything the viewer can do on it.

## Outside a board

At the console, printing the object opens the page in the viewer. In a
script run with `Rscript` it prints one line. In an R Markdown or Quarto
document with HTML output the frame is part of the document.

## Examples

``` r
title <- "Hello"
html_page(paste0("<h1>", title, "</h1>"))
#> <html_page> html, 671 characters, height auto

bands <- data.frame(name = c("Mick", "John"), band = c("Stones", "Beatles"))
html_page(
  paste0(
    "<ul>",
    paste0("<li>", bands$name, ": ", bands$band, "</li>", collapse = ""),
    "</ul>"
  )
)
#> <html_page> html, 709 characters, height auto

html_page(
  "<ul id='l'></ul><script>
  blockr.data.forEach(function (r) {
    document.getElementById('l').insertAdjacentHTML(
      'beforeend', '<li>' + r.name + '</li>'
    );
  });
  </script>",
  data = bands
)
#> <html_page> html, 903 characters, height auto

q <- "blockr"
html_page(
  url = paste0("https://example.org/search?q=", utils::URLencode(q, TRUE))
)
#> <html_page> https://example.org/search?q=blockr, height 600px
```
