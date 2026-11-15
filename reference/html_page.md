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
  ...,
  height = "auto",
  sandbox = NULL,
  max_rows = 1000L,
  js_data = TRUE
)
```

## Arguments

- html:

  A mustache template, a single string or a character vector of lines.

- url:

  A URL, with optional `{name}` placeholders.

- data:

  A data frame, a list or a vector the page reads. `NULL` for none.

- ...:

  Further named values for the template or the URL. In a template the
  names `rows`, `nrow` and `ncol` are taken.

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

- js_data:

  Whether scripts in the page get the data as `blockr.data`. With
  `FALSE` it is `null`.

## Value

An object of class `html_page`.

## HTML

`html` is a mustache template, filled in R with whisker. The template
sees `rows` (one entry per row of `data`), `nrow`, `ncol` and any value
passed in `...`. Use `{{col}}` for an escaped value and `{{{col}}}` for
raw HTML. The escaping covers `<`, `>`, `&` and `"`, but not `'`, so
write attributes with double quotes: `<a title="{{col}}">`.

Mustache cannot read a name that holds a dot or a space: it takes
`{{Sepal.Length}}` for `Length` inside `Sepal`. The template therefore
also gets each such column under a safe name, with dots and spaces
replaced by underscores: `{{Sepal_Length}}`. A template that writes the
real name gets a warning.

Scripts in the page read the same data as JSON from `blockr.data`, an
array of row objects with the real column names, or `null` without data.
Missing values differ between the two: the template writes `NA`, `Inf`
and `NaN` as text, and `blockr.data` holds `null`. Date-times are
written in UTC (`"2026-01-02T03:04:05Z"`).

The data travels to the browser with the page, once in the filled
template and once in `blockr.data`, and again each time the block
re-renders. `max_rows` limits it. A page that only uses the template can
set `js_data = FALSE` to leave `blockr.data` out, which about halves the
size. `rows` is only built when the template uses it.

The page runs in an iframe with `sandbox = "allow-scripts"`: its CSS and
JS cannot reach the board. The sandbox does not keep the page from
sending its data to other hosts. A script, an image or a style sheet in
the page can load from anywhere and pass along what the page holds. With
`height = "auto"` the iframe grows to fit the page, up to 20000 pixels.

## URL

`{name}` in `url` is filled from the first row of `data`, or from a
value in `...`, and URL-encoded. The filled URL has to start with
`http://` or `https://`. The other site has to allow framing: a site
that sends `X-Frame-Options: DENY` or `SAMEORIGIN`, or a CSP
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
html_page("<h1>{{title}}</h1>", title = "Hello")
#> <html_page> html, 671 characters, height auto

html_page(
  "<ul>{{#rows}}<li>{{name}}: {{band}}</li>{{/rows}}</ul>",
  data = data.frame(name = c("Mick", "John"), band = c("Stones", "Beatles"))
)
#> <html_page> html, 771 characters, height auto

html_page(url = "https://example.org/search?q={q}", q = "blockr")
#> <html_page> https://example.org/search?q=blockr, height 600px
```
