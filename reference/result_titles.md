# The sentence of a script block as text

The title, subtitle and caption a script block shows, as the plain text
now on screen. For a
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
method that needs them outside the head row: in a downloaded file, or
for a caption drawn under a table.

## Usage

``` r
result_titles(session, ui = TRUE)
```

## Arguments

- session:

  The block's session, as the method received it.

- ui:

  `TRUE` for the sentence as on screen, `FALSE` for the sentence without
  the script's `ui_*` lines.

## Value

A list with

- `title`, `subtitle` and `caption`, each one string or `NULL`;

- `filters`, the filter trail that `{filters}` prints, as a character
  vector (empty when the input carries none);

- `declared`, a character vector with those of `"title"`, `"subtitle"`
  and `"caption"` that the script has a line for;

- `templates`, a list keyed by the names in `declared`, each the line's
  text as written, `""` for a line that is switched off.

## Details

A line the script declares is resolved with the current control values
and the data tokens (`{n}`, `{label(col)}`). A line it does not declare
is the title the method gave to
[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md).
A line that is neither, or that the script switched off, is `NULL`.

The sentence can change without the block being evaluated again, for
example when a control that only the title names is changed. So read it
where it is used and do not keep a copy: inside a render function the
call takes a reactive dependency and the output follows the sentence,
inside the `content` function of a `downloadHandler` it gives the
current text.

## Lines for the app only

A script can carry `#| ui_title:`, `#| ui_subtitle:` and
`#| ui_caption:` lines, which are meant for the app: a `ui_title` or
`ui_caption` replaces the plain line on screen and a `ui_subtitle` is
added to it. With `ui = TRUE` the result is the sentence as on screen. A
download or a report passes `ui = FALSE` and gets the sentence built
from the plain lines only. Both forms follow the controls.

## What the script declares

`declared` names the lines the script has, also one it switched off with
nothing after the colon. So for `caption`:

- in `declared` and a string: the script's caption;

- in `declared` and `NULL`: the script switched the caption off, or it
  resolved to nothing;

- not in `declared` and a string: the title the method gave;

- not in `declared` and `NULL`: there is none.

`templates` holds the declared lines as the script wrote them, before
anything is filled in, for a rule that depends on the wording, such as a
caption that is exactly `{filters}`.

## See also

[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md)

## Examples

``` r
if (FALSE) { # \dontrun{
block_result_output.my_table <- function(result, block, session, ...) {
  session$output$csv <- shiny::downloadHandler(
    filename = "table.csv",
    content = function(file) {
      # without the lines the script wrote for the app only
      writeLines(c(result_titles(session, ui = FALSE)$title, ""), file)
      utils::write.table(result$data, file, append = TRUE, sep = ",")
    }
  )
  shiny::renderUI(draw_my_table(result))
}
} # }
```
