# blockr.script

An R script block for
[blockr](https://blockr-org.github.io/blockr.site/) boards. The block’s
state is an ordinary R script. It reads any number of upstream results
and returns whatever its last statement evaluates to.

## Installation

``` r

# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.script")
```

## Example

``` r

library(blockr.core)
library(blockr.script)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      s = new_script_block(c(
        "species <- factor(\"setosa\", levels(data$Species))",
        "n <- 5",
        "",
        "data |>",
        "  subset(Species == species) |>",
        "  utils::head(n)"
      ))
    ),
    links = links(from = "a", to = "s", input = "data")
  )
)
```

- **Inputs.** The first link is `data`. Every link is also read by its
  own name: the link name, or `.arg1`, `.arg2`, … for links without one
  (only the unnamed links are counted). Which link is first can change
  when links are removed and drawn again, so a script with several
  inputs reads each by its own name. A link named `data` is always
  `data`. With no link, `data` is `NULL` and the script is the source.
- **Controls.** A top-level assignment of a plain value becomes a
  control: a factor is a dropdown over its levels, a number a spin box,
  `TRUE`/`FALSE` a checkbox. A name starting with a dot stays private.
- **Export.** The exported code is the script with the current values
  written in and each input replaced by the id of its upstream block.

The script lives in the block’s gear tray, for whoever builds the board.
The controls and the result are on the block’s face, for whoever uses
it.

## Drawing your own result class

What a script block draws follows the class of the script’s result. A
package with its own table class registers a
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md)
method for it. The method gets the block’s Shiny session, so it can
register a download and put the button into the block’s head row, left
of the gear.

``` r

block_result_output.my_table <- function(result, block, session, ...) {
  session$output$csv <- shiny::downloadHandler(
    filename = "table.csv",
    content = function(file) {
      # the title as it reads now, without the script's `ui_*` lines
      title <- blockr.script::result_titles(session, ui = FALSE)$title
      writeLines(c(title, ""), file)
      utils::write.table(
        result$data, file, append = TRUE, sep = ",", row.names = FALSE
      )
    }
  )

  blockr.script::result_head(
    session,
    # a link or button with an icon becomes an icon button
    tools = shiny::downloadLink(
      session$ns("csv"), label = shiny::icon("download"), title = "Download"
    ),
    # shown where the script has no `#| title:` line
    titles = list(title = "My table"),
    # this method draws the caption under the table
    caption = FALSE
  )

  shiny::renderUI(shiny::tagList(
    draw_my_table(result),
    shiny::tags$p(blockr.script::result_titles(session)$caption)
  ))
}
```

The head row is emptied before every render, so the button is gone as
soon as the script returns something else.
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md)
gives the title, subtitle and caption as the text now on screen: the
script’s `#|` lines with the current control values, or the titles the
method gave.

The function the method returns replaces the block’s output, so the
browser builds the result again on every evaluation. A table then loses
its scroll position and its search text when a control changes. A class
that can change what it has drawn also registers a
[`block_result_update()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_update.md)
method, which sends the new result to the browser and returns `TRUE`:

``` r

block_result_output.my_table <- function(result, block, session, ...) {
  shiny::renderUI(my_table_widget(result$data, id = session$ns("table")))
}

block_result_update.my_table <- function(result, block, session, ...) {
  update_my_table_widget(session, session$ns("table"), result$data)
  TRUE
}
```

The block calls it when the result it drew last is still on screen and
the new one has the same class. The first result, a result of another
class and the first result after an error are drawn with
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md),
and so is a result whose update method returns `FALSE`.

A package can also ship a block of its own on top of the script block,
with its own default script and its own list under “Start from”:

``` r

new_my_table_block <- function(script = "my_table(data)", values = list(),
                               ...) {
  blockr.script::new_script_block(
    script, values,
    starters = list(
      list(label = "By group", hint = "one row per group",
           script = "my_table(data, by = \"group\")")
    ),
    class = "my_table_block",
    ...
  )
}
```

[`sentence_data()`](https://bristolmyerssquibb.github.io/blockr.script/reference/sentence_data.md)
is the generic to extend when the block’s input is not a data frame, and
[`script_expr()`](https://bristolmyerssquibb.github.io/blockr.script/reference/script_expr.md)
returns a script as one expression for a report. See
[`?new_script_block`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md),
section “Building on the script block”.
