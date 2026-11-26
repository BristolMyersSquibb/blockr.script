# Script block

A block whose state is an ordinary R script. The script reads its inputs
by name and returns whatever its last statement evaluates to: a data
frame for the blocks downstream, a ggplot, a gt table, or any other
object.

## Usage

``` r
new_script_block(
  script = "n <- 6\n\nutils::head(data, n)",
  values = list(),
  ...
)
```

## Arguments

- script:

  R code, either a single string with embedded newlines or a character
  vector of lines, which is joined with `\n`.

- values:

  Named list of current control values, restored with a saved board.
  Values for names the script no longer declares are dropped.

- ...:

  Two arguments are read here, and the rest is passed to
  [`blockr.core::new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.html).

  - `starters`: the scripts "Start from" offers in the editor, as a list
    of `list(label, hint, script)`, where `hint` may be left out.
    `NULL`, the default, keeps the built-in ones.

  - `class`: classes to put in front of `script_block`, for a block
    built on this one.

  Neither is part of the block's state, which is why they are not named
  arguments: blockr.core saves every named argument of a constructor. A
  restored block gets both from its constructor again.

## Value

A block of class `script_block`.

## Details

A script that does not parse, or that has no statement returning a
result, still gives a block, with a warning. The block then shows that
reason as its error. A saved board that holds such a script loads, and
the script can be repaired in the editor.

## Inputs

The block takes any number of inputs, including none.

- The first link into the block is `data`. A script written for one
  input reads `data`, as it would in a function.

- Every link also reads by its own name: the link name, or `.arg1`,
  `.arg2`, ... for a link that has none. Only the links without a name
  are counted, so next to one named link the first unnamed one is
  `.arg1`. Links are named where they are drawn: "Edit link" or "Edit
  inputs" in the dock.

- The block is variadic (`function(id, ...args)`), so the order of the
  links decides which one is `data`. A link that is removed and drawn
  again comes last, and `data` is then another input. A script with more
  than one input should read each by its own name, which does not move.
  A link named `data` is always `data`.

- With no input connected, `data` is `NULL` and the script is a source:
  it reads a file, calls an API or builds a data frame itself.

On export each input name is replaced by the id of the upstream block,
so the exported code reads like a script someone wrote by hand.

## Controls

A top-level assignment whose right-hand side is a **plain value**
becomes a control on the block, meaning a literal, a
[`c()`](https://rdrr.io/r/base/c.html) of literals, or a call to
[`factor()`](https://rdrr.io/r/base/factor.html) or
[`as.Date()`](https://rdrr.io/r/base/as.Date.html). Everything else is
code.

    .vars <- intersect(c("AGE", "SEX", "TRT"), names(data))  # private, no control

    vars <- factor(.vars, levels = .vars)                # a multi-select
    species <- factor("setosa", levels(data$Species))    # a dropdown
    n <- 10                                              # a number box
    desc <- TRUE                                         # a checkbox

    data |>
      dplyr::filter(Species %in% species) |>
      dplyr::slice_head(n = n)

A **factor** is how a select is expressed: its levels are the choice
list, and the length of its value decides single or multiple. For a text
column write `sort(unique(data$col))` as the levels.
[`unique()`](https://rdrr.io/r/base/unique.html) alone follows the order
of the rows, and the control is drawn again whenever that changes. A
pick whose level is no longer in the data is not used: the control goes
back to the value the script wrote.

A name starting with a dot is never a control. A name the script assigns
again is not one either, since only the last assignment could be.

The script reads a control as its current value, written into the code
as a literal. Three things follow, all by design:

- A select's pick is a string (a character vector for a multi-select),
  not a factor. `levels(x)` or `as.integer(x)` on a control do not give
  what they give for the factor on the declaration line.

- Every read of the control's name is replaced, also inside data-masking
  calls. With a control `n`, `dplyr::filter(n > 5)` compares the control
  and not a column called `n`. Write `.data$n` for the column. `.env$n`
  does not reach the control: its line is no longer in the code that
  runs.

- A later declaration reads an earlier control at its current value, so
  the choices of one dropdown can depend on the pick in another:
  `col <- factor("SEX", names(data))` followed by
  `val <- factor(character(0), sort(unique(data[[col]])))`.

The optional `#|` annotation carries what a value cannot say, such as a
numeric range or a label: `n <- 10 #| number(min = 1, max = 50)`.

## Sentence

`#| title:`, `#| subtitle:` and `#| caption:` lines give the block a
sentence whose words are its controls, as blockr.viz's blocks do:

    arm <- factor("Placebo", levels = sort(unique(data$TRT01A)))
    top_n <- 8  #| number(min = 3, max = 15)
    serious <- FALSE  #| label = "serious events only"
    #| title: Top {@top_n} adverse events in {@arm}
    #| subtitle: Subjects per term[, {@serious}]

`{@arg}` prints a control's value and makes the word clickable: a select
opens its list, a number or text becomes a field, a checkbox toggles. A
clause in `[ ... ]` leaves when its value is empty, and its control is
offered as a `+ label` chip instead.

`label_off` gives the chip its own text. A checkbox's word is its label,
so `label` can say what is shown and `label_off` what a click adds:

    all_pts <- TRUE  #| label = "including all patients column", label_off = "Add all patients column"
    #| ui_subtitle: [{@all_pts}. ]

On, the subtitle reads `including all patients column.`; off, it shows
the chip `+ Add all patients column`. A chip's tooltip and ARIA label
carry the same text. Any control takes `label_off`, for the chip it gets
when its value is empty.

`{n}`, `{label(col)}` and `{col}` read the first input, as the data
frame
[`sentence_data()`](https://bristolmyerssquibb.github.io/blockr.script/reference/sentence_data.md)
makes of it. The sentence sits in the row with the gear; the controls it
names leave the grid, the others stay. A date can be named but stays a
control in the grid. The sentence is shown above any result and is not
part of the exported code.

## Exported code

The current values are written into the body as literals, so the
exported code has no assignment left for a control.

blockr.core fills the input names in with
[`bquote()`](https://rdrr.io/r/base/bquote.html), which also evaluates
every
[`.()`](https://bristolmyerssquibb.github.io/blockr.core/reference/bbquote.html)
and
[`..()`](https://bristolmyerssquibb.github.io/blockr.core/reference/bbquote.html)
the script calls itself, such as data.table's `.(x)`. Write `list(x)`
there. The editor footer names the line.

## Saved as lines

A saved board writes the script as a JSON array, one element per line,
so a change to the script shows up in a git diff as the lines that
changed.

## Building on the script block

Another package can build a block on this one. Its constructor calls
`new_script_block()` with a `class`, its own default script and, if it
likes, its own `starters`:

    new_listing_block <- function(script = "my_listing(data)", values = list(),
                                  ...) {
      blockr.script::new_script_block(
        script, values,
        starters = my_starters(),
        class = "listing_block",
        ...
      )
    }

The block has the classes `c("listing_block", "script_block", "block")`,
so every method of the script block applies to it. A saved board
restores it through `new_listing_block()`, which therefore takes
`script`, `values` and `...`, no other named argument, and passes `...`
on. Register the constructor with
[`blockr.core::register_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.html)
as any other block.

The constructor is found by its arguments: it is the nearest function in
the chain of calls that takes exactly `script`, `values` and `...`. It
may therefore reach `new_script_block()` through a helper of its own.
Where there is no such function, as for `new_script_block(class = "x")`
called at top level, the block is saved with a constructor that keeps
the `class` and the `starters` it was built with.

What such a block draws is decided by the class of the script's result,
not by the class of the block: see
[`block_result_output()`](https://bristolmyerssquibb.github.io/blockr.script/reference/block_result_output.md),
[`result_head()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_head.md)
and
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md).
For an input that is not a data frame, see
[`sentence_data()`](https://bristolmyerssquibb.github.io/blockr.script/reference/sentence_data.md).

## Examples

``` r
blk <- new_script_block("n <- 6\n\nutils::head(data, n)")

# the same script, one element per line
blk <- new_script_block(c("n <- 6", "", "utils::head(data, n)"))

# no input: the script is the source
blk <- new_script_block("data.frame(x = 1:3, y = c(\"a\", \"b\", \"c\"))")

if (interactive()) {
  library(blockr.core)
  serve(
    new_board(
      blocks = c(
        a = new_dataset_block("band_members", package = "dplyr"),
        b = new_dataset_block("band_instruments", package = "dplyr"),
        s = new_script_block("merge(data, instruments, by = \"name\")")
      ),
      links = links(
        from = c("a", "b"),
        to = c("s", "s"),
        input = c("data", "instruments")
      )
    )
  )
}
```
