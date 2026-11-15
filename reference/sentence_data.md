# The data frame the sentence reads

The data tokens of a sentence (`{n}`, `{label(col)}`, `{col}`,
`{filters}`) and the column labels in the menu of a word read a data
frame. The script block calls this generic on its first input to get
one, so a package whose blocks hand on something else can say what the
sentence sees.

## Usage

``` r
sentence_data(x, script = NULL, ...)

# Default S3 method
sentence_data(x, script = NULL, ...)
```

## Arguments

- x:

  The block's first input.

- script:

  The block's script, one string. A method can read it to answer for the
  tables the script names.

- ...:

  Not used by the default method.

## Value

A data frame, or `NULL` when the sentence has nothing to read.

## Details

The default method returns a data frame as it is. A `dm` gives one data
frame with the columns of all its tables side by side and no rows, with
the column labels kept. Where two tables have a column of the same name,
the first table's is used. `{label(col)}` then works for a column of any
table, and `{n}` is 0. Anything else gives `NULL`: the data tokens are
empty then, and a clause in `[ ... ]` that holds one leaves.

A method returns a data frame or `NULL`. A character vector in the
attribute `blockr_filters` of that data frame is the filter trail that
`{filters}` prints and that
[`result_titles()`](https://bristolmyerssquibb.github.io/blockr.script/reference/result_titles.md)
returns as `filters`.

## Examples

``` r
sentence_data(datasets::iris[1:3, ])
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
sentence_data("not a data frame")
#> NULL

if (FALSE) { # \dontrun{
# A package whose blocks hand on a dm with a filter trail
sentence_data.dm <- function(x, script = NULL, ...) {
  out <- NextMethod()
  attr(out, "blockr_filters") <- my_filter_trail(x)
  out
}
} # }
```
