# Inline R in markdown

Writes the value of every `` `r expr` `` in a markdown string into the
text, as knitr does for inline code: each expression is evaluated with
the objects in `data` in scope, and its value replaces the code.

## Usage

``` r
inline_r(text, data = list())
```

## Arguments

- text:

  A markdown string.

- data:

  A named list of objects the expressions can use.

## Value

The markdown string with the values written in.

## Details

Left alone: code in fenced blocks, other inline code, and inline code
written between double backticks to show the syntax
(``` `` `r x` `` ```). Code in a block indented by four spaces is
evaluated, as knitr does.

A value has to be a vector of at most 100 elements. A number is written
in full (`1000000`, not `1e+06`) and several values are joined by
commas. `NULL` writes nothing. When the code holds several expressions,
the value of the last one is written.

Each expression is evaluated in a fresh environment whose parent is the
global environment. `data` is always bound there (`NULL` when the list
has none), so it never resolves to
[`utils::data`](https://rdrr.io/r/utils/data.html). `<<-` writes to the
global environment, which all sessions of a Shiny app share.

## Examples

``` r
inline_r("There are `r nrow(data)` cars.", list(data = mtcars))
#> [1] "There are 32 cars."
```
