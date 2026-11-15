# A script as one expression

The code a script block runs, built from the script alone: the controls
are replaced by their values and the lines that declared them are gone.
It is for a place that has the script but no block and no input, such as
the call a report makes.

## Usage

``` r
script_expr(script, values = list(), data = "data")
```

## Arguments

- script:

  The script, as for
  [`new_script_block()`](https://bristolmyerssquibb.github.io/blockr.script/reference/new_script_block.md).

- values:

  Named list of control values, as a block's state holds them.

- data:

  The name the expression reads the first input by. Every read of `data`
  in the script becomes a read of this name.

## Value

An expression, or `NULL` when the script does not parse or has no
statement left.

## Details

A control takes its value from `values`, and the script's own value
where `values` has none. Without an input, a declaration that reads the
input, such as `factor("a", levels = unique(data$col))`, cannot be
worked out. When `values` holds a value for it, that value is written
in. When it does not, the line stays in the expression as code and is
evaluated with the rest.

## Examples

``` r
script_expr("n <- 6\n\nutils::head(data, n)", list(n = 3))
#> utils::head(data, 3)

# the levels need the input, the value comes from `values`
script_expr(
  "arm <- factor(\"A\", levels = unique(data$ARM))\nsubset(data, ARM == arm)",
  values = list(arm = "B"),
  data = "adsl"
)
#> subset(adsl, ARM == "B")
```
