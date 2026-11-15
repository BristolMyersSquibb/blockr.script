# HTML dependency for the prose block JS/CSS

Ships `inst/js/prose-block.js`, built from `tools/srcjs/prose-block/`
with esbuild (see `package.json`), and `inst/css/prose-block.css`.

## Usage

``` r
prose_block_dep()
```

## Value

A list of
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
objects.
