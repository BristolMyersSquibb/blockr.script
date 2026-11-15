# HTML dependencies of the script block

`script_editor_dep()` ships `inst/js/blockr-script.js`, built from
`tools/srcjs/script-editor/` with esbuild (see `package.json`). The
bundle registers the `blockr.script.editor` Shiny input binding, the
`blockr-script-set` custom-message handler and `Blockr.Script`. The
others ship the style sheet and the hand-written scripts in `inst/js`.

## Usage

``` r
script_editor_dep()

script_css_dep()

gear_editor_dep()

sentence_dep()

script_tools_dep()

script_head_dep()
```

## Value

An
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html).
