#' HTML dependencies of the script block
#'
#' `script_editor_dep()` ships `inst/js/blockr-script.js`, built from
#' `tools/srcjs/script-editor/` with esbuild (see `package.json`). The bundle
#' registers the `blockr.script.editor` Shiny input binding, the
#' `blockr-script-set` custom-message handler and `Blockr.Script`. The others
#' ship the style sheet and the hand-written scripts in `inst/js`.
#'
#' @return An [htmltools::htmlDependency()].
#' @keywords internal
script_editor_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-script",
    version = js_version("blockr-script.js"),
    src = system.file("js", package = "blockr.script"),
    script = "blockr-script.js"
  )
})

#' @rdname script_editor_dep
#' @keywords internal
script_css_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-script-css",
    version = js_version("script-block.css", "css"),
    src = system.file("css", package = "blockr.script"),
    stylesheet = "script-block.css"
  )
})

#' @rdname script_editor_dep
#' @keywords internal
gear_editor_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-script-gear",
    version = js_version("gear-editor.js"),
    src = system.file("js", package = "blockr.script"),
    script = "gear-editor.js"
  )
})

#' @rdname script_editor_dep
#' @keywords internal
sentence_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-script-sentence",
    version = js_version("sentence.js"),
    src = system.file("js", package = "blockr.script"),
    script = "sentence.js"
  )
})

#' @rdname script_editor_dep
#' @keywords internal
script_tools_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-script-tools",
    version = js_version("script-tools.js"),
    src = system.file("js", package = "blockr.script"),
    script = "script-tools.js"
  )
})

# The package version with a number taken from the files' contents. The
# version is part of the asset URL, so a browser never serves a changed
# script or style sheet from its cache.
js_version <- function(file, dir = "js") {
  version <- as.character(utils::packageVersion("blockr.script"))
  sums <- tools::md5sum(system.file(dir, file, package = "blockr.script"))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(unname(sums), tmp)
  paste0(
    version, ".",
    strtoi(substr(unname(tools::md5sum(tmp)), 1L, 6L), 16L)
  )
}
