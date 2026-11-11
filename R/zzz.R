.onLoad <- function(libname, pkgname) {
  register_script_blocks()
  register_knit_print()
  invisible(NULL)
}

# knitr is only suggested. The method is registered now when knitr is already
# loaded, and otherwise at the moment it loads.
register_knit_print <- function() {
  register <- function(...) {
    registerS3method(
      "knit_print", "html_page", knit_print.html_page,
      envir = asNamespace("knitr")
    )
  }
  if (isNamespaceLoaded("knitr")) {
    register()
  }
  setHook(packageEvent("knitr", "onLoad"), register)
  invisible(NULL)
}
