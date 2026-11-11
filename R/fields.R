# Field layout for the controls grid: which kind of field a control is, the
# grid track it takes, and the wrapper that sizes it.

#' The kind a wrapped field declares
#'
#' The script block builds its fields one at a time and only then knows what
#' the band holds, so the kind is read back off the wrapper rather than
#' threaded through every branch.
#'
#' @param field A field built by `sb_field_wrapper()`.
#' @noRd
sb_field_kind <- function(field) {
  cls <- field$attribs$class
  if (is.null(cls)) {
    return(NA_character_)
  }
  hit <- regmatches(cls, regexpr("blockr-script-field--[a-z]+", cls))
  if (!length(hit)) NA_character_ else sub("blockr-script-field--", "", hit)
}


#' Which ladder the band steps down, and its clamped column counts
#'
#' Equal tracks keep the fields aligned; what the band has to choose is where
#' the column count steps down, and that depends on what it holds. A select
#' needs room for its tags, a number or a flag does not, and a band with both
#' should be laid out for the select: at a half-width panel that is two columns
#' where the old ladder insisted on four and gave each select 154px.
#'
#' The counts are clamped to the number of fields for the same reason they
#' always were: a two-field band widened into three tracks leaves an empty one.
#'
#' @param kinds The field kinds in the band.
#' @return A list with the band's modifier class and its inline style.
#' @noRd
sb_grid_track <- function(kinds) {
  n <- length(kinds)
  list(
    class = if (any(kinds %in% c("select", "text"))) {
      "blockr-script-band--wide"
    } else {
      "blockr-script-band--knobs"
    },
    style = sprintf(
      "--blockr-script-cols:%d; --blockr-script-cols-3:%d; --blockr-script-cols-2:%d;",
      min(n, 4L), min(n, 3L), min(n, 2L)
    )
  )
}


#' Wrap one generated field, declaring what kind of control it holds
#'
#' The band is a grid of equal columns (`blockr-script-params-grid` in
#' script-block.css), and `sb_grid_track()` picks where the column count steps
#' down from the kinds of field it holds. That only works if the markup says
#' which field is which, so the kind travels as a class rather than being
#' guessed from the contents with `:has()`, which in a Shiny page restyles
#' the whole document.
#'
#' A checkbox carries its own label beside the box, so it gets an empty label
#' row as a spacer: without it the control starts 23px above its neighbours and
#' the row stops lining up.
#'
#' @param kind One of `select`, `number`, `text`, `flag`, `date`.
#' @param inner The control.
#' @noRd
sb_field_wrapper <- function(kind, inner) {
  spacer <- if (identical(kind, "flag")) {
    shiny::tags$label(
      class = "blockr-script-field-label blockr-script-field-label--spacer",
      `aria-hidden` = "true",
      shiny::HTML("&nbsp;")
    )
  }
  shiny::div(
    class = paste0("block-input-wrapper blockr-script-field--", kind),
    spacer,
    inner
  )
}
