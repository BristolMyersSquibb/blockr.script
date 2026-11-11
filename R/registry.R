#' Register the script and prose blocks
#'
#' Called on load. Exported so an app that unregistered it can add it back.
#'
#' @return `NULL`, invisibly.
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string
register_script_blocks <- function() {
  blockr.core::register_blocks(
    "new_script_block",
    name = "Script",
    description = paste(
      "Run an R script on any number of inputs, or on none. Top-level",
      "assignments of plain values become controls: a factor renders a",
      "dropdown over its levels, a number a spin box, TRUE/FALSE a checkbox.",
      "Exports as plain R with the current values written in."
    ),
    category = "transform",
    icon = "braces",
    guidance = paste(
      "Write `script` as an ordinary R script that ends with the result. Do",
      "NOT wrap it in a function and do NOT call one.",
      "\n\nInputs: the first input is `data`. Every input is also read by its",
      "own name: the link name, or `.arg1`, `.arg2`, ... for links without a",
      "name (only the unnamed links are counted, so next to one named link",
      "the first unnamed one is `.arg1`). With more than one input, read each",
      "by its own name: which input is `data` follows the order of the links.",
      "With no input, `data` is NULL and the script builds its own result.",
      "\n\nTo offer the user a control, assign a plain value at the top level:",
      "a factor renders a dropdown (its LEVELS are the choices, and a value of",
      "length > 1 makes it a multi-select), a bare number a spin box, a string",
      "a text box, TRUE/FALSE a checkbox, as.Date() a date picker. Draw the",
      "levels from the data where that is what you mean, e.g.",
      "`site <- factor(\"Basel\", sort(unique(data$site)))`, sorted so the",
      "choices do not follow the row order. A literal is always the VALUE,",
      "never the choice list. c() is a control only when all its elements are",
      "literals.",
      "\n\nThe code reads a control as its current value, written in as a",
      "literal. A factor control is a STRING there (a character vector for a",
      "multi-select), not a factor, so do not call levels() on it. Every read",
      "of a control's name is replaced, also inside dplyr verbs: with a",
      "control `n`, write `.data$n` for a column called `n`, and never",
      "`.env$n`. A later declaration may read an earlier control, e.g. the",
      "choices of one dropdown from the pick in another. Do not call `.()`",
      "in the script; write list().",
      "\n\nEverything else is code: an assignment whose right-hand side is a",
      "pipe or any other call is a local variable, not a control. A",
      "declaration is also NOT a control when the script assigns the same name",
      "again (only the last assignment can be one).",
      "\n\nA name starting with a DOT is never a control. Use that for a line",
      "that works something out from the data, e.g.",
      "`.vars <- intersect(c(\"AGE\", \"SEX\"), names(data))` above",
      "`vars <- factor(.vars, levels = .vars)`.",
      "\n\nR coding rules: prefer dplyr/tidyr chained with the base pipe |>",
      "(never %>%). Namespace-prefix every call except base and stats",
      "(dplyr::filter(), tidyr::pivot_longer())."
    ),
    arguments = list(
      blockr.core::new_arg_specs(
        script = blockr.core::new_arg_spec(
          paste(
            "A string of R code. The first input is `data`; every input is",
            "also read by its link name, or as `.arg1`, `.arg2` when the",
            "link has none. Top-level assignments of plain values (literals,",
            "c() of literals, factor(), as.Date()) become UI controls; every",
            "other statement is code."
          ),
          example = paste(
            "species <- factor(\"setosa\", levels(data$Species))",
            "n <- 10",
            "",
            "data |>",
            "  dplyr::filter(Species == species) |>",
            "  dplyr::slice_head(n = n)",
            sep = "\n"
          ),
          type = blockr.core::arg_string()
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
  blockr.core::register_blocks(
    "new_prose_block",
    name = "Prose",
    description = paste(
      "Text that can compute: markdown edited as it reads, with inline R as",
      "`r expr` (Quarto's form). Values show in the text, marked with a",
      "dotted underline."
    ),
    category = "transform",
    icon = "card-text",
    guidance = paste(
      "Write `text` as GFM markdown. Compute values with inline R the way",
      "Quarto writes it: `r nrow(data)`, `r round(mean(data$mpg), 1)`.",
      "\n\nInputs are named as in the script block: the first input is",
      "`data`, and every input is also read by its own name: the link name,",
      "or `.arg1`, `.arg2`, ... for links without one. A bare column name",
      "does NOT resolve. Braces and other text need no escaping."
    ),
    arguments = list(
      blockr.core::new_arg_specs(
        text = blockr.core::new_arg_spec(
          paste(
            "Markdown string for the text. Inline R as `r expr` is evaluated",
            "with the inputs in scope: the first input is `data`, and every",
            "input also goes by its link name, or `.arg1`, `.arg2` when the",
            "link has none."
          ),
          example = "## Summary\n\nThe dataset has **`r nrow(data)`** rows.",
          type = blockr.core::arg_string()
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
  invisible(NULL)
}
