#' Scripts "Start from" offers
#'
#' Each starter is a whole script that runs as inserted, with or without an
#' input. Picking one replaces the script in the editor; undo brings the old
#' one back. They are examples to edit, not modes: a script started from
#' "HTML page" is an ordinary script that happens to end in `html_page()`.
#'
#' @return A list of starters, each with `label`, `hint` and `script`.
#' @noRd
sb_starters <- function() {
  list(
    list(
      label = "Table",
      hint = "first rows of the input",
      script = paste(
        "n <- 10",
        "",
        "utils::head(data, n)",
        sep = "\n"
      )
    ),
    list(
      label = "Chart",
      hint = "ggplot2 scatter",
      # A select's pick reaches the code as a string. Where the dropdowns
      # cannot be built (an input with one column), `x` and `y` stay the
      # factors of these lines, hence as.character().
      script = paste(
        "# Without an input the chart shows mtcars.",
        ".df <- if (is.data.frame(data)) data else datasets::mtcars",
        "x <- factor(names(.df)[1], names(.df))",
        "y <- factor(names(.df)[2], names(.df))",
        "",
        "ggplot2::ggplot(",
        "  .df,",
        "  ggplot2::aes(.data[[as.character(x)]], .data[[as.character(y)]])",
        ") +",
        "  ggplot2::geom_point()",
        sep = "\n"
      )
    ),
    list(
      label = "HTML page",
      hint = "template filled from the input",
      script = sb_starter_html
    ),
    list(
      label = "External page",
      hint = "a URL in a frame",
      script = paste(
        "# The site must allow framing. {name} is filled from the first row",
        "# of the input or from a named value below.",
        "q <- \"blockr\"",
        "",
        "blockr.script::html_page(",
        "  url = \"https://example.com/?q={q}\",",
        "  q = q",
        ")",
        sep = "\n"
      )
    )
  )
}

sb_starter_html <- paste(
  "title <- \"Rows\"",
  "n <- 5",
  "",
  ".rows <- if (is.null(data)) NULL else utils::head(as.data.frame(data), n)",
  "",
  "blockr.script::html_page(",
  "  html = '",
  "<style>",
  "  table { border-collapse: collapse; font-size: 13px; }",
  "  th, td { padding: 4px 10px; border-bottom: 1px solid #e5e7eb; text-align: left; }",
  "  th { color: #6b7280; font-weight: 500; }",
  "</style>",
  "<h3>{{title}}: {{nrow}} of {{total}}</h3>",
  "<table id=\"tbl\"></table>",
  "<script>",
  "  // {{ }} above is filled in R. blockr.data holds the rows as JSON.",
  "  var rows = blockr.data || [];",
  "  if (rows.length) {",
  "    var keys = Object.keys(rows[0]);",
  "    var t = document.getElementById(\"tbl\");",
  "    var head = t.insertRow();",
  "    keys.forEach(function (k) {",
  "      var th = document.createElement(\"th\"); th.textContent = k; head.appendChild(th);",
  "    });",
  "    rows.forEach(function (r) {",
  "      var tr = t.insertRow();",
  "      keys.forEach(function (k) { tr.insertCell().textContent = r[k]; });",
  "    });",
  "  }",
  "</script>',",
  "  data = .rows,",
  "  title = title,",
  "  total = if (is.null(data)) 0L else NROW(data)",
  ")",
  sep = "\n"
)
