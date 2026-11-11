# Demo board for the HTML block. Run from the package root:
#   Rscript dev/html-block-demo.R 4010
# The installed blockr.dock can lag the monorepo; load the local copies.
# blockr.dag is the installed one, which is its main.
for (pkg in c("blockr.core", "blockr.ui", "blockr.dock")) {
  pkgload::load_all(file.path("..", pkg), quiet = TRUE)
}
library(blockr.dag)
pkgload::load_all(".", quiet = TRUE)
options(blockr.dock_is_locked = FALSE)
port <- as.integer(commandArgs(TRUE)[1] %||% 4010)

cards <- paste(
  "min_mean <- 3",
  "",
  ".sp <- data |>",
  "  dplyr::summarise(",
  "    n = dplyr::n(),",
  "    mean = round(mean(Sepal.Length), 2),",
  "    .by = Species",
  "  ) |>",
  "  dplyr::mutate(low = mean < min_mean, pct = round(100 * mean / 8))",
  "",
  "blockr.script::html_page(",
  "  html = '",
  "<style>",
  "  .cards { display: grid; grid-template-columns: repeat(auto-fill, minmax(140px, 1fr)); gap: 8px; }",
  "  .card { border: 1px solid #e5e7eb; border-radius: 8px; padding: 10px; }",
  "  .card.low { border-color: #f59e0b; background: #fffbeb; }",
  "  .k { font-size: 11px; color: #6b7280; } .v { font-size: 22px; font-weight: 600; }",
  "  .bar { height: 4px; background: #e5e7eb; border-radius: 2px; margin-top: 6px; }",
  "  .bar i { display: block; height: 100%; background: #2563eb; border-radius: 2px; }",
  "</style>",
  "<div class=\"cards\">",
  "{{#rows}}",
  "  <div class=\"card {{#low}}low{{/low}}\">",
  "    <div class=\"k\">{{Species}} ({{n}})</div>",
  "    <div class=\"v\">{{mean}}</div>",
  "    <div class=\"bar\"><i style=\"width:{{pct}}%\"></i></div>",
  "  </div>",
  "{{/rows}}",
  "</div>',",
  "  data = .sp",
  ")",
  sep = "\n"
)

url <- paste(
  "dataset <- factor(\"ch_kof_barometer\", c(",
  "  \"ch_kof_barometer\", \"ch_fso_cpi\", \"ch_seco_gdp\",",
  "  \"ch_snb_amarbma\", \"ch_fso_retail\"",
  "))",
  "",
  "blockr.script::html_page(",
  "  url = \"https://dataseries.org/d/{dataset}?embed=1\",",
  "  dataset = dataset,",
  "  height = \"680px\"",
  ")",
  sep = "\n"
)

board <- new_dock_board(
  blocks = c(
    iris = new_dataset_block("iris", block_name = "Iris"),
    page = new_script_block(sb_starter_html, block_name = "Rows as table"),
    cards = new_script_block(cards, block_name = "Species means"),
    web = new_script_block(url, block_name = "dataseries.org"),
    note = new_prose_block(
      "The table has **`r nrow(data)`** rows and `r ncol(data)` columns.",
      block_name = "Note"
    )
  ),
  # Unnamed links: the script reads the first one as `data`.
  links = links(
    from = c("iris", "iris", "iris"), to = c("page", "cards", "note")
  ),
  extensions = list(dag = new_dag_extension())
)

shiny::runApp(serve(board), port = port, launch.browser = FALSE)
