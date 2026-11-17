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
  ".cards <- paste0(",
  "  '<div class=\"card', ifelse(.sp$low, ' low', ''), '\">',",
  "  '<div class=\"k\">', .sp$Species, ' (', .sp$n, ')</div>',",
  "  '<div class=\"v\">', .sp$mean, '</div>',",
  "  '<div class=\"bar\"><i style=\"width:', .sp$pct, '%\"></i></div>',",
  "  '</div>',",
  "  collapse = '\\n'",
  ")",
  "",
  "blockr.script::html_page(paste0('",
  "<style>",
  "  .cards { display: grid; grid-template-columns: repeat(auto-fill, minmax(140px, 1fr)); gap: 8px; }",
  "  .card { border: 1px solid #e5e7eb; border-radius: 8px; padding: 10px; }",
  "  .card.low { border-color: #f59e0b; background: #fffbeb; }",
  "  .k { font-size: 11px; color: #6b7280; } .v { font-size: 22px; font-weight: 600; }",
  "  .bar { height: 4px; background: #e5e7eb; border-radius: 2px; margin-top: 6px; }",
  "  .bar i { display: block; height: 100%; background: #2563eb; border-radius: 2px; }",
  "</style>",
  "<div class=\"cards\">', .cards, '</div>'))",
  sep = "\n"
)

url <- paste(
  "dataset <- factor(\"ch_kof_barometer\", c(",
  "  \"ch_kof_barometer\", \"ch_fso_cpi\", \"ch_seco_gdp\",",
  "  \"ch_snb_amarbma\", \"ch_fso_retail\"",
  "))",
  "",
  "blockr.script::html_page(",
  "  url = paste0(\"https://dataseries.org/d/\", dataset, \"?embed=1\"),",
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
