# Demo board for a result class of its own on the script block. Run from the
# package root:
#   Rscript dev/result-hooks-demo.R 4020
#
# It shows what another package would do: a block built on the script block
# (`new_tally_block()`), a result class (`tally`) and a
# `block_result_output()` method that puts a download button into the head
# row, gives the sentence a title and draws the caption under the table.
#
# The two listing blocks show a result that is updated in place. Both draw a
# long table with a search field. "Listing" has a `block_result_update()`
# method: scroll down, type into the search field, then change the sort
# column in the title, and both stay. "Listing, redrawn" has none: the same
# change builds the table again, at the top and with an empty search field.
# The table is hand-made, to keep the demo to this package. A widget with an
# update function of its own goes into the two methods the same way.
#
# The installed packages can lag the monorepo, so the local copies are
# loaded where they are found: next to this package, or under BLOCKR_ROOT.
# BLOCKR_UI names a checkout of blockr.ui that is somewhere else. blockr.dag
# is the installed one, which is its main.
root <- Sys.getenv("BLOCKR_ROOT", "..")
use_pkg <- function(pkg, path = file.path(root, pkg)) {
  if (dir.exists(path)) {
    pkgload::load_all(path, quiet = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}
use_pkg("blockr.core")
use_pkg("blockr.ui", Sys.getenv("BLOCKR_UI", file.path(root, "blockr.ui")))
use_pkg("blockr.dock")
library(blockr.dag)
pkgload::load_all(".", quiet = TRUE)
options(blockr.dock_is_locked = FALSE)
args <- commandArgs(TRUE)
port <- if (length(args)) as.integer(args[1]) else 4020L

# --- the result class ---------------------------------------------------------

# Rows per group. Scripts call it by name: a script is evaluated with the
# global environment behind it.
tally <- function(data, by) {
  counts <- as.data.frame(table(data[[by]]), stringsAsFactors = FALSE)
  names(counts) <- c(by, "rows")
  structure(list(table = counts, by = by), class = "tally")
}

download_icon <- htmltools::HTML(paste0(
  '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" ',
  'stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round">',
  '<path d="M8 2v8M4.5 7 8 10.5 11.5 7M3 13.5h10"/></svg>'
))

block_result_output.tally <- function(result, block, session, ...) {
  # Assigned on every call. That replaces the handler and stacks nothing.
  session$output$csv <- shiny::downloadHandler(
    filename = "tally.csv",
    content = function(file) {
      # Read here, so the file has the title as it is now. `ui = FALSE`
      # leaves out the lines a script writes for the app only.
      titles <- result_titles(session, ui = FALSE)
      writeLines(c(titles$title, titles$subtitle, ""), file)
      suppressWarnings(utils::write.table(
        result$table, file, append = TRUE, sep = ",", row.names = FALSE
      ))
    }
  )
  result_head(
    session,
    tools = shiny::downloadLink(
      session$ns("csv"), label = download_icon, title = "Download CSV",
      `aria-label` = "Download CSV"
    ),
    titles = list(
      title = paste("Rows by", result$by),
      caption = "Counted with table()."
    ),
    caption = FALSE
  )
  shiny::renderUI(shiny::tagList(
    shiny::tags$table(
      class = "table table-sm",
      shiny::tags$thead(shiny::tags$tr(
        lapply(names(result$table), shiny::tags$th)
      )),
      shiny::tags$tbody(lapply(seq_len(nrow(result$table)), function(i) {
        shiny::tags$tr(lapply(result$table[i, ], shiny::tags$td))
      }))
    ),
    # The caption follows the sentence without a new evaluation.
    shiny::tags$p(
      style = "color:#6b7280;font-size:12px;",
      result_titles(session)$caption
    )
  ))
}
registerS3method(
  "block_result_output", "tally", block_result_output.tally,
  envir = asNamespace("blockr.script")
)

# --- the block ----------------------------------------------------------------

tally_script <- paste(
  "by <- factor(\"Species\", names(data))",
  "",
  "tally(data, by)",
  sep = "\n"
)

tally_starters <- c(
  list(
    list(label = "Tally", hint = "the title comes from the result",
         script = tally_script),
    list(
      label = "Tally with a title",
      hint = "the script's title replaces it",
      script = paste(
        "by <- factor(\"Species\", names(data))",
        "note <- \"iris\"",
        "#| title: {n} flowers by {@by}",
        "#| caption: Source: {@note}",
        "",
        "tally(data, by)",
        sep = "\n"
      )
    ),
    list(label = "Plain table", hint = "no tools in the head row",
         script = "utils::head(data, 5)"),
    list(label = "Slow", hint = "two seconds, shows the busy line",
         script = "Sys.sleep(2)\ntally(data, \"Species\")"),
    list(label = "Slow and failing", hint = "the busy line clears",
         script = "Sys.sleep(2)\nstop(\"this script fails\")")
  ),
  # A long list: the menu scrolls.
  lapply(seq_len(25), function(i) {
    list(
      label = sprintf("First %d rows", 5 * i),
      hint = "tally of a part",
      script = sprintf("tally(utils::head(data, %d), \"Species\")", 5 * i)
    )
  })
)

new_tally_block <- function(script = tally_script, values = list(), ...) {
  new_script_block(
    script, values,
    starters = tally_starters, class = "tally_block", ...
  )
}

# A package registers by name with `package = utils::packageName()`. This
# constructor lives in the global environment, so it is passed itself.
blockr.core::register_block(
  new_tally_block,
  name = "Tally",
  description = "Rows per group, built on the script block.",
  category = "transform",
  icon = "list-ol",
  overwrite = TRUE
)

# --- a result that is updated in place ---------------------------------------

# The rows of `data`, sorted, with the columns asked for. `live = FALSE`
# gives a class that has no update method.
listing <- function(data, by, show = names(data), live = TRUE) {
  table <- data[order(data[[by]]), show, drop = FALSE]
  rownames(table) <- NULL
  structure(
    list(table = table),
    class = if (live) "listing" else "listing_redrawn"
  )
}

# The head and the rows of the table, as HTML.
listing_rows <- function(table) {
  cells <- vapply(table, function(col) {
    paste0("<td>", htmltools::htmlEscape(format(col)), "</td>")
  }, character(nrow(table)))
  dim(cells) <- dim(table)
  paste0(
    "<thead><tr>",
    paste0("<th>", htmltools::htmlEscape(names(table)), "</th>", collapse = ""),
    "</tr></thead><tbody>",
    paste0("<tr>", apply(cells, 1L, paste0, collapse = ""), "</tr>",
           collapse = ""),
    "</tbody>"
  )
}

# The browser side: the search field hides the rows that do not match, and
# new rows from R go into the table that is on screen.
listing_js <- htmltools::singleton(shiny::tags$head(shiny::tags$script(
  htmltools::HTML("
(function () {
  if (window.listingDemo) return;
  window.listingDemo = true;
  function filter(box) {
    var q = box.previousElementSibling.value.toLowerCase();
    box.querySelectorAll('tbody tr').forEach(function (tr) {
      tr.hidden = !!q && tr.textContent.toLowerCase().indexOf(q) < 0;
    });
  }
  document.addEventListener('input', function (e) {
    if (e.target.classList.contains('listing-search')) {
      filter(e.target.nextElementSibling);
    }
  });
  Shiny.addCustomMessageHandler('listing-rows', function (msg) {
    var box = document.getElementById(msg.id);
    if (!box) return;
    box.querySelector('table').innerHTML = msg.rows;
    filter(box);
  });
})();
")
)))

# The first render. The id is the same in every call, so the update finds
# the table.
block_result_output.listing <- function(result, block, session, ...) {
  id <- session$ns("listing")
  shiny::renderUI(shiny::tagList(
    listing_js,
    shiny::tags$input(
      type = "search", class = "listing-search form-control form-control-sm",
      placeholder = "Search", style = "max-width:240px;margin-bottom:8px;"
    ),
    shiny::tags$div(
      id = id, style = "max-height:320px;overflow:auto;",
      shiny::tags$table(
        class = "table table-sm", htmltools::HTML(listing_rows(result$table))
      )
    )
  ))
}

# Every later result of the class: the rows go to the table on screen.
block_result_update.listing <- function(result, block, session, ...) {
  session$sendCustomMessage(
    "listing-rows",
    list(id = session$ns("listing"), rows = listing_rows(result$table))
  )
  TRUE
}

for (cls in c("listing", "listing_redrawn")) {
  registerS3method(
    "block_result_output", cls, block_result_output.listing,
    envir = asNamespace("blockr.script")
  )
}
registerS3method(
  "block_result_update", "listing", block_result_update.listing,
  envir = asNamespace("blockr.script")
)

# `show` is a multi-select. Its choices follow `columns`, so changing
# `columns` redraws it with other choices, and a pick that is not among
# them (`mag`, say) is dropped. The live block names it in the
# subtitle, where it is a word. The other one does not, so there it is a
# control in the grid under the head row.
listing_script <- function(live) {
  paste(
    "by <- factor(\"mag\", names(data))",
    "columns <- factor(\"all\", c(\"all\", \"place\"))",
    "show <- factor(",
    "  c(\"lat\", \"long\"),",
    "  if (columns == \"all\") names(data) else c(\"lat\", \"long\", \"depth\")",
    ")",
    "#| title: {n} quakes by {@by}",
    if (live) {
      "#| subtitle: Showing {@show} of {@columns} columns"
    } else {
      "#| subtitle: Of {@columns} columns"
    },
    "",
    sprintf("listing(data, by, show, live = %s)", live),
    sep = "\n"
  )
}

board <- new_dock_board(
  blocks = c(
    iris = new_dataset_block("iris", block_name = "Iris"),
    tally = new_tally_block(block_name = "Tally"),
    quakes = new_dataset_block("quakes", block_name = "Quakes"),
    listing = new_script_block(
      listing_script(TRUE), block_name = "Listing"
    ),
    redrawn = new_script_block(
      listing_script(FALSE), block_name = "Listing, redrawn"
    )
  ),
  links = c(
    links(from = "iris", to = "tally"),
    links(from = "quakes", to = "listing"),
    links(from = "quakes", to = "redrawn")
  ),
  extensions = list(dag = new_dag_extension())
)

shiny::runApp(serve(board), port = port, launch.browser = FALSE)
