# The script block on a board: what its inputs are called when links come
# and go, and what the exported code reads.

link_board <- function(script, links) {
  blockr.core::new_board(
    blocks = c(
      flowers = blockr.core::new_static_block(iris[1:3, ]),
      cars = blockr.core::new_static_block(mtcars[1:5, ]),
      s = new_script_block(script)
    ),
    links = links
  )
}

exported_line <- function(rv) {
  exprs <- lapply(rv$blocks, function(b) b$server$expr())
  code <- blockr.core:::export_wrapped_code(exprs, rv$board)
  grep("^s <- ", strsplit(code, "\n")[[1L]], value = TRUE)
}

# The links are named differently from the blocks they come from, so an
# input name left in the exported code shows.
two_links <- function() {
  blockr.core::links(
    f = blockr.core::new_link("flowers", "s", "fl"),
    c = blockr.core::new_link("cars", "s", "cr")
  )
}

test_that("an input read by its own name survives the links being redrawn", {
  board <- link_board("c(fl = ncol(fl), cr = ncol(cr))", two_links())
  shiny::testServer(
    blockr.core::get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_identical(rv$blocks$s$server$result(), c(fl = 5L, cr = 11L))
      expect_identical(
        exported_line(rv),
        "s <- local(c(fl = ncol(flowers), cr = ncol(cars)))"
      )

      # remove the first link and draw it again: it is now the second
      board_update(list(links = list(rm = "f")))
      session$flushReact()
      board_update(list(links = list(
        add = blockr.core::links(
          f = blockr.core::new_link("flowers", "s", "fl")
        )
      )))
      session$flushReact()
      expect_identical(rv$blocks$s$server$result(), c(fl = 5L, cr = 11L))
      expect_identical(
        exported_line(rv),
        "s <- local(c(fl = ncol(flowers), cr = ncol(cars)))"
      )
    },
    args = list(x = board)
  )
})

test_that("data follows the first link, as documented", {
  board <- link_board("c(data = ncol(data), cr = ncol(cr))", two_links())
  shiny::testServer(
    blockr.core::get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_identical(rv$blocks$s$server$result(), c(data = 5L, cr = 11L))

      board_update(list(links = list(rm = "f")))
      session$flushReact()
      # `cr` is first now. It is `data`, and it is still `cr`: the export
      # names the upstream block for both.
      expect_identical(rv$blocks$s$server$result(), c(data = 11L, cr = 11L))
      expect_identical(
        exported_line(rv),
        "s <- local(c(data = ncol(cars), cr = ncol(cars)))"
      )
    },
    args = list(x = board)
  )
})

test_that("an unnamed first input exports under .arg1 and under data", {
  board <- link_board(
    "nrow(.arg1) + nrow(data)",
    blockr.core::links(blockr.core::new_link("flowers", "s"))
  )
  shiny::testServer(
    blockr.core::get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_identical(rv$blocks$s$server$result(), 6L)
      expect_identical(
        exported_line(rv),
        "s <- local(nrow(flowers) + nrow(flowers))"
      )
    },
    args = list(x = board)
  )
})

test_that("a script that assigns data runs as plain R would, on a board", {
  board <- link_board(
    "data <- utils::head(data, 2)\nnrow(data)",
    blockr.core::links(blockr.core::new_link("flowers", "s"))
  )
  shiny::testServer(
    blockr.core::get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_identical(rv$blocks$s$server$result(), 2L)
    },
    args = list(x = board)
  )
})

test_that("a block whose script cannot be used shows the reason", {
  suppressWarnings(board <- link_board(
    "n <- 6",
    blockr.core::links(blockr.core::new_link("flowers", "s"))
  ))
  shiny::testServer(
    blockr.core::get_s3_method("board_server", board),
    {
      session$flushReact()
      expect_null(rv$blocks$s$server$result())
      said <- paste(unlist(rv$blocks$s$server$conditions()), collapse = " ")
      expect_match(said, "has no result", fixed = TRUE)
    },
    args = list(x = board)
  )
})
