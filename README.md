# blockr.script

An R script block for [blockr](https://blockr-org.github.io/blockr.site/)
boards. The block's state is an ordinary R script. It reads any number of
upstream results and returns whatever its last statement evaluates to.

## Installation

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.script")
```

## Example

```r
library(blockr.core)
library(blockr.script)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      s = new_script_block(c(
        "species <- factor(\"setosa\", levels(data$Species))",
        "n <- 5",
        "",
        "data |>",
        "  subset(Species == species) |>",
        "  utils::head(n)"
      ))
    ),
    links = links(from = "a", to = "s", input = "data")
  )
)
```

- **Inputs.** The first link is `data`. Every link is also read by its own
  name: the link name, or `.arg1`, `.arg2`, ... for links without one (only
  the unnamed links are counted). Which link is first can change when links
  are removed and drawn again, so a script with several inputs reads each by
  its own name. A link named `data` is always `data`. With no link, `data`
  is `NULL` and the script is the source.
- **Controls.** A top-level assignment of a plain value becomes a control: a
  factor is a dropdown over its levels, a number a spin box, `TRUE`/`FALSE` a
  checkbox. A name starting with a dot stays private.
- **Export.** The exported code is the script with the current values written
  in and each input replaced by the id of its upstream block.

The script lives in the block's gear tray, for whoever builds the board. The
controls and the result are on the block's face, for whoever uses it.
