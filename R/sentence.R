# The sentence ----------------------------------------------------------------
#
# A script may carry `#| title:`, `#| subtitle:` and `#| caption:` lines, the
# title convention blockr.viz's blocks use:
#
#   arm <- factor("Placebo", levels = unique(data$TRT01A))
#   top_n <- 8  #| number(min = 5, max = 10)
#   #| title: Top {@top_n} adverse events in {@arm}
#
# `{@arg}` prints a control's current value and makes the word clickable; a
# click opens that control under the word. `[ ... ]` is a clause that leaves
# when a token inside it is empty, and the control it named is then offered
# as a `+ Label` chip at the end of its line. `{n}`, `{label(col)}`, `{col}`
# and `{filters}` read the block's first input, `data`.
#
# The sentence sits in the row with the gear, above the grid. A control the
# sentence names leaves the grid, so there is one live copy of each setting.
# Select, multi-select, number, text and checkbox can be words; a date stays
# in the grid and prints as plain text.
#
# The sentence is drawn on the face only. It does not reach downloads or
# reports yet.


# --- the template walk, copied from blockr.viz ---------------------------------
#
# Copied from blockr.viz R/title-template.R (0.2.223, c79f5fb), where these
# are internal. The chart block and this block parse the same grammar; the
# plan is one exported copy in blockr.ui. Keep the two in step until then.
# title_column_label() leaves out the picker block's `blockr_source`
# fallback, which only the chart block needs.

title_column_label <- function(data, col) {
  if (!is.data.frame(data) || !col %in% names(data)) {
    return("")
  }
  direct <- attr(data[[col]], "label", exact = TRUE)
  if (is.character(direct) && length(direct) == 1L &&
      !is.na(direct) && nzchar(direct)) {
    return(direct)
  }
  col
}

# How many distinct values a `{col}` token spells out before eliding.
TITLE_MAX_VALUES <- 8L

# `\n` written in a template is a line break: a `#|` line is one line.
tt_line_breaks <- function(template) {
  if (is.na(template)) {
    return(template)
  }
  gsub("\\n", "\n", template, fixed = TRUE)
}

# Split a template into segments, each `list(text =, optional =)`. An
# unmatched "[" is literal text: a template is display copy and must not error.
split_template_segments <- function(template) {
  out <- list()
  buf <- ""
  i <- 1L
  n <- nchar(template)
  while (i <= n) {
    ch <- substr(template, i, i)
    if (identical(ch, "[")) {
      close <- regexpr("]", substr(template, i, n), fixed = TRUE)
      if (close > 0L) {
        if (nzchar(buf)) {
          out <- c(out, list(list(text = buf, optional = FALSE)))
        }
        buf <- ""
        out <- c(out, list(list(
          text = substr(template, i + 1L, i + close - 2L),
          optional = TRUE
        )))
        i <- i + close
        next
      }
    }
    buf <- paste0(buf, ch)
    i <- i + 1L
  }
  if (nzchar(buf)) {
    out <- c(out, list(list(text = buf, optional = FALSE)))
  }
  out
}

# The pieces of one segment: plain text, and one piece per {token}. `arg` is
# set on the pieces a reader can click; `empty` marks a token that resolved to
# nothing, which is what makes an optional segment disappear.
resolve_template_chunk <- function(text, data, args) {
  starts <- gregexpr("\\{[^{}]+\\}", text)[[1L]]
  if (identical(starts[[1L]], -1L)) {
    return(list(list(text = text, arg = NULL, empty = FALSE)))
  }
  lens <- attr(starts, "match.length")
  out <- list()
  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[[i]] > pos) {
      out <- c(out, list(list(
        text = substr(text, pos, starts[[i]] - 1L), arg = NULL, empty = FALSE
      )))
    }
    tok <- trimws(substr(text, starts[[i]] + 1L, starts[[i]] + lens[[i]] - 2L))
    res <- resolve_title_token(tok, data, args)
    out <- c(out, list(list(
      text = res$text, arg = res$arg, by = res$by, empty = !nzchar(res$text)
    )))
    pos <- starts[[i]] + lens[[i]]
  }
  if (pos <= nchar(text)) {
    out <- c(out, list(list(
      text = substr(text, pos, nchar(text)), arg = NULL, empty = FALSE
    )))
  }
  out
}

# The pieces of a whole template, with the arguments whose clause dropped as
# the `offers` attribute.
title_template_parts <- function(template, data, args = list()) {
  template <- tt_line_breaks(as.character(template)[[1L]])
  if (!length(template) || is.na(template) || !nzchar(template)) {
    return(list())
  }
  out <- list()
  offers <- character()
  for (seg in split_template_segments(template)) {
    parts <- resolve_template_chunk(seg$text, data, args)
    if (isTRUE(seg$optional) &&
        any(vapply(parts, function(p) isTRUE(p$empty), logical(1L)))) {
      dropped <- vapply(
        parts,
        function(p) if (is.null(p$arg)) NA_character_ else p$arg,
        character(1L)
      )
      offers <- c(offers, dropped[!is.na(dropped)])
      next
    }
    out <- c(out, parts)
  }
  # Adjacent plain pieces merge, so the face renders one text node per run.
  merged <- list()
  for (p in out) {
    last <- if (length(merged)) merged[[length(merged)]] else NULL
    if (!is.null(last) && is.null(last$arg) && is.null(p$arg)) {
      merged[[length(merged)]]$text <- paste0(last$text, p$text)
    } else {
      merged <- c(merged, list(p))
    }
  }
  # An argument named twice, once in a clause that stayed, is not on offer.
  kept <- vapply(
    merged, function(p) if (is.null(p$arg)) NA_character_ else p$arg,
    character(1L)
  )
  offers <- setdiff(unique(offers), kept[!is.na(kept)])
  attr(merged, "offers") <- offers
  merged
}

# Returns `list(text =, arg =, by =)`: the resolved string, and the argument
# whose control the word opens when it is one.
resolve_title_token <- function(token, data, args = list()) {
  plain <- function(x) list(text = x, arg = NULL)
  if (!is.data.frame(data)) {
    data <- data.frame()
  }

  m <- regmatches(token, regexec("^@\\s*([^()@]+?)\\s*$", token))[[1L]]
  if (length(m) == 2L) {
    nm <- slot_arg_name(m[[2L]])
    return(list(text = arg_token_value(nm, args), arg = nm, by = "name"))
  }
  m <- regmatches(
    token,
    regexec("^([A-Za-z_.][A-Za-z0-9_.]*)\\(\\s*@\\s*([^()@]+?)\\s*\\)$", token)
  )[[1L]]
  if (length(m) == 3L) {
    m[[3L]] <- slot_arg_name(m[[3L]])
    col <- arg_token_value(m[[3L]], args)
    if (!nzchar(col)) {
      if (identical(m[[2L]], "n_distinct")) {
        return(plain(""))
      }
      return(list(text = "", arg = m[[3L]],
                  by = if (identical(m[[2L]], "label")) "label" else "name"))
    }
    inner <- resolve_title_token(paste0(m[[2L]], "(", col, ")"), data, args)
    if (identical(m[[2L]], "n_distinct")) {
      return(plain(inner$text))
    }
    return(list(text = inner$text, arg = m[[3L]],
                by = if (identical(m[[2L]], "label")) "label" else "name"))
  }

  if (identical(token, "n")) {
    return(plain(format(nrow(data), big.mark = "")))
  }

  if (identical(token, "filters")) {
    trail <- attr(data, "blockr_filters", exact = TRUE)
    trail <- trail[!is.na(trail) & nzchar(trail)]
    if (!length(trail)) {
      return(plain(""))
    }
    return(plain(paste(unname(trail), collapse = "; ")))
  }

  fn_arg <- function(fn) {
    m <- regmatches(
      token,
      regexec(paste0("^", fn, "\\(\\s*([^()]+?)\\s*\\)$"), token)
    )[[1L]]
    if (length(m) == 2L) m[[2L]] else NULL
  }

  col <- fn_arg("label")
  if (!is.null(col)) {
    if (!col %in% names(data)) {
      return(plain(""))
    }
    return(plain(title_column_label(data, col)))
  }

  col <- fn_arg("n_distinct")
  if (!is.null(col)) {
    if (!col %in% names(data)) {
      return(plain(""))
    }
    v <- data[[col]]
    return(plain(format(length(unique(v[!is.na(v)])), big.mark = "")))
  }

  if (!token %in% names(data)) {
    return(plain(""))
  }
  v <- data[[token]]
  if (is.factor(v)) {
    vals <- levels(v)[levels(v) %in% unique(as.character(v[!is.na(v)]))]
  } else {
    vals <- unique(as.character(v[!is.na(v)]))
  }
  if (!length(vals)) {
    return(plain(""))
  }
  if (length(vals) > TITLE_MAX_VALUES) {
    vals <- c(vals[seq_len(TITLE_MAX_VALUES)], "\u2026")
  }
  plain(paste(vals, collapse = ", "))
}

# A slot may name its choices: `{@arm in Placebo, Active}`. The list only
# narrows what the word's menu offers; the token reads the bare name.
SLOT_CHOICES_RE <- "^([^[:space:]]+)[[:space:]]+in[[:space:]]+(.*)$"

slot_arg_name <- function(x) {
  x <- trimws(x)
  m <- regmatches(x, regexec(SLOT_CHOICES_RE, x))[[1L]]
  if (length(m) == 3L) m[[2L]] else x
}

arg_token_value <- function(name, args) {
  v <- args[[name]]
  if (is.null(v) || !length(v)) {
    return("")
  }
  v <- as.character(v)[[1L]]
  if (is.na(v) || identical(v, "(none)")) {
    return("")
  }
  v
}


# --- the script's side ------------------------------------------------------

SB_TIERS <- c("title", "subtitle", "caption")

# How many values a multi-select spells out before eliding.
SB_SLOT_MAX_VALUES <- 8L

sb_or <- function(x, y) if (is.null(x)) y else x

#' The templates a script declares, read off its `#|` lines
#'
#' A line that is absent is `NULL`; a line with nothing after the colon is
#' `""`. The last of repeated lines wins. `ui_title`, `ui_subtitle` and
#' `ui_caption` are read too, so a script written with the older `ui_*`
#' names keeps its sentence here.
#'
#' @param lines The script's `#|` comments that stand on a line of their
#'   own (`comments` in the result of `cb_parse()`), or source lines.
#' @noRd
sb_sentence_templates <- function(lines) {
  out <- list(
    title = NULL, subtitle = NULL, caption = NULL,
    ui_title = NULL, ui_subtitle = NULL, ui_caption = NULL
  )
  if (!length(lines)) {
    return(out)
  }
  m <- regmatches(
    lines,
    regexec(
      "^\\s*#\\|\\s*((?:ui_)?(?:title|subtitle|caption))\\s*:\\s*(.*?)\\s*$",
      lines,
      perl = TRUE
    )
  )
  for (x in m) {
    if (length(x) == 3L) {
      out[[x[[2L]]]] <- x[[3L]]
    }
  }
  # The face shows the ui_ variants: title and caption replace, a subtitle
  # line is added to.
  face <- out[SB_TIERS]
  if (!is.null(out$ui_title)) {
    face$title <- out$ui_title
  }
  if (!is.null(out$ui_caption)) {
    face$caption <- out$ui_caption
  }
  if (!is.null(out$ui_subtitle)) {
    face$subtitle <- if (is.null(face$subtitle) || !nzchar(face$subtitle)) {
      out$ui_subtitle
    } else {
      paste(face$subtitle, out$ui_subtitle)
    }
  }
  face
}

#' Does the script declare a sentence at all?
#' @noRd
sb_has_sentence <- function(templates) {
  any(vapply(
    templates[SB_TIERS],
    function(x) is.character(x) && length(x) && nzchar(x),
    logical(1L)
  ))
}

#' Every name a template points at with `@`, whether or not its clause shows
#' @noRd
sb_sentence_arg_names <- function(templates) {
  tpl <- unlist(templates[SB_TIERS], use.names = FALSE)
  tpl <- tpl[!is.na(tpl) & nzchar(tpl)]
  if (!length(tpl)) {
    return(character())
  }
  hits <- regmatches(tpl, gregexpr("@\\s*[A-Za-z_.][A-Za-z0-9_.]*", tpl))
  unique(trimws(sub("^@\\s*", "", unlist(hits))))
}

#' What kind of word a control becomes, or `"none"`
#' @noRd
sb_slot_kind <- function(s) {
  if (identical(s$kind, "flag")) {
    return("flag")
  }
  if (identical(s$kind, "select")) {
    return(if (isTRUE(s$multiple)) "multi" else "select")
  }
  if (identical(s$kind, "number")) {
    return("number")
  }
  if (identical(s$kind, "text")) {
    return("text")
  }
  "none"
}

#' The controls the sentence takes over
#'
#' The one place that decides whether a word is live: named in a template,
#' of a kind a word can carry, and, for a select, with something to pick.
#' A `{@x in a, b}` list that matches none of the control's choices leaves
#' nothing to pick, so that word is plain text. What is decided here leaves
#' the grid; everything else stays in it, so a control is never lost.
#'
#' @noRd
sb_slot_arg_names <- function(specs, templates) {
  named <- sb_sentence_arg_names(templates)
  listed <- sb_template_slot_choices(templates)
  out <- character()
  for (s in specs) {
    if (!cb_spec_ok(s)) {
      next
    }
    if (!s$name %in% named) {
      next
    }
    kind <- sb_slot_kind(s)
    if (identical(kind, "none")) {
      next
    }
    if (kind %in% c("select", "multi") &&
        !length(sb_narrow_choices(s$choices, listed[[s$name]]))) {
      next
    }
    out <- c(out, s$name)
  }
  out
}

#' The value each control prints in the sentence
#'
#' An untouched control reads the script's own value. A checkbox reads as its
#' label when on and as nothing when off, so a clause can carry it. A
#' multi-select reads as its values joined with ", ", elided past 8.
#'
#' @noRd
sb_title_args <- function(specs, values = list()) {
  vals <- values
  for (s in specs) {
    if (!cb_spec_ok(s)) {
      next
    }
    v <- cb_value(s, values)
    if (identical(s$kind, "flag")) {
      v <- if (isTRUE(as.logical(v)[[1L]])) cb_label(s) else ""
    } else if (identical(s$kind, "select") && isTRUE(s$multiple)) {
      v <- as.character(v)
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) > SB_SLOT_MAX_VALUES) {
        v <- c(v[seq_len(SB_SLOT_MAX_VALUES)], "\u2026")
      }
      v <- paste(v, collapse = ", ")
    } else if (identical(s$kind, "date")) {
      v <- format(as.Date(v))
    } else if (is.numeric(v)) {
      v <- format(v, scientific = FALSE, trim = TRUE)
    }
    vals[[s$name]] <- v
  }
  vals
}

# A slot with a list, `{@arg in a, b}`: the name, then the choices.
SB_SLOT_LIST_RE <-
  "\\{\\s*@\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s+in\\s+([^{}]+)\\}"

#' The `{@arg in a, b}` lists in the templates, per argument
#' @noRd
sb_template_slot_choices <- function(templates) {
  tpl <- unlist(templates[SB_TIERS], use.names = FALSE)
  tpl <- tpl[!is.na(tpl) & nzchar(tpl)]
  out <- list()
  pattern <- SB_SLOT_LIST_RE
  for (x in tpl) {
    texts <- regmatches(x, gregexpr(pattern, x, perl = TRUE))[[1L]]
    for (text in texts) {
      m <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1L]]
      if (length(m) != 3L) {
        next
      }
      choices <- trimws(strsplit(m[[3L]], ",", fixed = TRUE)[[1L]])
      out[[m[[2L]]]] <- unique(c(out[[m[[2L]]]], choices[nzchar(choices)]))
    }
  }
  out
}

#' A listed slot keeps only the listed choices, in the order written
#'
#' The current value stays, so a saved board still finds its pick.
#'
#' @noRd
sb_narrow_choices <- function(choices, listed, current = NULL) {
  if (!length(listed)) {
    return(choices)
  }
  choices <- as.character(choices)
  keep <- listed[listed %in% choices]
  c(keep, setdiff(intersect(as.character(current), choices), keep))
}

#' A select's options, with the column label where a choice names a column
#' @noRd
sb_slot_options <- function(choices, data = NULL) {
  lapply(as.character(choices), function(ch) {
    lbl <- if (is.data.frame(data) && ch %in% names(data)) {
      attr(data[[ch]], "label", exact = TRUE)
    }
    if (is.character(lbl) && length(lbl) && nzchar(lbl[[1L]]) &&
        !identical(lbl[[1L]], ch)) {
      list(value = ch, label = lbl[[1L]])
    } else {
      ch
    }
  })
}

#' What each word opens, keyed by control name
#'
#' Ships to the browser as JSON on the band, so a menu opens without a round
#' trip.
#'
#' @noRd
sb_slot_specs <- function(specs, values, slot_args, data = NULL,
                          listed = list()) {
  out <- list()
  for (s in specs) {
    if (!s$name %in% slot_args) {
      next
    }
    v <- cb_value(s, values)
    label <- cb_label(s)
    kind <- sb_slot_kind(s)
    if (identical(kind, "flag")) {
      out[[s$name]] <- list(
        kind = "flag", value = isTRUE(as.logical(v)[[1L]]), label = label
      )
    } else if (kind %in% c("select", "multi")) {
      choices <- sb_narrow_choices(s$choices, listed[[s$name]], v)
      sel <- as.character(v)
      sel <- sel[!is.na(sel) & nzchar(sel)]
      out[[s$name]] <- list(
        kind = kind,
        options = sb_slot_options(choices, data),
        # as.list(): one pick must ship as a one-element array.
        selected = if (identical(kind, "multi")) as.list(sel) else
          if (length(sel)) sel[[1L]] else "",
        label = label
      )
    } else if (identical(kind, "number")) {
      rec <- list(
        kind = "number",
        value = suppressWarnings(as.numeric(v)[[1L]]),
        label = label
      )
      # Absent limits are left out, not sent as null: the field would read
      # "null" as its limit.
      for (k in c("min", "max", "step")) {
        if (!is.null(s[[k]]) && !is.na(s[[k]])) {
          rec[[k]] <- suppressWarnings(as.numeric(s[[k]])[[1L]])
        }
      }
      out[[s$name]] <- rec
    } else if (identical(kind, "text")) {
      rec <- list(kind = "text", value = as.character(v)[[1L]], label = label)
      if (!is.null(s$placeholder) && nzchar(s$placeholder)) {
        rec$placeholder <- as.character(s$placeholder)[[1L]]
      }
      out[[s$name]] <- rec
    }
  }
  out
}

#' A pick made on a word, written the way the grid would have written it
#' @noRd
sb_apply_slot <- function(msg, specs, values) {
  arg <- as.character(sb_or(msg$arg, ""))[[1L]]
  spec <- Find(function(s) identical(s$name, arg), specs)
  if (is.null(spec) || !cb_spec_ok(spec)) {
    return(values)
  }
  v <- if (identical(spec$kind, "flag")) {
    isTRUE(as.logical(unlist(msg$value, use.names = FALSE))[[1L]])
  } else {
    cb_coerce(msg$value, spec)
  }
  if (is.null(v)) {
    return(values)
  }
  if (identical(spec$kind, "number")) {
    if (!is.null(spec$min) && !is.na(spec$min)) v <- max(v, spec$min)
    if (!is.null(spec$max) && !is.na(spec$max)) v <- min(v, spec$max)
  }
  values[[arg]] <- v
  values
}

#' The band: title, subtitle and caption with the words that are controls
#'
#' `NULL` when the script declares no sentence. The band carries what the
#' click handler needs: the slot specs (`data-sb-slots`) and the Shiny input a
#' pick goes to (`data-sb-input`).
#'
#' A word gets the slot mark only if `sb_slot_arg_names()` made it live, and
#' that function also takes it out of the grid. A word the sentence names but
#' that cannot open anything is plain text, and its control stays in the
#' grid.
#'
#' @param templates From `sb_sentence_templates()`.
#' @param specs,values The block's control specs and current values.
#' @param data The block's first input, for the data tokens.
#' @param slot_input The namespaced input id picks are sent to.
#' @noRd
sb_sentence_ui <- function(templates, specs, values, data, slot_input) {
  if (!sb_has_sentence(templates)) {
    return(NULL)
  }
  if (!is.data.frame(data)) {
    data <- data.frame()
  }
  slot_args <- sb_slot_arg_names(specs, templates)
  listed <- sb_template_slot_choices(templates)
  pattern <- SB_SLOT_LIST_RE
  args <- sb_title_args(specs, values)
  slots <- sb_slot_specs(specs, values, slot_args, data, listed)

  paint <- function(cls, template) {
    if (is.null(template) || !nzchar(template)) {
      return(NULL)
    }
    template <- gsub(pattern, "{@\\1}", template, perl = TRUE)
    parts <- title_template_parts(template, data, args)
    offers <- intersect(attr(parts, "offers", exact = TRUE), names(slots))
    kids <- list()
    for (p in parts) {
      if (is.null(p$text) || !nzchar(p$text)) {
        next
      }
      if (is.null(p$arg) || !p$arg %in% names(slots)) {
        kids <- c(kids, list(p$text))
        next
      }
      # .noWS: htmltools puts each child on its own line, and the newline
      # before a closing word renders as a space.
      kids <- c(kids, list(htmltools::tags$span(
        class = "blockr-slot",
        role = "button", tabindex = "0",
        `data-slot-arg` = p$arg,
        `data-slot-by` = sb_or(p$by, "name"),
        `data-slot-kind` = slots[[p$arg]]$kind,
        title = paste("Change", slots[[p$arg]]$label),
        p$text,
        .noWS = "outside"
      )))
    }
    for (key in offers) {
      kids <- c(kids, list(htmltools::tags$span(
        class = "blockr-slot-offer",
        role = "button", tabindex = "0",
        `data-slot-arg` = key,
        `data-slot-kind` = slots[[key]]$kind,
        title = paste("Set", tolower(slots[[key]]$label)),
        paste0("+ ", slots[[key]]$label),
        .noWS = "outside"
      )))
    }
    if (!length(kids)) {
      return(NULL)
    }
    # Runs of plain text become one node, for the same reason as .noWS.
    merged <- list()
    for (k in kids) {
      last <- if (length(merged)) merged[[length(merged)]] else NULL
      if (is.character(k) && is.character(last)) {
        merged[[length(merged)]] <- paste0(last, k)
      } else {
        merged <- c(merged, list(k))
      }
    }
    do.call(htmltools::tags$div, c(list(class = cls, .noWS = "inside"), merged))
  }

  title <- paint("blockr-script-sentence__title", templates$title)
  subtitle <- paint("blockr-script-sentence__subtitle", templates$subtitle)
  caption <- paint("blockr-script-sentence__caption", templates$caption)
  if (is.null(title) && is.null(subtitle) && is.null(caption)) {
    return(NULL)
  }
  htmltools::tags$div(
    class = "blockr-script-sentence",
    `data-sb-slots` = as.character(jsonlite::toJSON(
      slots, auto_unbox = TRUE, null = "null", digits = NA
    )),
    `data-sb-input` = slot_input,
    title, subtitle, caption
  )
}
