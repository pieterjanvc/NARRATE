#' Module UI for overlap filter controls
#'
#' UI has (top to bottom): a "group" multi-select (filters by `group_id`),
#' a "user" multi-select (filters by `user_id`), and a reset button that
#' restores both selects to "all selected". Choices for both selects are
#' populated/refreshed server-side from the distinct `group_id`/`user_id`
#' values present in the (unfiltered) `highlights` passed to the paired
#' [mod_overlap_server()] call, so this UI function only lays out empty
#' selects - it does not take the data as an argument.
#'
#' Must be paired with a [mod_overlap_ui_text()] call using the *same*
#' module `id`, and both wired up to a single [mod_overlap_server()] call
#' (also using that `id`).
#'
#' @param id Module ID
#' @param group_label (Default = "Group"). Label for the group filter select
#' @param user_label (Default = "User"). Label for the user filter select
#'
#' @import shiny
#'
#' @returns Shiny UI element
#' @export
#'
mod_overlap_ui_filters <- function(id, group_label = "Group", user_label = "User") {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("group"),
      group_label,
      choices = character(0),
      multiple = TRUE
    ),
    selectInput(
      ns("user"),
      user_label,
      choices = character(0),
      multiple = TRUE
    ),
    actionButton(ns("resetFilters"), "Reset filters")
  )
}

#' Module UI for the overlap text display
#'
#' Contains only a [uiOutput()] that the paired [mod_overlap_server()]
#' renders the source text into, with highlight `<span>`s injected around
#' every region covered by at least one (post-filter) highlight. Overlapping
#' coverage from multiple distinct users is shown as a single merged span
#' per atomic sub-interval, its background shaded on a scale from pale
#' yellow (one covering user) to green (all distinct users in the
#' *unfiltered* highlights); if the covering highlights span more than one
#' `group_id`, the span's text (not its background) renders in red instead
#' of the default text color, layering the conflict signal on top of the
#' user-count scale rather than replacing it.
#'
#' Hovering a span reveals a small table (user, group) of everyone covering
#' that segment. Showing/hiding it is pure CSS (a hidden table baked into
#' the span, revealed via `:hover`) and never round-trips through Shiny; a
#' small scoped script only *positions* the table (`position: fixed`,
#' tracking the cursor via `mousemove`) so it isn't clipped by a scrollable
#' ancestor - e.g. callers that wrap this UI in an `overflow-y: auto`
#' container, as `mod_highlight_ui_text()` typically is.
#'
#' Must be paired with a [mod_overlap_ui_filters()] call using the *same*
#' module `id`, and both wired up to a single [mod_overlap_server()] call
#' (also using that `id`).
#'
#' @param id Module ID
#'
#' @import shiny
#'
#' @returns Shiny UI element
#' @export
#'
mod_overlap_ui_text <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf(
      "
  #%1$s .overlap-hover-table {
    display: none; position: fixed; z-index: 1000;
    color: initial; font-weight: normal; font-style: normal;
  }
  #%1$s .overlap-span:hover .overlap-hover-table { display: block; }
  #%1$s .overlap-hover-table table { border-collapse: collapse; background: #fff; border: 1px solid #999; box-shadow: 0 1px 4px rgba(0,0,0,0.3); }
  #%1$s .overlap-hover-table th, #%1$s .overlap-hover-table td { padding: 2px 6px; border: 1px solid #ccc; font-size: 0.85em; text-align: left; }
  ",
      ns("textDisplay")
    ))),
    tags$script(HTML(sprintf(
      "
  (function() {
    var containerId = '%1$s';

    // `position: fixed` keeps the hover table from being clipped by a
    // scrollable ancestor, but its offset must then be computed relative
    // to the viewport (percentage-based CSS can't do that), hence this
    // small positioning-only listener - it never talks to Shiny. Positioned
    // from the cursor coordinates rather than the span's own
    // getBoundingClientRect(): a span that wraps across multiple lines
    // returns a bounding box spanning all of its line fragments, whose
    // left edge collapses to the container's left margin instead of
    // tracking where the mouse actually is.
    //
    // Listens on `document` (not the container) and resolves the container
    // lazily inside the handler, rather than looking it up once up front:
    // this script tag is emitted *before* the uiOutput() div it targets, so
    // at the time this script itself runs, that div doesn't exist in the
    // DOM yet - looking it up here instead, on the first real mousemove
    // (well after the page has finished loading), sidesteps that ordering
    // entirely.
    document.addEventListener('mousemove', function(e) {
      var container = document.getElementById(containerId);
      if (!container) return;
      var span = e.target.closest('.overlap-span');
      if (!span || !container.contains(span)) return;
      var table = span.querySelector('.overlap-hover-table');
      if (!table) return;
      table.style.left = (e.clientX + 12) + 'px';
      table.style.top = (e.clientY + 12) + 'px';
    });
  })();
  ",
      ns("textDisplay")
    ))),
    uiOutput(ns("textDisplay"))
  )
}

#' Internal function to HTML-escape a character vector
#'
#' Used to safely embed `group_names`/`user_names` display strings inside
#' the hover-table markup baked into overlap spans.
#'
#' @param x Character vector
#'
#' @returns Character vector with `&`, `<`, `>` escaped
#'
mod_overlap_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Internal function to resolve display names for ids
#'
#' @param ids Character vector of ids to resolve
#' @param names (Optional) A named vector, id -> display name. Ids with no
#' entry (or a `NULL`/empty `names`) fall back to the raw id
#'
#' @returns Character vector, same length/order as `ids`
#'
mod_overlap_display_name <- function(ids, names) {
  ids <- as.character(ids)
  if (is.null(names) || length(names) == 0) {
    return(ids)
  }
  resolved <- unname(names[ids])
  ifelse(is.na(resolved), ids, resolved)
}

#' Internal function to interpolate the overlap color scale
#'
#' Linearly interpolates, per RGB channel, between `#fff2cc` (1 covering
#' user) and `#b6d7a8` (`totalUsers` covering users).
#'
#' @param nUsers Number of distinct users covering a given segment
#' @param totalUsers Total distinct users in the unfiltered highlights input.
#' If `<= 1` the scale is vacuous (no real overlap is possible) and the
#' low end of the scale is returned
#'
#' @returns A `#rrggbb` color string
#'
mod_overlap_interpolate_color <- function(nUsers, totalUsers) {
  from <- c(255, 242, 204) # #fff2cc
  to <- c(182, 215, 168) # #b6d7a8

  frac <- if (totalUsers <= 1) {
    0
  } else {
    min(max((nUsers - 1) / (totalUsers - 1), 0), 1)
  }

  rgb <- round(from + (to - from) * frac)
  sprintf("#%02x%02x%02x", rgb[1], rgb[2], rgb[3])
}

#' Internal function to render a hover-table's HTML
#'
#' @param hoverRows Data frame with columns `user_name`, `group_name`, one
#' row per distinct covering (user, group) pair
#'
#' @returns Character string, a `<table>` (empty string if `hoverRows` has
#' no rows)
#'
mod_overlap_hover_table_html <- function(hoverRows) {
  if (nrow(hoverRows) == 0) {
    return("")
  }
  rowsHtml <- paste0(
    "<tr><td>",
    mod_overlap_html_escape(hoverRows$user_name),
    "</td><td>",
    mod_overlap_html_escape(hoverRows$group_name),
    "</td></tr>",
    collapse = ""
  )
  paste0(
    "<table><tr><th>User</th><th>Group</th></tr>",
    rowsHtml,
    "</table>"
  )
}

#' Internal function to compute merged overlap segments
#'
#' Sweep-line over a (already filtered, malformed-row-free) highlights data
#' frame: computes the breakpoints formed by every distinct `start`/`end`
#' value, then for each atomic gap between consecutive breakpoints
#' determines the covering (`user_id`, `group_id`) pairs (deduped, so a user
#' covering via multiple raw rows counts once), the resulting background
#' color (always [mod_overlap_interpolate_color()] on the user-count scale -
#' a `group_id` conflict does not affect it), a text color override (
#' `#ff0000` if the covering pairs span more than one `group_id`, `NA`
#' otherwise, meaning "leave the default text color alone"), and the
#' hover-table HTML.
#'
#' Because `start`/`end` are half-open and `lo`/`hi` come from consecutive
#' breakpoints, a row either fully contains `[lo, hi)` or is disjoint from
#' it - no partial-overlap case exists at this granularity, so a plain
#' containment check (`start <= lo & end >= hi`) is sufficient.
#'
#' @param df Data frame with columns `user_id`, `group_id`, `start`, `end`
#' (integer, half-open, `start < end`, no `NA`s)
#' @param totalUsers Total distinct users in the unfiltered highlights input
#' @param group_names (Optional) Named vector, `group_id` -> display name
#' @param user_names (Optional) Named vector, `user_id` -> display name
#'
#' @returns A list of segments, each
#' `list(start, end, bgColor, textColor, hoverHtml)` (`textColor` is
#' `NA_character_` when there's no cross-group conflict), sorted by `start`
#' and mutually non-overlapping. Empty list if `df` has no rows or covers no
#' gaps
#'
mod_overlap_compute_segments <- function(df, totalUsers, group_names, user_names) {
  if (nrow(df) == 0) {
    return(list())
  }

  breaks <- sort(unique(c(df$start, df$end)))
  if (length(breaks) < 2) {
    return(list())
  }

  segments <- list()
  for (i in seq_len(length(breaks) - 1L)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]

    covering <- df[df$start <= lo & df$end >= hi, , drop = FALSE]
    if (nrow(covering) == 0) {
      next
    }

    pairs <- unique(covering[, c("user_id", "group_id")])
    nUsers <- length(unique(pairs$user_id))
    nGroups <- length(unique(pairs$group_id))

    bgColor <- mod_overlap_interpolate_color(nUsers, totalUsers)
    textColor <- if (nUsers > 1 && nGroups > 1) "#ff0000" else NA_character_

    hoverRows <- data.frame(
      user_name = mod_overlap_display_name(pairs$user_id, user_names),
      group_name = mod_overlap_display_name(pairs$group_id, group_names),
      stringsAsFactors = FALSE
    )

    segments[[length(segments) + 1L]] <- list(
      start = lo,
      end = hi,
      bgColor = bgColor,
      textColor = textColor,
      hoverHtml = mod_overlap_hover_table_html(hoverRows)
    )
  }

  segments
}

#' Internal function to splice overlap segments into source HTML
#'
#' Tokenizes `txt` into alternating tag / plain-text runs (same approach as
#' `mod_highlight_server()`'s internal `buildHighlightedHtml()`), then for
#' each plain-text token clips the (already disjoint, `start`-sorted)
#' `segments` to that token's local plain-text-offset range and splices in a
#' `<span>` carrying the segment's background/text colors and hover-table
#' HTML. A segment that straddles an HTML tag naturally splits into multiple
#' identically-styled spans, one per token fragment it touches - harmless,
#' since each fragment is independently hoverable with identical content.
#'
#' @param txt Character string, source text in HTML format
#' @param segments List of segments as returned by
#' [mod_overlap_compute_segments()]
#'
#' @returns A `shiny::HTML()` string
#'
mod_overlap_build_html <- function(txt, segments) {
  if (is.null(txt) || !nzchar(txt)) {
    return(HTML(""))
  }
  if (length(segments) == 0) {
    return(HTML(txt))
  }

  tagMatches <- gregexpr("<[^>]+>", txt, perl = TRUE)[[1]]
  if (tagMatches[1] == -1) {
    tokens <- txt
    isTag <- FALSE
  } else {
    tagLens <- attr(tagMatches, "match.length")
    tagStarts <- as.integer(tagMatches)
    tagEnds <- tagStarts + tagLens - 1L

    tokens <- character(0)
    isTag <- logical(0)
    cursor <- 1L
    for (i in seq_along(tagStarts)) {
      if (tagStarts[i] > cursor) {
        tokens <- c(tokens, substr(txt, cursor, tagStarts[i] - 1L))
        isTag <- c(isTag, FALSE)
      }
      tokens <- c(tokens, substr(txt, tagStarts[i], tagEnds[i]))
      isTag <- c(isTag, TRUE)
      cursor <- tagEnds[i] + 1L
    }
    if (cursor <= nchar(txt)) {
      tokens <- c(tokens, substr(txt, cursor, nchar(txt)))
      isTag <- c(isTag, FALSE)
    }
  }
  lens <- ifelse(isTag, 0L, nchar(tokens))
  tokEnd <- cumsum(lens)
  tokStart <- tokEnd - lens

  for (j in seq_along(tokens)) {
    if (isTag[j]) {
      next
    }

    chunk <- tokens[j]
    pieces <- character(0)
    cursor <- 0

    for (seg in segments) {
      lo <- max(seg$start, tokStart[j])
      hi <- min(seg$end, tokEnd[j])
      if (lo >= hi) {
        next
      }
      localLo <- lo - tokStart[j]
      localHi <- hi - tokStart[j]

      spanStyle <- if (is.na(seg$textColor)) {
        sprintf("background-color:%s;", seg$bgColor)
      } else {
        sprintf("background-color:%s;color:%s;", seg$bgColor, seg$textColor)
      }

      pieces <- c(pieces, substr(chunk, cursor + 1, localLo))
      pieces <- c(
        pieces,
        sprintf(
          '<span class="overlap-span" style="%s">%s<span class="overlap-hover-table">%s</span></span>',
          spanStyle,
          substr(chunk, localLo + 1, localHi),
          seg$hoverHtml
        )
      )
      cursor <- localHi
    }
    pieces <- c(pieces, substr(chunk, cursor + 1, nchar(chunk)))
    tokens[j] <- paste(pieces, collapse = "")
  }

  HTML(paste(tokens, collapse = ""))
}

#' Module server for the overlap summary display
#'
#' @param id Module ID
#' @param highlights A (reactive) data frame with columns `id`, `user_id`,
#' `group_id`, `start`, `end`. This is the same shape [mod_highlight_server()]
#' returns, plus a `user_id` column the caller must add after combining
#' multiple users' highlight-module outputs (the highlight module's own
#' return value has no `user_id`, since a single instance of it only ever
#' tracks one user's session). `start`/`end` are plain-text (tag-stripped),
#' zero-indexed, half-open offsets into `text`, matching the highlight
#' module's convention. Rows with `start >= end` or `NA` `start`/`end` are
#' dropped before rendering and before computing the color scale's
#' denominator (treated as malformed, not as zero-length-valid highlights).
#' `id` is not relied upon internally (only `(user_id, group_id)` pairs are
#' deduped) so it need not be unique across users.
#' @param group_names (Optional) A named vector, `group_id` -> display name,
#' used both for the group filter select's labels and the `group_name`
#' column in each span's hover table. `group_id` values with no entry here
#' (or if the argument is omitted entirely) fall back to the raw `group_id`
#' @param user_names (Optional) Same idea as `group_names`, for `user_id` ->
#' display name (`user_name` column / user filter labels)
#' @param text A (reactive) character string with the source text to render,
#' in HTML format - same convention as [mod_highlight_server()]'s `text`
#' argument. `NULL`/`""` renders as empty output
#'
#' @import shiny
#'
#' @returns No return value; the module renders directly into its own
#' [uiOutput()] (this is a read-only summary view, not an input module)
#'
#' @export
#'
mod_overlap_server <- function(id, highlights, group_names, user_names, text) {
  if (missing(group_names)) {
    group_names <- NULL
  }
  if (missing(user_names)) {
    user_names <- NULL
  }

  moduleServer(id, function(input, output, session) {
    if (!is.reactive(highlights)) {
      highlights <- reactiveVal(highlights)
    }
    if (!is.reactive(text)) {
      text <- reactiveVal(text)
    }
    if (is.list(group_names)) {
      group_names <- unlist(group_names)
    }
    if (is.list(user_names)) {
      user_names <- unlist(user_names)
    }

    emptyHighlights <- function() {
      data.frame(
        user_id = character(),
        group_id = character(),
        start = integer(),
        end = integer(),
        stringsAsFactors = FALSE
      )
    }

    # Drop malformed rows up front - they never contribute to rendering nor
    # to `totalUsers`, the color scale's fixed denominator
    cleaned <- reactive({
      df <- highlights()
      if (is.null(df) || nrow(df) == 0) {
        return(emptyHighlights())
      }
      df$user_id <- as.character(df$user_id)
      df$group_id <- as.character(df$group_id)
      df$start <- as.integer(df$start)
      df$end <- as.integer(df$end)
      df[!is.na(df$start) & !is.na(df$end) & df$start < df$end, , drop = FALSE]
    })

    # Fixed regardless of the group/user filters, so "all users" on the
    # color scale always means the same thing
    totalUsers <- reactive({
      length(unique(cleaned()$user_id))
    })

    groupChoices <- reactive({
      ids <- unique(cleaned()$group_id)
      setNames(ids, mod_overlap_display_name(ids, group_names))
    })

    userChoices <- reactive({
      ids <- unique(cleaned()$user_id)
      setNames(ids, mod_overlap_display_name(ids, user_names))
    })

    # Refresh filter choices (defaulting to "all selected") whenever the
    # upstream highlights change shape, so stale ids don't linger and newly
    # appearing ids default to included
    observeEvent(cleaned(), {
      updateSelectInput(
        session,
        "group",
        choices = groupChoices(),
        selected = groupChoices()
      )
      updateSelectInput(
        session,
        "user",
        choices = userChoices(),
        selected = userChoices()
      )
    })

    observeEvent(input$resetFilters, {
      updateSelectInput(session, "group", selected = groupChoices())
      updateSelectInput(session, "user", selected = userChoices())
    })

    # A NULL/empty selection is treated as "no filter" rather than "match
    # nothing" - this also covers the brief window before the client
    # acknowledges the initial updateSelectInput() call above
    filtered <- reactive({
      df <- cleaned()
      if (length(input$group) > 0) {
        df <- df[df$group_id %in% input$group, , drop = FALSE]
      }
      if (length(input$user) > 0) {
        df <- df[df$user_id %in% input$user, , drop = FALSE]
      }
      df
    })

    output$textDisplay <- renderUI({
      segments <- mod_overlap_compute_segments(
        filtered(),
        totalUsers(),
        group_names,
        user_names
      )
      mod_overlap_build_html(text(), segments)
    })
  })
}
