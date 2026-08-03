#' Module UI for text highlight controls
#'
#' UI has (top to bottom): a staging area listing not-yet-committed
#' selections, a "Stage" / "Discard" button pair to add the current text
#' selection to that staging area or drop it, and a compact list of
#' selections already committed to the currently active group. Both lists
#' let individual entries be removed again. Committing staged selections to
#' a group happens outside this module - see the `status` column on
#' [mod_highlight_server()]'s return value.
#'
#' Must be paired with a [mod_highlight_ui_text()] call using the *same*
#' module `id`, and both wired up to a single [mod_highlight_server()] call
#' (also using that `id`).
#'
#' @param id Module ID
#' @param element (Optional) Limit text selection to a specific HTML element ID
#' and its children (don't provide the #)
#' @param label (Default = = "Text selections"). Label for the UI list
#' @param info (Has default) HTML to display underneath the label
#'
#' @import shiny
#'
#' @returns Shiny UI element
#' @export
#'
mod_highlight_ui_controls <- function(
  id,
  element,
  label = "Text evidence",
  info = paste(
    "<i>Select pieces of text and click 'Stage' to add them to the list",
    "below. Staged selections are highlighted in orange and remain pending until",
    "all changes are saved.</i>"
  )
) {
  # When no element is set '' JS wil choose the body as default
  if (missing(element)) {
    element <- ""
  }

  ns <- NS(id)
  tagList(
    # Capture highlighted text on the screen
    tags$script(HTML(sprintf(
      "
  document.addEventListener('mouseup', function() {
    var selBox = document.getElementById('%s'); // target element
    if (!selBox) {
      el = document.body;
    }
    var selection = window.getSelection();

    // Check if selection exists and is inside selBox
    if (selection.rangeCount > 0 && selBox.contains(selection.anchorNode) && selBox.contains(selection.focusNode)) {
      Shiny.setInputValue('%s', selection.toString());
    } else {
      Shiny.setInputValue('%s', ''); // optional: clear input if selection is outside
    }
  });
  ",
      element,
      ns("highlighted_text"),
      ns("highlighted_text")
    ))),
    tags$label(label, class = "control-label"),
    HTML(info),
    tags$label("Staged highlights", class = "control-label"),
    div(id = ns("stagedList")),
    fluidRow(
      column(
        6,
        actionButton(ns("addSel"), "Stage selected text", width = "100%")
      ),
      column(
        6,
        actionButton(
          ns("discardStaged"),
          "Discard all staged text",
          width = "100%"
        )
      )
    ),
    tags$hr(),
    tags$label("Previous highlights", class = "control-label"),
    div(id = ns("selList"))
  )
}

#' Module UI for the highlighted text display
#'
#' Contains only a [uiOutput()] that the paired [mod_highlight_server()]
#' renders the source text into (with highlight `<span>`s injected around
#' the currently saved selections). Also carries the selection-capture script,
#' since it needs to reference this element's own container id directly.
#'
#' Must be paired with a [mod_highlight_ui_controls()] call using the *same*
#' module `id`, and both wired up to a single [mod_highlight_server()] call
#' (also using that `id`).
#'
#' @param id Module ID
#'
#' @import shiny
#'
#' @returns Shiny UI element
#' @export
#'
mod_highlight_ui_text <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML(sprintf(
      "
  (function() {
    function getOffset(container, node, offset) {
      var r = document.createRange();
      r.selectNodeContents(container);
      r.setEnd(node, offset);
      return r.toString().length;
    }

    document.addEventListener('mouseup', function() {
      var container = document.getElementById('%1$s');
      if (!container) return;

      var selection = window.getSelection();
      if (
        selection.rangeCount > 0 &&
        container.contains(selection.anchorNode) &&
        container.contains(selection.focusNode)
      ) {
        var range = selection.getRangeAt(0);
        var start = getOffset(container, range.startContainer, range.startOffset);
        var end = getOffset(container, range.endContainer, range.endOffset);
        if (start > end) {
          var tmp = start;
          start = end;
          end = tmp;
        }
        Shiny.setInputValue('%2$s', {start: start, end: end, text: selection.toString()});
      } else {
        Shiny.setInputValue('%2$s', {start: 0, end: 0, text: ''});
      }
    });
  })();
  ",
      ns("textDisplay"),
      ns("selInfo")
    ))),
    uiOutput(ns("textDisplay"))
  )
}

#' Strip HTML tags from a string
#'
#' Converts HTML to the plain-text, tag-stripped coordinate space that
#' [mod_highlight_server()] and its client-side selection capture use for
#' `start`/`end` offsets.
#'
#' @param html Character string in HTML format
#'
#' @returns Character string with tags removed
#' @export
#'
mod_highlight_strip_tags <- function(html) {
  gsub("<[^>]+>", "", html)
}

#' Locate verbatim text matches within a plain-text string
#'
#' Used to backfill `start`/`end` offsets for highlights that were captured
#' as plain text only (e.g. AI-extracted competency evidence), by finding
#' each match's first occurrence that doesn't overlap a previously claimed
#' range. Matches are resolved in the order given, so earlier entries in
#' `matches` get first pick of ambiguous (repeated) occurrences.
#'
#' @param plainText Character string to search within (tag-stripped)
#' @param matches Character vector of verbatim substrings to locate
#'
#' @returns Data frame with columns `start`, `end` (0-indexed, half-open,
#' one row per element of `matches`). Both are `NA` where a match couldn't
#' be located without overlapping an earlier claim.
#' @export
#'
mod_highlight_locate <- function(plainText, matches) {
  claimedStart <- integer(0)
  claimedEnd <- integer(0)
  starts <- rep(NA_integer_, length(matches))
  ends <- rep(NA_integer_, length(matches))

  for (i in seq_along(matches)) {
    m <- matches[i]
    if (is.na(m) || !nzchar(m)) {
      next
    }

    pos <- gregexpr(m, plainText, fixed = TRUE)[[1]]
    if (pos[1] == -1) {
      next
    }
    lens <- attr(pos, "match.length")

    chosen <- NULL
    for (j in seq_along(pos)) {
      s <- pos[j] - 1L
      e <- s + lens[j]
      overlaps <- any(claimedStart < e & claimedEnd > s)
      if (!overlaps) {
        chosen <- c(s, e)
        break
      }
    }
    if (is.null(chosen)) {
      next
    }

    starts[i] <- chosen[1]
    ends[i] <- chosen[2]
    claimedStart <- c(claimedStart, chosen[1])
    claimedEnd <- c(claimedEnd, chosen[2])
  }

  data.frame(start = starts, end = ends)
}

#' Module server for text highlights
#'
#' @param id Module ID
#' @param text A (reactive) character string with the source text to render,
#' in HTML format. Changing this fully resets the module's state back to
#' `init_vals()`, discarding any staged (unsaved) highlights.
#' @param cur_group_id A (reactive) value with the currently active group ID.
#' Highlights already saved to this group are shown in yellow, highlights
#' saved to other groups in light gray, and staged (not yet saved to any
#' group) highlights in light orange. Changing this does not reset the state,
#' only the display: it re-filters the saved-highlights list to the newly
#' active group. The staging area is independent of `cur_group_id` and is
#' unaffected by it.
#' @param init_vals A (reactive) data frame with the initial highlights to
#' seed the state with whenever `text` changes. Expected columns are
#' `group_id`, `start`, `end` and `text`; `start`/`end` must use the same
#' plain-text (tag-stripped), zero-indexed, half-open coordinate system that
#' the module itself uses internally. Any `id` column is ignored - fresh
#' internal ids are always assigned on seed. An optional `status` column
#' ("staged" or "committed") pre-populates the staging area: rows with
#' `status == "staged"` seed as staged (with `group_id` forced to `NA`,
#' regardless of what's supplied), everything else seeds as committed. If
#' `status` is absent, all rows are assumed already committed.
#'
#' @import shiny dplyr
#'
#' @returns A reactive data frame with columns `id`, `group_id`, `start`,
#' `end`, `text` and `status`. Includes both committed highlights (across all
#' groups) and staged (not yet committed) highlights; `status` is
#' "committed" or "staged", and `group_id` is `NA` for staged rows.
#'
#' @export
#'
mod_highlight_server <- function(id, text, cur_group_id, init_vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!is.reactive(text)) {
      text <- reactiveVal(text)
    }
    if (!is.reactive(cur_group_id)) {
      cur_group_id <- reactiveVal(cur_group_id)
    }
    if (!is.reactive(init_vals)) {
      init_vals <- reactiveVal(init_vals)
    }

    # Counter to hand out fresh internal ids
    counter <- reactiveVal(0)

    # Handles of the dynamically registered delete-button observers, keyed by
    # button id, so a full reset can destroy them instead of leaking/duplicating
    deleteObservers <- list()
    stagedDeleteObservers <- list()

    emptyState <- function() {
      data.frame(
        id = integer(),
        group_id = character(),
        start = integer(),
        end = integer(),
        text = character(),
        stringsAsFactors = FALSE
      )
    }

    emptyStagedState <- function() {
      data.frame(
        id = integer(),
        start = integer(),
        end = integer(),
        text = character(),
        stringsAsFactors = FALSE
      )
    }

    state <- reactiveVal(emptyState())
    # Highlights that have been staged but not yet committed to a group.
    # Rows only leave here via individual delete, "Discard", or a full
    # reseed from init_vals - there is no internal action that promotes a
    # staged row into `state()` (committing happens outside this module).
    staged <- reactiveVal(emptyStagedState())

    # Build fresh state + staged data frames from init_vals, ignoring any
    # incoming id - fresh internal ids are always assigned on seed. Rows are
    # split by the optional `status` column: "staged" rows seed into
    # `staged` (with group_id forced to NA), everything else (including a
    # missing `status` column entirely) seeds into `state` as committed.
    seedFromInitVals <- function() {
      iv <- init_vals()
      if (is.null(iv) || nrow(iv) == 0) {
        return(list(state = emptyState(), staged = emptyStagedState()))
      }

      isStaged <- if (is.null(iv$status)) {
        rep(FALSE, nrow(iv))
      } else {
        as.character(iv$status) == "staged"
      }
      ids <- seq_len(nrow(iv))

      list(
        state = data.frame(
          id = ids[!isStaged],
          group_id = as.character(iv$group_id)[!isStaged],
          start = as.integer(iv$start)[!isStaged],
          end = as.integer(iv$end)[!isStaged],
          text = as.character(iv$text)[!isStaged],
          stringsAsFactors = FALSE
        ),
        staged = data.frame(
          id = ids[isStaged],
          start = as.integer(iv$start)[isStaged],
          end = as.integer(iv$end)[isStaged],
          text = as.character(iv$text)[isStaged],
          stringsAsFactors = FALSE
        )
      )
    }

    # Splice highlight <span>s into the source HTML for the current state.
    # Coordinates are plain-text (tag-stripped), zero-indexed and half-open,
    # matching what the client-side selection capture produces. `df` must
    # already carry the per-row `color` to use - this function does not
    # decide colors, only where to splice them in.
    buildHighlightedHtml <- function(txt, df) {
      if (is.null(txt) || !nzchar(txt)) {
        return(HTML(""))
      }
      if (nrow(df) == 0) {
        return(HTML(txt))
      }

      # Tokenize into alternating tag / plain-text runs. Deliberately not a
      # strsplit() on a zero-width lookaround pattern: strsplit()'s handling
      # of zero-width matches in R splits "<b>" into "<" and "b>" instead of
      # keeping it whole, silently treating every tag as plain text.
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

      # Collect the (local) intervals each highlight touches, per token
      tokenIntervals <- vector("list", length(tokens))

      rows <- df[order(df$start), ]
      for (i in seq_len(nrow(rows))) {
        h <- rows[i, ]
        color <- h$color

        for (j in seq_along(tokens)) {
          if (isTag[j]) {
            next
          }
          lo <- max(h$start, tokStart[j])
          hi <- min(h$end, tokEnd[j])
          if (lo >= hi) {
            next
          }

          tokenIntervals[[j]] <- rbind(
            tokenIntervals[[j]],
            data.frame(
              lo = lo - tokStart[j],
              hi = hi - tokStart[j],
              color = color
            )
          )
        }
      }

      for (j in seq_along(tokens)) {
        ivs <- tokenIntervals[[j]]
        if (is.null(ivs)) {
          next
        }
        ivs <- ivs[order(ivs$lo), ]

        chunk <- tokens[j]
        pieces <- character(0)
        cursor <- 0
        for (k in seq_len(nrow(ivs))) {
          pieces <- c(pieces, substr(chunk, cursor + 1, ivs$lo[k]))
          pieces <- c(
            pieces,
            sprintf(
              '<span style="background-color:%s;">%s</span>',
              ivs$color[k],
              substr(chunk, ivs$lo[k] + 1, ivs$hi[k])
            )
          )
          cursor <- ivs$hi[k]
        }
        pieces <- c(pieces, substr(chunk, cursor + 1, nchar(chunk)))
        tokens[j] <- paste(pieces, collapse = "")
      }

      HTML(paste(tokens, collapse = ""))
    }

    output$textDisplay <- renderUI({
      committed <- state()
      committed$color <- as.character(ifelse(
        as.character(committed$group_id) %in% as.character(cur_group_id()),
        "#fff59d",
        "#e0e0e0"
      ))

      stagedRows <- staged()
      stagedRows$color <- rep("#f8e5cd", nrow(stagedRows))

      combined <- bind_rows(
        committed[, c("start", "end", "color")],
        stagedRows[, c("start", "end", "color")]
      )

      buildHighlightedHtml(text(), combined)
    })

    # Clear and rebuild the sidebar list for the currently active group.
    # Registers a delete-observer for any button id that doesn't have one yet.
    rebuildSelList <- function() {
      removeUI(selector = paste0("#", ns("selList"), " > div"), multiple = TRUE)

      rows <- state()[
        state()$group_id == as.character(cur_group_id()),
        ,
        drop = FALSE
      ]
      if (nrow(rows) == 0) {
        return(invisible())
      }
      rows <- rows[order(-rows$id), ]

      for (i in seq_len(nrow(rows))) {
        rid <- rows$id[i]
        btnId <- paste0("del", rid)

        insertUI(
          paste0("#", ns("selList")),
          "beforeEnd",
          tags$div(
            actionButton(
              ns(btnId),
              label = NULL,
              icon = icon("trash"),
              style = "padding: 3px;"
            ),
            tags$span(rows$text[i], style = "color:#0d6efd;"),
            id = ns(paste0("item", rid))
          )
        )

        if (is.null(deleteObservers[[btnId]])) {
          local({
            rid_ <- rid
            btnId_ <- btnId
            deleteObservers[[btnId_]] <<- observeEvent(input[[btnId_]], {
              state(state()[state()$id != rid_, , drop = FALSE])
            })
          })
        }
      }
    }

    # Clear and rebuild the staging list. Unlike rebuildSelList(), this is
    # never filtered by cur_group_id() and never re-triggered by it - staged
    # highlights don't belong to a group yet, so the staging area stays put
    # while the user switches groups underneath it.
    rebuildStagedList <- function() {
      removeUI(
        selector = paste0("#", ns("stagedList"), " > div"),
        multiple = TRUE
      )

      rows <- staged()
      if (nrow(rows) == 0) {
        return(invisible())
      }
      rows <- rows[order(-rows$id), ]

      for (i in seq_len(nrow(rows))) {
        rid <- rows$id[i]
        btnId <- paste0("delStaged", rid)

        insertUI(
          paste0("#", ns("stagedList")),
          "beforeEnd",
          tags$div(
            actionButton(
              ns(btnId),
              label = NULL,
              icon = icon("trash"),
              style = "padding: 3px;"
            ),
            tags$span(rows$text[i], style = "color:#0d6efd;"),
            id = ns(paste0("stagedItem", rid))
          )
        )

        if (is.null(stagedDeleteObservers[[btnId]])) {
          local({
            rid_ <- rid
            btnId_ <- btnId
            stagedDeleteObservers[[btnId_]] <<- observeEvent(input[[btnId_]], {
              staged(staged()[staged()$id != rid_, , drop = FALSE])
            })
          })
        }
      }
    }

    # `text` changing fully resets state (and staging) back to init_vals
    observeEvent(text(), {
      for (obs in deleteObservers) {
        obs$destroy()
      }
      deleteObservers <<- list()
      for (obs in stagedDeleteObservers) {
        obs$destroy()
      }
      stagedDeleteObservers <<- list()

      seeded <- seedFromInitVals()
      counter(nrow(seeded$state) + nrow(seeded$staged))
      state(seeded$state)
      staged(seeded$staged)

      # Rebuild both lists explicitly rather than relying on the state()/
      # staged() watchers below: those use ignoreInit = TRUE, and their very
      # first run (which establishes their reactive subscription) happens
      # *after* the seeding above already changed state()/staged() once -
      # so left to themselves they'd silently skip rendering the seeded
      # (previously-saved) highlights on initial load.
      rebuildSelList()
      rebuildStagedList()
    })

    # `cur_group_id` changing only affects display: recolor + reload the
    # (group-filtered) sidebar list, the underlying state is untouched
    observeEvent(
      cur_group_id(),
      {
        rebuildSelList()
      },
      ignoreInit = TRUE
    )

    # Any state change (add/remove/reset) refreshes the sidebar list
    observeEvent(
      state(),
      {
        rebuildSelList()
      },
      ignoreInit = TRUE
    )

    # Any staged change (add/remove/discard) refreshes the staging list
    observeEvent(
      staged(),
      {
        rebuildStagedList()
      },
      ignoreInit = TRUE
    )

    # Add the current selection to the staging area (not yet assigned to a group)
    observeEvent(input$addSel, {
      sel <- input$selInfo
      req(sel, sel$end > sel$start)

      overlaps <- bind_rows(state(), staged()) |>
        filter(start < sel$end, end > sel$start) |>
        nrow() >
        0

      if (overlaps) {
        showNotification(
          "This selection overlaps with an existing highlight and cannot be added.",
          type = "warning"
        )
      } else {
        counter(counter() + 1)
        staged(bind_rows(
          staged(),
          data.frame(
            id = counter(),
            start = as.integer(sel$start),
            end = as.integer(sel$end),
            text = sel$text,
            stringsAsFactors = FALSE
          )
        ))
      }
    })

    # Discard all staged (unsaved) highlights without assigning them to a group
    observeEvent(input$discardStaged, {
      staged(emptyStagedState())
    })

    # Return the full data frame of highlights, both committed (assigned to a
    # group) and staged (not yet assigned). `status` distinguishes the two;
    # `group_id` is always NA for staged rows.
    return(reactive({
      committed <- state()
      committed$status <- rep("committed", nrow(committed))

      stagedRows <- staged()
      stagedRows$group_id <- rep(NA_character_, nrow(stagedRows))
      stagedRows$status <- rep("staged", nrow(stagedRows))

      bind_rows(
        committed[, c("id", "group_id", "start", "end", "text", "status")],
        stagedRows[, c("id", "group_id", "start", "end", "text", "status")]
      )
    }))
  })
}
