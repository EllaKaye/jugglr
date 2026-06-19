# Validate the `direction` argument and collapse the "h"/"v" shorthands to the
# canonical "horizontal"/"vertical" used throughout the ladder plotting code.
normalise_direction <- function(direction, error_call = rlang::caller_env()) {
  direction <- rlang::arg_match(
    direction,
    c("horizontal", "vertical", "h", "v"),
    error_call = error_call
  )
  if (direction %in% c("h", "horizontal")) "horizontal" else "vertical"
}

warn_if_props_hidden <- function(siteswap, max_prop) {
  if (siteswap@valid && max_prop < siteswap@n_props) {
    cli::cli_warn(c(
      "!" = "There are {siteswap@n_props} props in siteswap '{siteswap@sequence}', but only {max_prop} {?is/are} shown.",
      "i" = "Increase {.arg n_cycles} to see more throws.",
      "i" = "Setting n_cycles >= {siteswap@period * siteswap@n_props * 2} will show each prop thrown at least twice."
    ))
  }
}

okabe_ito <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

# Uses Okabe-Ito for up to 7 props; falls back to ggplot2 default for more
# (rare for valid patterns but possible for invalid ones, e.g. "21").
prop_color_scale <- function(n_props) {
  if (n_props <= 7L) {
    scale_color_manual(values = okabe_ito[seq_len(n_props)], guide = "none")
  } else {
    scale_color_discrete(guide = "none")
  }
}

# Style set for plot subtitles: "note" class renders secondary lines smaller and paler.
plot_subtitle_style <- marquee::style_set(
  base = marquee::base_style(),
  note = marquee::style(size = 10, color = "#888888")
)

# direction: NULL (timelines), "horizontal"/"h", or "vertical"/"v"
title_subtitle_theme <- function(direction = NULL) {
  is_horizontal <- !is.null(direction) &&
    direction %in% c("horizontal", "h")
  is_vertical <- !is.null(direction) &&
    direction %in% c("vertical", "v")
  sub_margin <- if (is_horizontal) {
    margin(0, 0, 2, 0)
  } else if (is_vertical) {
    margin(0, 0, 20, 0)
  } else {
    margin(0, 0, 8, 0)
  }
  theme(
    # Span title/subtitle across the full plot, not the panel column. Otherwise
    # `coord_fixed` (in ladders) can shrink the panel so the marquee subtitle's
    # `width = unit(1, "npc")` wraps too narrowly and overlaps the panel.
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      size = rel(1.8),
      margin = margin(12, 0, 8, 0)
    ),
    plot.subtitle = marquee::element_marquee(
      style = plot_subtitle_style,
      size = rel(1.2),
      lineheight = 1.2,
      margin = sub_margin,
      width = unit(1, "npc")
    )
  )
}

plot_subtitle <- function(siteswap, extra = NULL) {
  if (!siteswap@valid) {
    return("Not a valid juggling pattern.")
  }

  n_props <- siteswap@n_props
  prop_word <- if (n_props == 1L) "prop" else "props"
  base <- paste0(
    "A valid ",
    siteswap@type,
    " siteswap with ",
    n_props,
    " ",
    prop_word
  )

  if (siteswap@type == "passing") {
    base <- paste0(base, " across ", siteswap@n_jugglers, " jugglers")
  }

  notes <- c(
    if (n_props > 1L) "Each colour represents a different prop.",
    extra
  )

  if (length(notes) == 0L) {
    paste0(base, ".")
  } else {
    # Hard line break (  \n) keeps everything in one paragraph so marquee
    # span styling renders correctly in all plot types.
    notes_text <- paste(notes, collapse = "  \n")
    paste0(base, ".  \n{.note ", notes_text, "}")
  }
}

generate_parabola <- function(x1, x2, height, prop, beat, n_points = 100) {
  vx <- (x1 + x2) / 2 # vertex
  xs <- seq(x1, x2, length.out = n_points)
  ys <- height * (1 - ((xs - vx) / (vx - x1))^2)

  data.frame(x = xs, y = ys, prop = prop, beat = beat)
}

# Hand-axis separation (in hand units) between fanned duplicate ladder arcs.
# `create_curve_points()` nudges the Bezier control point's hand-axis coordinate
# by `fan * spread`, so otherwise identical multiplex throws spread apart instead
# of overlapping (the ladder analogue of the timeline peak-height fan in
# `fan_duplicate_heights()`). Same-hand (even) arcs are short and curve toward
# centre, so a given control nudge separates them much more than it does the long
# cross-hand (odd) lines; even arcs therefore need a smaller spread.
ladder_fan_spread_odd <- 0.3
ladder_fan_spread_even <- 0.14

create_curve_points <- function(
  x_start,
  y_start,
  x_end,
  y_end,
  direction,
  prop_num,
  fan = 0,
  n_points = 50
) {
  t <- seq(0, 1, length.out = n_points)

  # The control point sits at the beat-midpoint with its hand-axis coordinate at
  # 0.5. For a same-hand (even) throw this bows the arc toward centre; for a
  # cross-hand (odd) throw the control lies on the line, so the Bezier collapses
  # to an exact straight segment. Fanning the hand-axis coordinate is what
  # separates duplicate multiplex throws: even arcs gain distinct apex heights
  # and odd lines bow symmetrically apart, while unique throws (fan = 0) are
  # unchanged. Same-hand throws (equal hand coordinates) use the smaller spread.
  same_hand <- if (direction == "vertical") {
    x_start == x_end
  } else {
    y_start == y_end
  }
  spread <- if (same_hand) ladder_fan_spread_even else ladder_fan_spread_odd
  hand_control <- 0.5 + fan * spread
  if (direction == "vertical") {
    x_control <- hand_control
    y_control <- mean(c(y_start, y_end))
  } else {
    x_control <- mean(c(x_start, x_end))
    y_control <- hand_control
  }

  # Quadratic Bezier curve
  x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * x_control + t^2 * x_end
  y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * y_control + t^2 * y_end

  data.frame(x = x, y = y, prop = prop_num)
}

build_ladder_plot <- function(
  plot_data,
  direction,
  title,
  subtitle = NULL,
  beat_step = 1L
) {
  is_vertical <- direction == "vertical"
  max_prop <- max(plot_data$prop, na.rm = TRUE)

  if (is_vertical) {
    # having -beat means 0 is at the "top" of the ladder,
    # and we go down the negative y-axis as time increases
    plot_data <- plot_data |>
      mutate(
        x_start = .data$hand,
        y_start = -.data$beat,
        x_end = .data$catch_hand,
        y_end = -.data$catch_beat,
      )
  } else {
    plot_data <- plot_data |>
      mutate(
        x_start = .data$beat,
        y_start = .data$hand,
        x_end = .data$catch_beat,
        y_end = .data$catch_hand
      )
  }

  # Fan identical multiplex throws apart (same hand, height, beat and landing).
  plot_data <- duplicate_fan_rank(
    plot_data,
    c("beat", "hand", "throw", "catch_beat", "catch_hand")
  )

  # Every throw goes through one Bezier generator: odd cross-hand throws come out
  # straight, even same-hand throws bow toward centre, and the fan rank nudges
  # duplicate multiplex throws apart so each prop's arc stays visible. This
  # replaces the former even-arc/odd-segment split, which left identical
  # multiplex throws fully overlapping.
  if (nrow(plot_data) > 0) {
    curve_data <- plot_data |>
      purrr::pmap(\(
        x_start,
        y_start,
        x_end,
        y_end,
        beat,
        hand,
        throw,
        prop,
        fan,
        ...
      ) {
        curve_pts <- create_curve_points(
          x_start,
          y_start,
          x_end,
          y_end,
          direction,
          prop,
          fan = fan
        )
        curve_pts$group <- paste0(
          "arc_",
          beat,
          "_",
          hand,
          "_",
          throw,
          "_",
          prop
        )
        curve_pts
      }) |>
      purrr::list_rbind()
  } else {
    curve_data <- data.frame(
      x = numeric(0),
      y = numeric(0),
      prop = numeric(0),
      group = character(0)
    )
  }

  p <- ggplot()

  if (nrow(curve_data) > 0) {
    p <- p +
      geom_path(
        data = curve_data,
        aes(
          x = .data$x,
          y = .data$y,
          group = .data$group,
          color = factor(.data$prop)
        ),
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  # Rungs/labels sit on the regular beat grid; beat_step > 1 (e.g. synchronous)
  # skips the empty intermediate beats so labels read 0, 2, 4, ...
  min_beat <- min(plot_data$beat)
  max_beat <- max(plot_data$catch_beat)
  beats_grid <- seq(min_beat, max_beat, by = beat_step)

  if (is_vertical) {
    rung_data <- data.frame(
      x = 0,
      y = -beats_grid,
      xend = 1,
      yend = -beats_grid
    )
    rail_data <- data.frame(
      x = c(0, 1),
      y = c(-min_beat, -min_beat),
      xend = c(0, 1),
      yend = c(-max_beat, -max_beat)
    )
  } else {
    rung_data <- data.frame(
      x = beats_grid,
      y = 0,
      xend = beats_grid,
      yend = 1
    )
    rail_data <- data.frame(
      x = c(min_beat, min_beat),
      y = c(0, 1),
      xend = c(max_beat, max_beat),
      yend = c(0, 1)
    )
  }

  # hand_axis gets limits; time_axis gets no limits. The ratio compensates for
  # beat_step so larger steps keep the same rendered proportions.
  hand_limits <- c(-0.2, 1.2)
  ratio <- if (is_vertical) 0.3 / beat_step else 3 * beat_step

  # Beat number labels positioned just outside the bottom/left rail in data coords
  beat_label_data <- if (!is_vertical) {
    data.frame(x = beats_grid, y = -0.12, label = beats_grid)
  } else {
    data.frame(x = -0.12, y = -beats_grid, label = beats_grid)
  }

  p <- p +
    geom_text(
      data = beat_label_data,
      aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      size = 3,
      colour = "grey60",
      hjust = if (!is_vertical) 0.5 else 1,
      vjust = if (!is_vertical) 1 else 0.5
    ) +
    geom_segment(
      data = rung_data,
      aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.3,
      color = "grey80"
    ) +
    geom_segment(
      data = rail_data,
      aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.8
    ) +
    prop_color_scale(max_prop) +
    scale_x_continuous(
      limits = if (is_vertical) hand_limits,
      breaks = NULL
    ) +
    scale_y_continuous(
      limits = if (!is_vertical) hand_limits,
      breaks = NULL
    ) +
    coord_fixed(ratio = ratio) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    title_subtitle_theme(direction) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = ""
    )

  p
}

build_simple_ladder <- function(
  siteswap,
  n_cycles,
  direction,
  title,
  subtitle,
  is_even_col,
  title_seq,
  beat_step = 1L
) {
  plot_data <- throw_data(siteswap, n_cycles = n_cycles) |>
    mutate(is_even = {{ is_even_col }}) |>
    filter(.data$throw > 0)
  build_ladder_plot(
    plot_data,
    direction,
    title = if (title) title_seq else NULL,
    subtitle = if (subtitle) plot_subtitle(siteswap) else NULL,
    beat_step = beat_step
  )
}

# For two-sided synchronous timelines `x_labels` holds the bottom-hand (hand 1)
# throw notation and `top_labels` the top-hand (hand 0) notation; single-sided
# types pass only `x_labels` and leave `top_labels` NULL.
build_simple_timeline <- function(
  siteswap,
  n_cycles,
  title,
  subtitle,
  x_labels,
  title_seq,
  two_sided = FALSE,
  top_labels = NULL,
  subtitle_extra = NULL
) {
  td <- throw_data(siteswap, n_cycles = n_cycles)
  max_prop <- max(td$prop, na.rm = TRUE)

  parabolas <- td |>
    filter(.data$throw > 0) |>
    fan_duplicate_heights() |>
    mutate(prop = factor(.data$prop)) |>
    purrr::pmap(\(beat, catch_beat, peak, prop, hand, ...) {
      df <- generate_parabola(beat, catch_beat, peak, prop, beat)
      if (two_sided && hand == 1L) {
        df$y <- -df$y
      }
      df$hand <- hand
      df
    }) |>
    purrr::list_rbind()

  warn_if_props_hidden(siteswap, max_prop)

  mapping <- aes(
    x = .data$x,
    y = .data$y,
    group = interaction(.data$beat, .data$prop),
    color = .data$prop
  )

  p <- ggplot(parabolas, mapping)
  if (two_sided) {
    p <- p + geom_hline(yintercept = 0, color = "grey80", linewidth = 0.5)
  }
  p <- p +
    geom_path(linewidth = 2, show.legend = FALSE) +
    prop_color_scale(max_prop)

  beats <- sort(unique(td$beat))
  if (two_sided) {
    p <- p +
      scale_x_continuous(
        breaks = beats,
        labels = rep(x_labels, n_cycles),
        sec.axis = dup_axis(labels = rep(top_labels, n_cycles))
      ) +
      theme_void() +
      theme(
        axis.text.x.bottom = element_text(face = "bold", size = rel(1.5)),
        axis.text.x.top = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 20, 20, 20)
      )
  } else {
    p <- p +
      scale_x_continuous(breaks = beats, labels = rep(x_labels, n_cycles)) +
      theme_void() +
      theme(
        axis.text.x = element_text(face = "bold", size = rel(1.5)),
        plot.margin = margin(10, 20, 20, 20)
      )
  }

  p +
    title_subtitle_theme() +
    labs(
      title = if (title) title_seq else NULL,
      subtitle = if (subtitle) {
        plot_subtitle(siteswap, extra = subtitle_extra)
      } else {
        NULL
      }
    )
}

# Identical multiplex throws (matching `group_cols`) produce fully overlapping
# arcs. Assign each a centred rank within its duplicate group: 0 when the throw
# is unique, and a symmetric spread otherwise (e.g. -0.5, +0.5 for a pair like
# `[33]`; -1, 0, 1 for a triple). Callers turn this rank into a small offset so
# every prop stays visible.
duplicate_fan_rank <- function(data, group_cols) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    mutate(fan = dplyr::row_number() - (dplyr::n() + 1) / 2) |>
    dplyr::ungroup()
}

# Multiplex slots can throw several props of the *same* height from one hand,
# producing identical, fully overlapping arcs. Give each such duplicate a
# slightly different peak height (a symmetric fan) so every prop is visible.
# Throws with distinct heights or catch beats keep their exact height.
fan_duplicate_heights <- function(arc_data, fan_offset = 0.18) {
  arc_data |>
    duplicate_fan_rank(c("beat", "hand", "throw", "catch_beat")) |>
    mutate(peak = .data$throw * (1 + fan_offset * .data$fan))
}

# Per-hand labels for two-sided synchronous timelines: split each "(a,b)" slot
# into the top-hand (hand 0) label `a` and the bottom-hand (hand 1) label `b`.
sync_hand_labels <- function(full_sequence) {
  slots <- str_extract_all(full_sequence, "\\([^)]+\\)")[[1]]
  list(
    top = str_replace(slots, "^\\((.+),(.+)\\)$", "\\1"),
    bottom = str_replace(slots, "^\\((.+),(.+)\\)$", "\\2")
  )
}

generate_pass_arc <- function(
  x_start,
  y_start,
  x_end,
  y_end,
  throw_height,
  prop,
  beat,
  n_points = 100
) {
  xs <- seq(x_start, x_end, length.out = n_points)
  t <- (xs - x_start) / (x_end - x_start)
  y_linear <- y_start * (1 - t) + y_end * t
  vx <- (x_start + x_end) / 2
  y_arc <- throw_height * (1 - ((xs - vx) / (vx - x_start))^2)
  data.frame(x = xs, y = y_linear + y_arc, prop = prop, beat = beat)
}

build_passing_ladder_plot <- function(
  plot_data,
  direction,
  title,
  n_jugglers,
  hand_gap = 2L,
  subtitle = NULL
) {
  is_vertical <- direction == "vertical"
  max_prop <- max(plot_data$prop, na.rm = TRUE)

  plot_data <- plot_data |>
    mutate(
      y_throw = (.data$juggler - 1L) * hand_gap + .data$hand,
      y_catch = (.data$catch_juggler - 1L) * hand_gap + .data$catch_hand
    )

  if (is_vertical) {
    plot_data <- plot_data |>
      mutate(
        x_start = .data$y_throw,
        y_start = -.data$beat,
        x_end = .data$y_catch,
        y_end = -.data$catch_beat
      )
  } else {
    plot_data <- plot_data |>
      mutate(
        x_start = .data$beat,
        y_start = .data$y_throw,
        x_end = .data$catch_beat,
        y_end = .data$y_catch
      )
  }

  # Fan identical multiplex throws apart; a multiplex pass repeats across the
  # same throw/juggler pair, so include the juggler columns in the grouping.
  plot_data <- duplicate_fan_rank(
    plot_data,
    c(
      "beat",
      "hand",
      "throw",
      "catch_beat",
      "catch_hand",
      "juggler",
      "catch_juggler"
    )
  )

  # Every throw goes through one Bezier generator. Self-thrown even throws bow
  # toward their juggler's centre line; passes and odd throws keep a straight
  # (collinear) control point. The fan offset on the hand-axis control coordinate
  # separates duplicate multiplex throws so each prop stays visible.
  if (nrow(plot_data) > 0) {
    curve_data <- plot_data |>
      purrr::pmap(\(
        x_start,
        y_start,
        x_end,
        y_end,
        beat,
        hand,
        throw,
        prop,
        juggler,
        is_pass,
        is_even,
        fan,
        ...
      ) {
        bow <- !is_pass && is_even
        cross_base <- if (bow) {
          (juggler - 1L) * hand_gap + 0.5
        } else if (is_vertical) {
          mean(c(x_start, x_end))
        } else {
          mean(c(y_start, y_end))
        }
        spread <- if (bow) ladder_fan_spread_even else ladder_fan_spread_odd
        t <- seq(0, 1, length.out = 50)
        if (is_vertical) {
          x_ctrl <- cross_base + fan * spread
          y_ctrl <- mean(c(y_start, y_end))
        } else {
          x_ctrl <- mean(c(x_start, x_end))
          y_ctrl <- cross_base + fan * spread
        }
        x_pts <- (1 - t)^2 * x_start + 2 * (1 - t) * t * x_ctrl + t^2 * x_end
        y_pts <- (1 - t)^2 * y_start + 2 * (1 - t) * t * y_ctrl + t^2 * y_end
        data.frame(
          x = x_pts,
          y = y_pts,
          prop = prop,
          group = paste0("arc_", beat, "_", hand, "_", throw, "_", prop)
        )
      }) |>
      purrr::list_rbind()
  } else {
    curve_data <- data.frame(
      x = numeric(0),
      y = numeric(0),
      prop = numeric(0),
      group = character(0)
    )
  }

  p <- ggplot()

  if (nrow(curve_data) > 0) {
    p <- p +
      geom_path(
        data = curve_data,
        aes(
          x = .data$x,
          y = .data$y,
          group = .data$group,
          color = factor(.data$prop)
        ),
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  hand_max <- (n_jugglers - 1L) * hand_gap + 1L

  if (is_vertical) {
    y_range <- range(c(plot_data$y_start, plot_data$y_end))
    rung_list <- lapply(seq_len(n_jugglers), function(j) {
      j_y_vals <- sort(unique(plot_data$y_start[plot_data$juggler == j]))
      x0 <- (j - 1L) * hand_gap
      data.frame(x = x0, y = j_y_vals, xend = x0 + 1L, yend = j_y_vals)
    })
    rung_data <- do.call(rbind, rung_list)
    rail_list <- lapply(seq_len(n_jugglers), function(j) {
      x0 <- (j - 1L) * hand_gap
      data.frame(
        x = c(x0, x0 + 1L),
        y = c(y_range[2L], y_range[2L]),
        xend = c(x0, x0 + 1L),
        yend = c(y_range[1L], y_range[1L])
      )
    })
    rail_data <- do.call(rbind, rail_list)
    hand_limits <- c(-0.2, hand_max + 0.2)
    ratio <- 1 / hand_gap
  } else {
    x_range <- range(c(plot_data$x_start, plot_data$x_end))
    rung_list <- lapply(seq_len(n_jugglers), function(j) {
      j_beats <- sort(unique(plot_data$x_start[plot_data$juggler == j]))
      y0 <- (j - 1L) * hand_gap
      data.frame(x = j_beats, y = y0, xend = j_beats, yend = y0 + 1L)
    })
    rung_data <- do.call(rbind, rung_list)
    rail_list <- lapply(seq_len(n_jugglers), function(j) {
      y0 <- (j - 1L) * hand_gap
      data.frame(
        x = c(x_range[1L], x_range[1L]),
        y = c(y0, y0 + 1L),
        xend = c(x_range[2L], x_range[2L]),
        yend = c(y0, y0 + 1L)
      )
    })
    rail_data <- do.call(rbind, rail_list)
    hand_limits <- c(-0.2, hand_max + 0.2)
    ratio <- hand_gap
  }

  n_beats <- if (is_vertical) max(-plot_data$y_end) else max(plot_data$x_end)

  # Beat number labels positioned just outside the bottom/left rail in data coords
  beat_label_data <- if (!is_vertical) {
    data.frame(x = seq_len(n_beats), y = -0.12, label = seq_len(n_beats))
  } else {
    data.frame(x = -0.12, y = -seq_len(n_beats), label = seq_len(n_beats))
  }

  p +
    geom_text(
      data = beat_label_data,
      aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      size = 3,
      colour = "grey60",
      hjust = if (!is_vertical) 0.5 else 1,
      vjust = if (!is_vertical) 1 else 0.5
    ) +
    geom_segment(
      data = rung_data,
      aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.3,
      color = "grey80"
    ) +
    geom_segment(
      data = rail_data,
      aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linewidth = 0.8
    ) +
    prop_color_scale(max_prop) +
    scale_x_continuous(
      limits = if (is_vertical) hand_limits,
      breaks = NULL
    ) +
    scale_y_continuous(
      limits = if (!is_vertical) hand_limits,
      breaks = NULL
    ) +
    coord_fixed(ratio = ratio) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    title_subtitle_theme(direction) +
    labs(title = title, subtitle = subtitle, x = "", y = "")
}
