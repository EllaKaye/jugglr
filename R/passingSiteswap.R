#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
#' @include utils-passing.R
NULL

#' Passing siteswap
#'
#' Creates a passing siteswap object from a multi-juggler sequence in
#' `<A|B>` notation. Each section separated by `|` gives one juggler's throws;
#' a `p` suffix (e.g. `"3p"`) marks a throw that passes to the next juggler.
#' Fractional notation (e.g. `"<4.5 3 3 | 3 4 3.5>"`) is also supported,
#' where `.5` throws are passes and the second juggler is half a beat later.
#'
#' In p-notation, both jugglers are fully synchronised: they throw on the same
#' beat and alternate hands together. In fractional notation, the second juggler
#' is offset by half a beat in the combined timeline.
#'
#' @param sequence A single character string of passing siteswap notation,
#'   e.g. `"<3p 3 3 3 3 3 | 3p 3 3 3 3 3>"` or `"<4.5 3 3 | 3 4 3.5>"`.
#'   Spaces within sections are ignored; `<3p33|3p33>` and `<3p 3 3|3p 3 3>`
#'   are equivalent.
#'
#' @returns A `passingSiteswap` S7 object.
#'
#' @prop type Always `"passing"` (read-only).
#' @prop is_fractional `TRUE` for fractional notation; `FALSE` for p-notation.
#' @prop n_jugglers Number of jugglers (sections in the sequence).
#' @prop sequences_by_juggler List of character vectors, one per juggler, each
#'   giving the raw tokens (e.g. `c("3p", "3", "3")`).
#' @prop throws_by_juggler List of numeric vectors, one per juggler, of throw
#'   heights (integers for p-notation; may include `.5` for fractional).
#' @prop is_pass_by_juggler List of logical vectors, one per juggler, indicating
#'   which throws are passes.
#' @prop period Number of throws per juggler per cycle.
#' @prop symmetry `"symmetrical"` when period is odd; `"asymmetrical"` when
#'   period is even.
#' @prop n_props Total number of props (`sum(all throws) / period`).
#' @prop can_throw `TRUE` if no two throws land on the same
#'   `(juggler, beat, hand)` triple (no collision).
#' @prop satisfies_average_theorem `TRUE` if `n_props` is a whole number.
#' @prop valid `TRUE` if both `can_throw` and `satisfies_average_theorem` are
#'   `TRUE`.
#'
#' @export
#'
#' @examples
#' passingSiteswap("<3p 3 3 3 3 3 | 3p 3 3 3 3 3>")
#' passingSiteswap("<4p 3 | 3 4p>")
#'
#' # Compact and spaced forms are equivalent
#' s1 <- passingSiteswap("<3p33|3p33>")
#' s2 <- passingSiteswap("<3p 3 3 | 3p 3 3>")
#' identical(s1@throws_by_juggler, s2@throws_by_juggler)
#'
#' s <- passingSiteswap("<4p 3 | 3 4p>")
#' s@n_props
#' s@valid
passingSiteswap <- new_class(
  "passingSiteswap",
  properties = list(
    type = new_property(class = class_character, getter = function(self) {
      "passing"
    }),
    is_fractional = new_property(
      class = class_logical,
      getter = function(self) {
        is_fractional_notation(self@sequence)
      }
    ),
    sequences_by_juggler = new_property(
      class = class_list,
      getter = function(self) {
        parse_passing_sequence(self@sequence, self@is_fractional)
      }
    ),
    throws_by_juggler = new_property(
      class = class_list,
      getter = function(self) {
        lapply(
          self@sequences_by_juggler,
          get_passing_throws,
          fractional = self@is_fractional
        )
      }
    ),
    is_pass_by_juggler = new_property(
      class = class_list,
      getter = function(self) {
        lapply(
          self@sequences_by_juggler,
          get_pass_flags,
          fractional = self@is_fractional
        )
      }
    ),
    n_jugglers = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@sequences_by_juggler)
      }
    ),
    period = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@sequences_by_juggler[[1L]])
      }
    ),
    symmetry = new_property(
      class = class_character,
      getter = function(self) {
        ifelse(is_even(self@period), "asymmetrical", "symmetrical")
      }
    ),
    n_props = new_property(
      class = class_numeric,
      getter = function(self) {
        sum(unlist(self@throws_by_juggler)) / self@period
      }
    ),
    can_throw = new_property(
      class = class_logical,
      getter = function(self) {
        can_pass_throw(
          self@throws_by_juggler,
          self@is_pass_by_juggler,
          self@period,
          fractional = self@is_fractional
        )
      }
    ),
    satisfies_average_theorem = new_property(
      class = class_logical,
      getter = function(self) {
        is_whole_number(self@n_props)
      }
    ),
    valid = new_property(
      class = class_logical,
      getter = function(self) {
        self@satisfies_average_theorem && self@can_throw
      }
    )
  ),
  validator = function(self) {
    if (!is_passing_notation(self@sequence)) {
      return("@sequence is not valid passing siteswap notation")
    }
    frac <- is_fractional_notation(self@sequence)
    seqs <- parse_passing_sequence(self@sequence, frac)
    lens <- lengths(seqs)
    if (frac && length(seqs) > 2L) {
      return("fractional notation only supports 2 jugglers")
    }
    if (length(unique(lens)) > 1L) {
      return("all jugglers must have the same number of throws")
    }
    if (any(lens == 0L)) {
      return("each juggler must have at least one throw")
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

method(print, passingSiteswap) <- function(x, ...) {
  if (x@valid) {
    cli::cli_bullets(c(
      "v" = "'{x@sequence}' is valid {x@type} siteswap",
      "i" = "It uses {x@n_props} props across {x@n_jugglers} jugglers",
      "i" = "It is {x@symmetry} with period {x@period}"
    ))
  } else {
    reasons <- c(
      if (!x@satisfies_average_theorem) {
        c("i" = "The throws don't average to a whole number")
      },
      if (!x@can_throw) {
        c("i" = "Two or more throws land on the same beat (collision)")
      }
    )
    cli::cli_bullets(c(
      "x" = "'{x@sequence}' is not a valid juggling pattern",
      reasons
    ))
  }
}

method(throw_data, passingSiteswap) <- function(siteswap, n_cycles = 3) {
  check_n_cycles(n_cycles)
  period <- siteswap@period
  n_jugglers <- siteswap@n_jugglers
  fractional <- siteswap@is_fractional
  total_beats <- period * n_cycles

  rows <- vector("list", total_beats * n_jugglers)
  k <- 0L

  for (cycle in seq_len(n_cycles)) {
    for (beat_in_period in seq_len(period)) {
      extended_beat <- (cycle - 1L) * period + beat_in_period
      hand <- (extended_beat - 1L) %% 2L
      for (j in seq_len(n_jugglers)) {
        h <- siteswap@throws_by_juggler[[j]][beat_in_period]
        is_p <- siteswap@is_pass_by_juggler[[j]][beat_in_period]

        if (fractional) {
          throw_combined <- (j - 1L) * 0.5 + (extended_beat - 1L)
          catch_combined <- round(throw_combined + h, 10L)
          frac_part <- catch_combined %% 1
          catch_juggler <- if (abs(frac_part) < 1e-9) 1L else 2L
          beat_out <- throw_combined + 1
          catch_beat <- catch_combined + 1
          catch_hand <- as.integer(floor(catch_combined) %% 2L)
        } else {
          beat_out <- extended_beat
          catch_beat <- extended_beat + as.integer(h)
          catch_juggler <- if (is_p) (j %% n_jugglers) + 1L else j
          catch_hand <- (extended_beat + h - 1L) %% 2L
        }

        k <- k + 1L
        rows[[k]] <- data.frame(
          beat = beat_out,
          juggler = j,
          hand = hand,
          throw = h,
          is_pass = is_p,
          catch_beat = catch_beat,
          catch_juggler = catch_juggler,
          catch_hand = as.integer(catch_hand),
          prop = NA_real_
        )
      }
    }
  }

  throws <- do.call(rbind, rows[seq_len(k)])

  throws$prop[1L] <- 1L
  next_prop <- 2L

  for (i in seq_len(nrow(throws))) {
    if (throws$throw[i] == 0) {
      next
    }

    current_prop <- throws$prop[i]
    if (is.na(current_prop)) {
      throws$prop[i] <- next_prop
      current_prop <- next_prop
      next_prop <- next_prop + 1L
    }

    idx <- which(
      throws$beat == throws$catch_beat[i] &
        throws$juggler == throws$catch_juggler[i] &
        is.na(throws$prop)
    )
    if (length(idx) > 0L) throws$prop[idx[1L]] <- current_prop
  }

  throws
}

method(timeline, passingSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE,
  subtitle = TRUE
) {
  td <- throw_data(siteswap, n_cycles = n_cycles)

  fractional <- siteswap@is_fractional
  max_throw <- max(td$throw, na.rm = TRUE)
  lane_gap <- max_throw + 2
  n_jugglers <- siteswap@n_jugglers
  max_prop <- max(td$prop, na.rm = TRUE)

  self_throws <- td |> filter(throw > 0, !is_pass)
  pass_throws <- td |> filter(throw > 0, is_pass)

  self_parabolas <- if (nrow(self_throws) > 0) {
    self_throws |>
      mutate(lane_baseline = (juggler - 1) * lane_gap) |>
      purrr::pmap(\(
        beat,
        catch_beat,
        throw,
        prop,
        juggler,
        lane_baseline,
        ...
      ) {
        p <- generate_parabola(beat, catch_beat, throw, prop, beat)
        p$y <- p$y + lane_baseline
        p$juggler <- juggler
        p
      }) |>
      purrr::list_rbind()
  } else {
    data.frame(
      x = numeric(0),
      y = numeric(0),
      prop = numeric(0),
      beat = integer(0),
      juggler = integer(0)
    )
  }

  pass_arcs <- if (nrow(pass_throws) > 0) {
    pass_throws |>
      mutate(
        y_from = (juggler - 1) * lane_gap,
        y_to = (catch_juggler - 1) * lane_gap
      ) |>
      purrr::pmap(\(beat, catch_beat, throw, prop, juggler, y_from, y_to, ...) {
        p <- generate_pass_arc(
          beat,
          y_from,
          catch_beat,
          y_to,
          throw,
          prop,
          beat
        )
        p$juggler <- juggler
        p
      }) |>
      purrr::list_rbind()
  } else {
    data.frame(
      x = numeric(0),
      y = numeric(0),
      prop = numeric(0),
      beat = integer(0),
      juggler = integer(0)
    )
  }

  all_curves <- purrr::list_rbind(list(self_parabolas, pass_arcs)) |>
    mutate(prop = factor(prop))

  period <- siteswap@period

  annot_data <- if (fractional) {
    data.frame(
      x = seq(1.5, period * n_cycles + 0.5, by = 1),
      y = lane_gap,
      label = rep(siteswap@sequences_by_juggler[[2L]], n_cycles)
    )
  } else if (n_jugglers > 1L) {
    purrr::list_rbind(lapply(seq(2L, n_jugglers), function(j) {
      data.frame(
        x = seq_len(period * n_cycles),
        y = (j - 1L) * lane_gap,
        label = rep(siteswap@sequences_by_juggler[[j]], n_cycles)
      )
    }))
  }

  warn_if_props_hidden(siteswap, max_prop)

  juggler_breaks <- (seq_len(n_jugglers) - 1) * lane_gap
  juggler_labels_vec <- paste0("J", seq_len(n_jugglers))

  p <- ggplot(
    all_curves,
    aes(
      x = x,
      y = y,
      group = interaction(beat, juggler, prop),
      color = prop
    )
  ) +
    geom_path(linewidth = 2, show.legend = FALSE) +
    {
      if (!is.null(annot_data)) {
        geom_text(
          data = annot_data,
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          vjust = 1.5,
          fontface = "bold",
          size = 6
        )
      }
    } +
    prop_color_scale(max_prop) +
    scale_x_continuous(
      breaks = seq_len(period * n_cycles),
      labels = rep(siteswap@sequences_by_juggler[[1L]], n_cycles)
    ) +
    scale_y_continuous(
      breaks = juggler_breaks,
      labels = juggler_labels_vec
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(face = "bold", size = rel(1.5)),
      axis.text.y = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 1,
        margin = margin(r = 8)
      ),
      plot.margin = margin(10, 20, 20, 20)
    ) +
    title_subtitle_theme() +
    labs(
      title = if (title) siteswap@sequence else NULL,
      subtitle = if (subtitle) plot_subtitle(siteswap) else NULL
    )

  p
}

method(ladder, passingSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  direction = c("horizontal", "vertical", "h", "v"),
  title = TRUE,
  subtitle = TRUE,
  hand_gap = 2L
) {
  direction <- rlang::arg_match(direction)
  direction <- if (direction %in% c("h", "horizontal")) {
    "horizontal"
  } else {
    "vertical"
  }

  plot_data <- throw_data(siteswap, n_cycles = n_cycles) |>
    mutate(is_even = is_even(throw)) |>
    filter(throw > 0)

  build_passing_ladder_plot(
    plot_data,
    direction,
    title = if (title) siteswap@sequence else NULL,
    siteswap@n_jugglers,
    hand_gap = hand_gap,
    subtitle = if (subtitle) plot_subtitle(siteswap) else NULL
  )
}
