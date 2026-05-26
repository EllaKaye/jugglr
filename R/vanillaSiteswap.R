#' @include siteswap.R
#' @include utils.R
#' @include utils-plotting.R
NULL

# MAYBE: add orbits
#' Vanilla siteswap
#'
#' Creates a vanilla siteswap object from an alphanumeric siteswap sequence
#' such as `"531"` or `"97531"`. Vanilla siteswap describes solo juggling
#' patterns where one prop is thrown per beat, alternating hands.
#'
#' @param sequence A single character string of vanilla siteswap notation
#'   (digits and letters only, e.g. `"531"`).
#'
#' @returns A `vanillaSiteswap` S7 object.
#'
#' @prop type Always `"vanilla"` (read-only).
#' @prop throws Integer vector of throw heights for one cycle.
#' @prop period Number of throws per cycle (length of `throws`).
#' @prop symmetry `"symmetrical"` when period is odd (pattern repeats with
#'   swapped hands); `"asymmetrical"` when period is even.
#' @prop n_props Mean throw height, equal to the number of props required.
#' @prop can_throw `TRUE` if no two throws land on the same beat (no
#'   collisions).
#' @prop satisfies_average_theorem `TRUE` if `n_props` is a whole number.
#' @prop valid `TRUE` if both `can_throw` and `satisfies_average_theorem` are
#'   `TRUE`.
#'
#' @export
#'
#' @examples
#' vanillaSiteswap("531")
#' vanillaSiteswap("97531")
#'
#' s <- vanillaSiteswap("531")
#' s@n_props
#' s@valid
vanillaSiteswap <- new_class(
  "vanillaSiteswap",
  properties = list(
    type = new_property(class = class_character, getter = function(self) {
      "vanilla"
    }),
    throws = new_property(
      class = class_integer,
      getter = function(self) {
        get_throws(self@sequence)
      }
    ),
    period = new_property(
      class = class_integer,
      getter = function(self) {
        length(self@throws)
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
        mean(self@throws)
      }
    ),
    can_throw = new_property(
      class = class_logical,
      getter = function(self) {
        can_throw(self@throws)
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
    if (!(str_detect(self@sequence, "^[a-zA-Z0-9]+$"))) {
      "@sequence must only contain digits and letters"
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

# MAYBE: add orbits to print output (depends on orbits feature)

method(throw_data, vanillaSiteswap) <- function(siteswap, n_cycles = 3) {
  check_n_cycles(n_cycles)
  total_throws <- siteswap@period * n_cycles

  throws <- data.frame(
    beat = 1:total_throws,
    hand = rep(0:1, length.out = total_throws), # 0,1 rather than R/L or L/R
    throw = rep(siteswap@throws, length.out = total_throws)
  )

  throws <- throws |>
    mutate(
      catch_beat = beat + throw,
      catch_hand = ifelse(is_even(throw), hand, 1 - hand),
      prop = NA
    )

  # track which ball is thrown at each beat
  # initialise
  throws$prop[1] <- 1
  next_prop <- 2

  # simulate the pattern to track balls
  for (i in seq_len(nrow(throws))) {
    throw <- throws$throw[i]

    # Skip beats with no throw (0)
    if (throw == 0) {
      next
    }

    current_prop <- throws$prop[i]

    # If no ball assigned yet, assign the next available ball
    if (is.na(current_prop)) {
      throws$prop[i] <- next_prop
      current_prop <- next_prop
      next_prop <- next_prop + 1
    }

    catch_beat <- throws$catch_beat[i]

    # Place the ball at its catch beat
    if (catch_beat <= nrow(throws)) {
      throws$prop[catch_beat] <- current_prop
    }
  }

  throws
}

method(timeline, vanillaSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE
) {
  throw_data <- throw_data(siteswap, n_cycles = n_cycles)

  max_prop <- max(throw_data$prop, na.rm = TRUE)

  throw_data <- throw_data |>
    mutate(prop = factor(prop))

  parabolas <- throw_data |>
    filter(throw > 0) |> # nothing to draw when there's no throw
    select(beat, catch_beat, throw, prop) |>
    purrr::pmap(\(beat, catch_beat, throw, prop) {
      generate_parabola(beat, catch_beat, throw, prop, beat)
    }) |>
    purrr::list_rbind()

  subtitle <- plot_subtitle(siteswap)

  warn_if_props_hidden(siteswap, max_prop)

  p <- ggplot(
    parabolas,
    aes(x = x, y = y, group = beat, color = prop)
  ) +
    geom_path(linewidth = 2, show.legend = FALSE) +
    prop_color_scale(max_prop) +
    scale_x_continuous(
      breaks = 1:(siteswap@period * n_cycles),
      labels = rep(siteswap@throws, n_cycles)
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(face = "bold", size = rel(1.5)),
      plot.margin = margin(10, 20, 20, 20)
    ) +
    title_subtitle_theme()

  if (title) {
    p <- p +
      labs(
        title = paste0("Siteswap '", siteswap@sequence, "'"),
        subtitle = subtitle
      )
  }

  p
}

method(ladder, vanillaSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  direction = c("horizontal", "vertical"),
  subtitle = TRUE
) {
  direction <- rlang::arg_match(direction)

  plot_data <- throw_data(siteswap, n_cycles = n_cycles) |>
    mutate(is_even = is_even(throw)) |>
    filter(throw > 0)

  build_ladder_plot(
    plot_data,
    direction,
    paste0("Siteswap '", siteswap@sequence, "'"),
    subtitle = if (subtitle) plot_subtitle(siteswap) else NULL
  )
}
