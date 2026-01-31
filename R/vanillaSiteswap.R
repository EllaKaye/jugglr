# MAYBE: add orbits
#' @export
vanillaSiteswap <- new_class(
  "vanillaSiteswap",
  properties = list(
    type = new_property(
      class = class_character,
      setter = function(self, value) {
        if (!is.null(self@type)) {
          stop("@type is read-only", call. = FALSE)
        }
        self@type <- "vanilla"
        self
      }
    ),
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
        is_whole_number(mean(self@throws))
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

asynchronousSiteswap <- vanillaSiteswap
asyncSiteswap <- vanillaSiteswap

# MAYBE: Any other information to print about the pattern?
# MAYBE: Of not valid, say more about what the problem is,
# or suggest visualising with timeline (only vanilla?) or ladder
# MAYBE: print orbits (if calculating them)
# TODO: define print for Siteswap then use `super` and add an extra bullet
#' @export
method(print, vanillaSiteswap) <- function(x, ...) {
  if (x@valid) {
    cli::cli_bullets(
      c(
        "v" = "'{x@sequence}' is valid {x@type} siteswap",
        "i" = "It uses {x@n_props} props",
        "i" = "It is {x@symmetry} with period {x@period}"
      )
    )
  } else {
    cli::cli_bullets(
      c(
        "x" = "This siteswap is not a valid juggling pattern"
      )
    )
  }
}

method(throw_data, vanillaSiteswap) <- function(siteswap, n_cycles = 3) {
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

# TODO: Document that when n_props < x@period,
# will need to increase n_cycles to get a sense of the pattern/see all props being thrown
# Can say something like n_cycle likely to need increasing if period is short
# and/or there are more than three props
# Also document and give an example of modifying the plot with additional ggplot2 layers,
# such as palette.
# TODO: maybe no `title` arg, but document how to override it with `labs`
# TODO: still figurng out where to document this
method(timeline, vanillaSiteswap) <- function(
  siteswap,
  n_cycles = 3,
  title = TRUE
) {
  throw_data <- throw_data(siteswap, n_cycles = n_cycles)

  # TODO: check n_cycles is a single integer (or coercible)
  # What happens when n_cycles is a double, e.g. 1.3

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

  subtitle <- ifelse(
    siteswap@valid,
    paste("A valid juggling pattern with", siteswap@n_props, "props."),
    "Not a valid juggling pattern"
  )

  # generate warning if not all props are shown on plot
  # TODO: convert to warning with cli, not `stop`
  # TODO: separate into `check_all_props_shown(siteswap)` function
  # Will be useful elsewhere
  if (siteswap@valid && max_prop < siteswap@n_props) {
    #stop("not showing all props")
    cli::cli_warn(
      c(
        "!" = "There are {siteswap@n_props} props in siteswap '{siteswap@sequence}', but only {max_prop} {?is/are} shown.",
        "i" = "Increase {.arg n_cycles} to see more throws.",
        "i" = "Setting n_cycles >= {siteswap@period * siteswap@n_props * 2} will show each prop thrown at least twice."
      )
    )
  }

  p <- ggplot(
    parabolas,
    aes(x = x, y = y, group = beat, color = prop)
  ) +
    geom_path(linewidth = 2, show.legend = FALSE) +
    scale_x_continuous(
      breaks = 1:(siteswap@period * n_cycles),
      labels = rep(siteswap@throws, n_cycles)
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(face = "bold", size = rel(1.5)),
      plot.margin = margin(10, 20, 20, 20),
      plot.title = element_text(
        size = rel(1.8),
        margin = ggplot2::margin(12, 0, 8, 0)
      ),
      plot.subtitle = element_text(
        size = rel(1.2),
        margin = ggplot2::margin(0, 0, 8, 0)
      )
    )

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
  direction = c("horizontal", "vertical")
) {
  direction <- rlang::arg_match(direction)

  plot_data <- throw_data(siteswap, n_cycles = n_cycles) |>
    mutate(is_even = is_even(throw)) |>
    filter(throw > 0)

  # transform plot data based on direction
  if (direction == "vertical") {
    # having -beat means 0 is at the "top" of the ladder,
    # and we go down the negative y-axis as time increases
    plot_data <- plot_data |>
      mutate(
        x_start = hand,
        y_start = -beat,
        x_end = catch_hand,
        y_end = -catch_beat,
      )
  } else {
    plot_data <- plot_data |>
      mutate(
        x_start = beat,
        y_start = hand,
        x_end = catch_beat,
        y_end = catch_hand
      )
  }

  # Separate odd (straight) and even (curved) throws
  odd_throws <- plot_data |>
    dplyr::filter(!is_even)
  even_throws <- plot_data |>
    filter(is_even)

  # Create curve points for even heights - curves toward center (0.5)
  create_curve_points <- function(
    x_start,
    y_start,
    x_end,
    y_end,
    direction,
    prop_num,
    n_points = 50
  ) {
    t <- seq(0, 1, length.out = n_points)

    if (direction == "vertical") {
      x_control <- 0.5 # Curve toward center
      y_control <- mean(c(y_start, y_end))
    } else {
      x_control <- mean(c(x_start, x_end))
      y_control <- 0.5 # Curve toward center
    }

    #x_control <- mean(c(x_start, x_end))
    #y_control <- 0.5 # Curve toward center

    # Quadratic Bezier curve
    x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * x_control + t^2 * x_end
    y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * y_control + t^2 * y_end

    data.frame(x = x, y = y, prop = prop_num)
  }

  # Generate curve points for even throws
  if (nrow(even_throws) > 0) {
    curve_data <- even_throws |>
      dplyr::rowwise() |>
      do({
        curve_pts <- create_curve_points(
          .$x_start,
          .$y_start,
          .$x_end,
          .$y_end,
          direction,
          .$prop
        )
        curve_pts$group <- paste0("arc_", .$beat, "_", .$throw)
        curve_pts
      }) |>
      dplyr::ungroup()
  } else {
    curve_data <- data.frame(
      x = numeric(0),
      y = numeric(0),
      prop = numeric(0),
      group = character(0)
    )
  }

  #return(curve_data)

  p <- ggplot()

  # Add curved lines for even heights with colors by ball
  if (nrow(curve_data) > 0) {
    p <- p +
      geom_path(
        data = curve_data,
        aes(x = x, y = y, group = group, color = factor(prop)),
        show.legend = FALSE
      )
  }

  # Add straight lines for odd heights with colors by ball
  if (nrow(odd_throws) > 0) {
    p <- p +
      geom_segment(
        data = odd_throws,
        aes(
          x = x_start,
          y = y_start,
          xend = x_end,
          yend = y_end,
          color = factor(prop)
        ),
        show.legend = FALSE
      )
  }
  p
}
