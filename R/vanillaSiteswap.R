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
