#' @export
vanillaSiteswap <- S7::new_class(
  "vanillaSiteswap",
  properties = list(
    type = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        if (!is.null(self@type)) {
          stop("@type is read-only", call. = FALSE)
        }
        self@type <- "vanilla"
        self
      }
    ),
    throws = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        get_throws(self@sequence)
      }
    ),
    period = S7::new_property(
      class = S7::class_integer,
      getter = function(self) {
        length(self@throws)
      }
    ),
    symmetry = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        ifelse(is_even(self@period), "asymmetrical", "symmetrical")
      }
    ),
    n_props = S7::new_property(
      class = S7::class_numeric,
      getter = function(self) {
        mean(self@throws)
      }
    ),
    can_throw = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        can_throw(self@throws)
      }
    ),
    satisfies_average_theorem = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        is_whole_number(mean(self@throws))
      }
    ),
    valid = S7::new_property(
      class = S7::class_logical,
      getter = function(self) {
        self@satisfies_average_theorem && self@can_throw
      }
    )
  ),
  validator = function(self) {
    if (!(str_detect(self@sequence, "^[a-wA-W0-9]+$"))) {
      "@sequence must only contain digits and letters a-w"
    }
  },
  parent = Siteswap,
  package = "jugglr"
)

asynchronousSiteswap <- vanillaSiteswap
asyncSiteswap <- vanillaSiteswap

# MAYBE: Any other information to print about the pattern?
#' @export
S7::method(print, vanillaSiteswap) <- function(x, ...) {
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

S7::method(throw_data, vanillaSiteswap) <- function(x, n_cycles = 3) {
  total_throws <- x@period * n_cycles

  throws <- data.frame(
    beat = 1:total_throws,
    hand = rep(0:1, length.out = total_throws), # 0,1 rather than R/L or L/R
    throw = rep(x@throws, length.out = total_throws)
  )

  throws <- throws |>
    mutate(
      catch_beat = beat + throw,
      catch_hand = ifelse(is_even(throw), hand, 1 - hand),
      ball = NA
    )

  # track which ball is thrown at each beat
  # initialise: balls 1, ..., n_props are first thrown at beats 1, ..., n_props
  throws$ball[1:x@n_props] <- 1:x@n_props

  # simulate the pattern to track balls
  for (i in seq_len(nrow(throws))) {
    catch_beat <- throws$catch_beat[i]
    throw <- throws$throw[i]
    ball <- throws$ball[i]

    if (throw > 0 && catch_beat <= nrow(throws)) {
      throws$ball[catch_beat] <- ball # place ball when it's caught
    }
  }

  throws <- throws |>
    mutate(ball = factor(ball))

  throws
}

#' @export
S7::method(timeline, vanillaSiteswap) <- function(x, n_cycles = 3) {
  throw_data <- throw_data(x)

  parabolas <- throw_data |>
    select(beat, catch_beat, throw, ball) |>
    purrr::pmap(\(beat, catch_beat, throw, ball) {
      generate_parabola(beat, catch_beat, throw, ball, beat)
    }) |>
    purrr::list_rbind()

  p <- ggplot(
    parabolas,
    aes(x = x, y = y, group = beat, color = ball)
  ) +
    geom_path() +
    theme_minimal()

  # p <- ggplot2::ggplot(
  #   throw_data,
  #   ggplot2::aes(
  #     x = beat,
  #     y = 0,
  #     xend = catch_beat,
  #     yend = 0,
  #     colour = ball
  #   )
  # ) +
  #   ggplot2::geom_curve(curvature = -1)

  p
}
