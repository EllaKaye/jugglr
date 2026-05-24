generate_parabola <- function(x1, x2, height, prop, beat, n_points = 100) {
  vx <- (x1 + x2) / 2 # vertex
  xs <- seq(x1, x2, length.out = n_points)
  ys <- height * (1 - ((xs - vx) / (vx - x1))^2)

  data.frame(x = xs, y = ys, prop = prop, beat = beat)
}

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

  # Quadratic Bezier curve
  x <- (1 - t)^2 * x_start + 2 * (1 - t) * t * x_control + t^2 * x_end
  y <- (1 - t)^2 * y_start + 2 * (1 - t) * t * y_control + t^2 * y_end

  data.frame(x = x, y = y, prop = prop_num)
}

build_ladder_plot <- function(plot_data, direction, title) {
  is_vertical <- direction == "vertical"

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

  # Separate straight (odd/crossing) and curved (even/same-hand) throws
  odd_throws <- plot_data |>
    filter(!is_even)
  even_throws <- plot_data |>
    filter(is_even)

  # Generate curve points for even throws
  if (nrow(even_throws) > 0) {
    curve_data <- even_throws |>
      purrr::pmap(\(
        x_start,
        y_start,
        x_end,
        y_end,
        beat,
        hand,
        throw,
        prop,
        ...
      ) {
        curve_pts <- create_curve_points(
          x_start,
          y_start,
          x_end,
          y_end,
          direction,
          prop
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

  if (nrow(odd_throws) > 0) {
    p <- p +
      geom_segment(
        data = odd_throws,
        aes(
          x = .data$x_start,
          y = .data$y_start,
          xend = .data$x_end,
          yend = .data$y_end,
          color = factor(.data$prop)
        ),
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  if (is_vertical) {
    n_beats <- max(-plot_data$y_end)
    beats <- -seq_len(n_beats)
    rung_data <- data.frame(
      x = 0,
      y = beats,
      xend = 1,
      yend = beats
    )
    rail_data <- data.frame(
      x = c(0, 1),
      y = c(-1, -1),
      xend = c(0, 1),
      yend = c(-n_beats, -n_beats)
    )
  } else {
    n_beats <- max(plot_data$x_end)
    beats <- seq_len(n_beats)
    rung_data <- data.frame(
      x = beats,
      y = 0,
      xend = beats,
      yend = 1
    )
    rail_data <- data.frame(
      x = c(1, 1),
      y = c(0, 1),
      xend = c(n_beats, n_beats),
      yend = c(0, 1)
    )
  }

  # hand_axis gets limits; time_axis gets no limits
  hand_limits <- c(-0.2, 1.2)
  ratio <- ifelse(is_vertical, 0.3, 3)

  p <- p +
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
    labs(
      title = title,
      x = "",
      y = ""
    )

  p
}
