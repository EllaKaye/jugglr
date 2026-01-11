library(ggplot2)
library(dplyr)

# Function to create ladder diagram data with ball tracking
create_ladder_data <- function(pattern, n_cycles = 2) {
  n <- length(pattern)
  total_throws <- n * n_cycles

  # Determine number of balls (average of pattern)
  n_balls <- mean(pattern)

  # Create data frame with throw information
  throws <- data.frame(
    throw_num = 1:total_throws,
    beat = 1:total_throws,
    hand = rep(c("R", "L"), length.out = total_throws),
    height = rep(pattern, length.out = total_throws)
  )

  # Calculate landing positions
  throws <- throws %>%
    mutate(
      catch_beat = beat + height,
      catch_hand = ifelse(height %% 2 == 0, hand, ifelse(hand == "R", "L", "R"))
    )

  # Track which ball is thrown at each beat
  # Initialize: balls 1, 2, 3, ... are in hands at beats 1, 2, 3, ...
  ball_at_beat <- rep(NA, max(throws$catch_beat))
  ball_at_beat[1:n_balls] <- 1:n_balls

  # Simulate the pattern to track balls
  for (i in seq_len(nrow(throws))) {
    throw_beat <- throws$beat[i]
    catch_beat <- throws$catch_beat[i]
    height <- throws$height[i]

    if (height > 0) {
      # Get the ball being thrown
      ball_num <- ball_at_beat[throw_beat]

      # Record this ball number in the throw
      throws$ball[i] <- ball_num

      # Place the ball at its landing beat
      if (catch_beat <= length(ball_at_beat)) {
        ball_at_beat[catch_beat] <- ball_num
      }
    } else {
      throws$ball[i] <- NA
    }
  }

  # Create line segments for ball trajectories with ball numbers
  arcs <- throws %>%
    filter(height > 0) %>%
    select(beat, hand, catch_beat, catch_hand, height, ball)

  list(throws = throws, arcs = arcs, n_balls = n_balls)
}

# Plot function with colored balls
plot_ladder <- function(ladder_data, direction = "vertical") {
  pattern_str <- paste(unique(ladder_data$throws$height), collapse = "")
  n_balls <- ladder_data$n_balls

  # Create a color palette for the balls
  ball_colors <- scales::hue_pal()(n_balls)

  # Create curve points for even heights - curves toward center (0.5)
  create_curve_points <- function(
    x_start,
    y_start,
    x_end,
    y_end,
    direction,
    ball_num,
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

    data.frame(x = x, y = y, ball = ball_num)
  }

  # Transform data based on direction
  if (direction == "vertical") {
    plot_data <- ladder_data$arcs %>%
      mutate(
        x_start = ifelse(hand == "L", 0, 1),
        y_start = -beat,
        x_end = ifelse(catch_hand == "L", 0, 1),
        y_end = -catch_beat,
        is_even = (height %% 2 == 0)
      )
  } else {
    plot_data <- ladder_data$arcs %>%
      mutate(
        x_start = beat,
        y_start = ifelse(hand == "L", 1, 0),
        x_end = catch_beat,
        y_end = ifelse(catch_hand == "L", 1, 0),
        is_even = (height %% 2 == 0)
      )
  }

  # Separate odd (straight) and even (curved) throws
  odd_throws <- plot_data %>% filter(!is_even)
  even_throws <- plot_data %>% filter(is_even)

  # Generate curve points for even throws
  if (nrow(even_throws) > 0) {
    curve_data <- even_throws %>%
      rowwise() %>%
      do({
        curve_pts <- create_curve_points(
          .$x_start,
          .$y_start,
          .$x_end,
          .$y_end,
          direction,
          .$ball
        )
        curve_pts$group <- paste0("arc_", .$beat, "_", .$height)
        curve_pts
      }) %>%
      ungroup()
  } else {
    curve_data <- data.frame(
      x = numeric(0),
      y = numeric(0),
      ball = numeric(0),
      group = character(0)
    )
  }

  p <- ggplot()

  # Add curved lines for even heights with colors by ball
  if (nrow(curve_data) > 0) {
    p <- p +
      geom_path(
        data = curve_data,
        aes(x = x, y = y, group = group, color = factor(ball)),
        linewidth = 0.8,
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
          color = factor(ball)
        ),
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  # Add color scale
  p <- p +
    scale_color_manual(
      values = setNames(ball_colors, 1:n_balls),
      guide = "none"
    )

  if (direction == "vertical") {
    # Create horizontal rungs between the two hand lines
    rung_beats <- -seq(1, max(-plot_data$y_end), by = 1)
    rung_data <- data.frame(y = rung_beats)

    p <- p +
      geom_segment(
        data = rung_data,
        aes(x = 0, y = y, xend = 1, yend = y),
        linewidth = 0.3,
        color = "grey80"
      ) +
      geom_segment(
        aes(x = 0, y = -1, xend = 0, yend = -max(-plot_data$y_end)),
        linewidth = 1
      ) +
      geom_segment(
        aes(x = 1, y = -1, xend = 1, yend = -max(-plot_data$y_end)),
        linewidth = 1
      ) +
      scale_y_continuous(
        breaks = NULL
      ) +
      scale_x_continuous(
        limits = c(-0.2, 1.2),
        breaks = NULL
      ) +
      coord_fixed(ratio = 0.3) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      labs(
        title = paste("Ladder Diagram: Siteswap", pattern_str),
        x = "",
        y = ""
      )
  } else {
    # Create vertical rungs between the two hand lines
    rung_beats <- 1:max(plot_data$x_end)
    rung_data <- data.frame(x = rung_beats)

    p <- p +
      geom_segment(
        data = rung_data,
        aes(x = x, y = 0, xend = x, yend = 1),
        linewidth = 0.3,
        color = "grey80"
      ) +
      geom_segment(
        aes(x = 1, y = 0, xend = max(plot_data$x_end), yend = 0),
        linewidth = 1
      ) +
      geom_segment(
        aes(x = 1, y = 1, xend = max(plot_data$x_end), yend = 1),
        linewidth = 1
      ) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(
        limits = c(-0.2, 1.2),
        breaks = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(
        title = paste("Ladder Diagram: Siteswap", pattern_str),
        x = "",
        y = ""
      )
  }

  p
}

# Test
ladder_data_531 <- create_ladder_data(c(5, 3, 1), n_cycles = 2)
plot_ladder(ladder_data_531, direction = "vertical")
plot_ladder(ladder_data_531, direction = "horizontal")

ladder_data_423 <- create_ladder_data(c(4, 2, 3), n_cycles = 3)
plot_ladder(ladder_data_423, direction = "vertical")
plot_ladder(ladder_data_423, direction = "horizontal")

ladder_data_441 <- create_ladder_data(c(4, 4, 1), n_cycles = 3)
plot_ladder(ladder_data_441, direction = "vertical")
plot_ladder(ladder_data_441, direction = "horizontal")

ladder_data_55550 <- create_ladder_data(c(5, 5, 5, 5, 0), n_cycles = 3)
plot_ladder(ladder_data_55550, direction = "vertical")
plot_ladder(ladder_data_55550, direction = "horizontal")

# Not valid pattern, but should still be able to plot
ladder_data_21 <- create_ladder_data(c(2, 1), n_cycles = 3)
plot_ladder(ladder_data_21, direction = "vertical")
plot_ladder(ladder_data_21, direction = "horizontal")

# geom_curve and geom_segment
df <- data.frame(
  x1 = c(1, 3, 4),
  x2 = c(2, 5, 6),
  y1 = c(0, 0, 1),
  y2 = c(0, 1, 1),
  curvature = c(-0.5, 0, 0.5),
  ball = factor(1:3)
)

df1 <- filter(df, curvature == -0.5)
df2 <- filter(df, curvature == 0)
df3 <- filter(df, curvature == 0.5)

b <- ggplot(
  df,
  aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2,
    colour = ball,
  )
) +
  geom_curve(,
    data = df1,
    curvature = -0.5
  ) +
  geom_curve(,
    data = df2,
    curvature = 0
  ) +
  geom_curve(,
    data = df3,
    curvature = +0.5
  )
b +
  scale_color_discrete() +
  #coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Siteswap") +
  theme_minimal() +
  # theme_void() +
  theme(panel.grid.major.x = element_line(colour = "grey90"))

# throws423 <- throw_data(siteswap("423"))
# throws4413 <- throw_data(siteswap("4413"))

generate_parabola <- function(x1, x2, height, ball, beat, n_points = 100) {
  vx <- (x1 + x2) / 2 # vertex
  xs <- seq(x1, x2, length.out = n_points)
  ys <- height * (1 - ((xs - vx) / (vx - x1))^2)

  data.frame(x = xs, y = ys, ball = ball, beat = beat)
}

# all_parabolas_423 <- throws423 |>
#   purrr::pmap(~ generate_parabola(..1, ..4, ..3, ..6, ..1)) |>
#   purrr::list_rbind()

# ggplot(
#   all_parabolas_423,
#   aes(x = x, y = y, group = beat, color = ball)
# ) +
#   geom_path() +
#   theme_minimal()

# all_parabolas_4413 <- throws4413 |>
#   purrr::pmap(~ generate_parabola(..1, ..4, ..3, ..6, ..1)) |>
#   purrr:::list_rbind()

# ggplot(
#   all_parabolas_4413,
#   aes(x = x, y = y, group = beat, color = ball)
# ) +
#   geom_path() +
#   theme_minimal()

# ss423 <- siteswap("423")
# throws423 <- throw_data(ss423)

# full_parabolas <- throws423 |>
#   select(beat, catch_beat, throw, ball) |>
#   purrr::pmap(\(beat, catch_beat, throw, ball) {
#     generate_parabola(beat, catch_beat, throw, ball, beat)
#   }) |>
#   purrr::list_rbind()

# # TODO: need to get `n_cycles` programmatically
# last_throws_423 <- throw_data(ss423, n = 1) |>
#   mutate(beat = beat + 9, catch_beat = catch_beat + 9)

# # TODO: need to get n programmatically
# half_parabolas <- last_throws_423 |>
#   select(beat, catch_beat, throw, ball) |>
#   purrr::pmap(\(beat, catch_beat, throw, ball) {
#     generate_parabola(beat, catch_beat, throw, ball, beat)
#   }) |>
#   purrr::list_rbind() |>
#   group_by(beat) |>
#   slice_head(n = 50)

# all_parabolas <- bind_rows(full_parabolas, half_parabolas)

# p <- ggplot(
#   all_parabolas,
#   aes(x = x, y = y, group = beat, color = ball)
# ) +
#   geom_path() +
#   theme_minimal()

# p
