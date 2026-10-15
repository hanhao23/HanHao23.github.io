install.packages("gganimate")
install.packages(c("gifski", "png"))

library(ggplot2)
library(gganimate)
library(dplyr)

set.seed(123)

n_flakes <- 100
n_frames <- 100

# Random snowflake initial positions and speeds
snowflakes <- data.frame(
  flake_id = 1:n_flakes,
  x0 = runif(n_flakes, 0, 1),
  y0 = runif(n_flakes, 0, 1),
  speed = runif(n_flakes, min = 0.002, max = 0.01)
)

# Create a data frame that has each flake's position across all time steps
animated_data <- expand.grid(
  flake_id = 1:n_flakes,
  time = 1:n_frames
) %>%
  left_join(snowflakes, by = "flake_id") %>%
  rowwise() %>%
  mutate(
    y = (y0 - speed * time) %% 1,
    x = x0
  ) %>%
  ungroup()


# A little helper function to create a circle (polygon) given center & radius
circleFun <- function(center = c(0, 0), radius = 1, npoints = 100) {
  theta <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center[1] + radius * cos(theta),
    y = center[2] + radius * sin(theta)
  )
}

# Coordinates for each "ball" of the snowman
# Adjust centers & radii for size/placement
snowman_bottom <- circleFun(center = c(0.8, 0.2), radius = 0.1, npoints = 100)
snowman_middle <- circleFun(center = c(0.8, 0.35), radius = 0.07, npoints = 100)
snowman_head   <- circleFun(center = c(0.8, 0.45), radius = 0.05, npoints = 100)

# Eyes as small black points
eyes <- data.frame(
  x = c(0.79, 0.81),
  y = c(0.47, 0.47)
)

# Nose as a small orange polygon (triangle)
nose <- data.frame(
  x = c(0.80, 0.85, 0.80),
  y = c(0.46, 0.45, 0.44)
)

# Optional stick arms (using geom_segment)
# They attach near the middle “ball” and extend outward
arms <- data.frame(
  x    = c(0.75, 0.85),
  xend = c(0.68, 0.92),
  y    = c(0.36, 0.36),
  yend = c(0.42, 0.42)
)


p <- ggplot(animated_data, aes(x = x, y = y, group = flake_id)) +
  # Snowflakes as small white points
  geom_point(color = "white", shape = 16, size = 2, alpha = 0.8) +
  
  # Add the snowman layers (static, not animated)
  # 1) Bottom ball
  geom_polygon(
    data = snowman_bottom, aes(x, y),
    fill = "white", color = "black", inherit.aes = FALSE
  ) +
  # 2) Middle ball
  geom_polygon(
    data = snowman_middle, aes(x, y),
    fill = "white", color = "black", inherit.aes = FALSE
  ) +
  # 3) Head
  geom_polygon(
    data = snowman_head, aes(x, y),
    fill = "white", color = "black", inherit.aes = FALSE
  ) +
  # Eyes
  geom_point(
    data = eyes, aes(x, y),
    color = "black", size = 2, inherit.aes = FALSE
  ) +
  # Nose
  geom_polygon(
    data = nose, aes(x, y),
    fill = "orange", color = NA, inherit.aes = FALSE
  ) +
  # Arms (if you want them)
  geom_segment(
    data = arms, aes(x = x, y = y, xend = xend, yend = yend),
    color = "brown", size = 3, inherit.aes = FALSE
  ) +
  
  # Night sky theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "deepskyblue3", color = "deepskyblue3"),
    panel.background = element_rect(fill = "deepskyblue3", color = "deepskyblue3"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, color = "gold2")
  ) +
  
  labs(title = "Merry Christmas & Happy Holidays!") +
  
  # Animate over 'time'
  transition_time(time) +
  enter_fade() + 
  exit_fade()

# If you want to see it in RStudio, just call:
# animate(p, nframes = n_frames, fps = 10, width = 400, height = 400)

# To save as a GIF (need gifski or similar installed):
animate(
  p,
  nframes = n_frames,
  fps = 10,
  width = 400,
  height = 400,
  renderer = gifski_renderer("snowfall_snowman.gif")
)
