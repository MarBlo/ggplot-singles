library(ggplot2)
library(tibble)

angles <- seq(0, 2 * pi, length.out = 100)

circle <- tibble(
  x = cos(angles),
  y = sin(angles)
)

circles <- expand.grid(circle)

ggplot() +
  geom_point(data = circles, aes(x, y)) +
  geom_path(data = circle, aes(x, y), linewidth = 1) +
  geom_path() +
  theme_void() +
  coord_equal()
