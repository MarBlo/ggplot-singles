library(ggplot2)
library(dplyr)
library(tibble)

#| Segmented curves (radian) ----
angles1 <- seq(0, pi / 8, length.out = 10)

tibble(
  angles = angles1,
  x = cos(angles1),
  y = sin(angles1)
) |>
  ggplot() +
  geom_path(aes(x = x, y = y)) +
  coord_equal(ylim = c(0, 1), xlim = c(0, 1))


#| Make Cricle ----
# Define the circle; add a point at the center if the
# 'pie slice' if the shape is to be filled
circle_fun <- function(center = c(0, 0),
                       diameter = 1,
                       npoints = 100,
                       start = 0,
                       end = 2,
                       filled = TRUE) {
  tt <- seq(start * pi, end * pi, length.out = npoints)
  df <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  if (filled == TRUE) {
    # add a point at the center so the whole 'pie slice' is filled
    df <- rbind(df, center)
  }
  return(df)
}


full_flower <- function(no_segments = 16) {
  df_full_flower <- data.frame()
  for (n in seq_len(no_segments)) {
    print(n)
    df_full_flower <- bind_rows(
      df_full_flower,
      circle_fun(start = (n - 1) / 8, end = n / 8, diameter = 2) |>
        mutate(group = LETTERS[n])
    )
  }
  df_full_flower
}
ss <- full_flower()

ggplot() +
  geom_polygon(ss, mapping = aes(x, y, fill = group)) +
  coord_equal(ylim = c(-1, 1)) +
  guides(fill = "none", alpha = "none") +
  theme_void() +
  coord_equal()


ss |>
  filter(group == "A" | group == "B") |>
  ggplot(aes(x, y, fill = group, alpha = 0.2)) +
  geom_polygon() +
  guides(fill = "none", alpha = "none") +
  theme_void() +
  coord_equal()
