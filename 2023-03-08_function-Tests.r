param <- list(
  x_point = c(3:10),
  y_point = c(2:5),
  df_exp = expand.grid(x = x_point, y = y_point)
)

sample_points <- function(x) {
  sample(x, 1, replace = TRUE)
}

make_dreieck2 <- function(param, origin = c(0, 0), label = "") {
  cat("A:  ", param$x_point, "\n")
  all_x_points <- param$x_point
  cat(all_x_points, "\n")
  all_y_points <- param$y_point
  cat(all_y_points, "\n")
  b <- c(sample_points(all_x_points), sample_points(all_y_points))
  c <- c(sample_points(all_x_points), sample_points(all_y_points))

  df <- tibble(
    a = origin,
    b = b,
    c = c
  ) |>
    t() |>
    data.frame() |>
    setNames(c("x", "y")) |>
    mutate(label = rep(label, 3))
  return(df)
}

make_dreieck2(param)
