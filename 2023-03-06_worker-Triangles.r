#| data_frame for every new triangle ---

make_dreieck <- function(origin = c(0, 0), b, c, label) {
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


## | Dreieck drehen          ----
## | Drehen um den Nullpunkt ----

drehen_dreieck <- function(data_f, winkel = 10, label = "") {
  dreh_winkel <- winkel # in degree
  if (nrow(data_f) > 3) {
    df_m <- as.matrix(data_f[4:6, -3])
  } else {
    df_m <- as.matrix(data_f[, -3])
  }

  rownames(df_m) <- stringr::str_extract(rownames(df_m), "[a-z]+")
  null_winkel_b <- degree(angle(t(c(1, 0)), df_m["b", ]))
  null_winkel_c <- degree(angle(t(c(1, 0)), df_m["c", ]))
  winkel_b <- null_winkel_b + dreh_winkel
  winkel_c <- null_winkel_c + dreh_winkel
  len_b <- sqrt(sum(df_m["b", ]^2))
  len_c <- sqrt(sum(df_m["c", ]^2))
  neue_b_ecke <- c(
    len_b * cos(radian(winkel_b)),
    len_b * sin(radian(winkel_b))
  )
  neue_c_ecke <- c(
    len_c * cos(radian(winkel_c)),
    len_c * sin(radian(winkel_c))
  )
  df_neu <- tibble(
    a = c(0, 0),
    b = neue_b_ecke,
    c = neue_c_ecke
  ) |>
    t() |>
    data.frame() |>
    setNames(c("x", "y")) |>
    mutate(label = rep(label, 3))

  df <- bind_rows(data_f, df_neu)
  return(df)
}

#| add Schwerpunkt
schwerpunkt <- function(data_f) {
  df <- data_f |>
    group_by(label) |>
    mutate(
      mp_x = 1 / 3 * sum(x),
      mp_y = 1 / 3 * sum(y)
    ) |>
    slice(1) |>
    select(mp_x, mp_y, label)
  return(df)
}

#| add Seitenhalbierende
seitenhalbierende <- function(data_f) {
  #| zum plotten der seitenhalbierende mit geom_point
  #| werden die Variablen x_2 und y_2 genommen
  df <- data_f |>
    mutate(x_2 = 0.5 * x, y_2 = 0.5 * y) |>
    group_by(label) |>
    group_modify(~ add_row(
      x = NA,
      y = NA,
      x_2 = .x$x[2] + 0.5 * (.x$x[3] - .x$x[2]),
      y_2 = .x$y[2] + 0.5 * (.x$y[3] - .x$y[2]),
      .x
    )) |>
    select(c(label, x_2, y_2)) |>
    filter(x_2 > 0)
  return(df)
}


#| Make Triangles ----

make_triangles <- function(param) {
  all_x_points <- param$x_point
  all_y_points <- param$y_point
  b <- c(sample_points(all_x_points), sample_points(all_y_points))
  c <- c(sample_points(all_x_points), sample_points(all_y_points))

  f <- make_dreieck(b = b, c = c, label = "1")
  tt <- NULL
  f_0 <- f
  for (i in seq(, param$rotation)) {
    lab <- i + 1
    tt <- drehen_dreieck(
      f_0,
      winkel = i * param$degrees, label = as.character(lab)
    )
    f <- bind_rows(f, tt[4:6, ])
  }

  plot_f(f, param)
}

#| Plot ---
plot_f <- function(f, param) {
  if (param$mit_schwerpunkt) {
    sp <- schwerpunkt(f)
    print(sp)
  }
  if (param$mit_pathline) {
    ff_path <- f |>
      group_by(label) |>
      group_modify(~ add_row(
        x = 0,
        y = 0,
        .x
      ))
  }

  drei_e <- ggplot(f) +
    geom_polygon(aes(x = x, y = y, fill = label), alpha = .2) +
    { # nolint: brace_linter.
      if (param$mit_pathline) {
        geom_path(data = ff_path, aes(
          x = x, y = y, group = label
        ), color = "white", linewidth = param$linewidth)
      }
    } +
    { # nolint: brace_linter.
      if (param$mit_schwerpunkt) {
        geom_point(
          data = sp, aes(x = mp_x, y = mp_y, color = label),
          size = param$size
        )
      }
    } +
    coord_equal(xlim = c(-10, 10), ylim = c(-10, 10)) +
    theme_void() +
    guides(color = FALSE, fill = FALSE) +
    theme(plot.background = element_rect(fill = "white"))

  return(drei_e)
}
