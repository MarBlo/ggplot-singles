#| Worker Functions  ----

#' make_dreieck
#' erstellt DF für Dreiecke
#'
#' @param origin of triangle
#' @param b second point
#' @param c third point
#' @return tibble with coordinates
#'

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

#' drehen_dreieck
#' Turns Triangle around origin
#'
#' @param data_f DF with coordinates
#' @param winkel Truning angle
#' @param label bb
#' @return  new DF with coordinates
#'

drehen_dreieck <- function(data_f, param, winkel, label = "") {
  # winkel <- param$degrees
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

#' schwerpunkt
#' calculates schwerpunkt Triangle
#'
#' @param data_f coordinates of Triangle
#' @return DF with coordinates of schwerpunkt
#'

schwerpunkt <- function(data_f) {
  df <- data_f |>
    group_by(label, rw) |>
    mutate(
      mp_x = 1 / 3 * sum(x),
      mp_y = 1 / 3 * sum(y)
    ) |>
    slice(1) |>
    select(mp_x, mp_y, label)
  return(df)
}

#' make_triangles
#'
#' @description
#'  * random samples for making corner points of triangle
#'  * checks if coordinates of point b is the sames as point c
#'  * if so, continues sampling
#'  * in second part calls `make_dreieck` for makeing first triangle
#'  * calls then `drehen_dreieck` as many times as defined in
#'  * main function `grid_triangles()` and binds DF together
#'
#' @param param from main call function
#' @return DF ready for plotting
#' @details
#'  * calls make_dreieck
#'  * calls drehen_dreieck
#'  * get called by main function `grid_24_triangles()`
#'  * number of triangles per plot is randomly sampled => `rot`
#'

make_triangles <- function(param) {
  all_x_points_b <- param$x_point_b
  all_y_points_b <- param$y_point_b
  all_x_points_c <- param$x_point_c
  all_y_points_c <- param$y_point_c
  b <- c(sample_points(all_x_points_b), sample_points(all_y_points_b))
  c <- c(sample_points(all_x_points_c), sample_points(all_y_points_c))
  b_equal_c <- b[1] == c[1] & b[2] == c[2]
  print(b_equal_c)
  print(b)
  print(c)
  count <- 1
  while (b_equal_c) {
    count <- count + 1
    print("--------")
    print(count)
    print("inside")
    b <- c(sample_points(all_x_points_b), sample_points(all_y_points_b))
    c <- c(sample_points(all_x_points_c), sample_points(all_y_points_c))
    b_equal_c <- b[1] == c[1] & b[2] == c[2]
    print(b_equal_c)
    print("--------")
  }

  f <- make_dreieck(b = b, c = c, label = "1")
  tt <- NULL
  f_0 <- f
  rot <- sample(1:20, 1)
  for (i in seq(rot)) {
    lab <- i + 1
    tt <- drehen_dreieck(
      f_0, param,
      winkel = i * param$degrees, label = as.character(lab)
    )
    f <- bind_rows(f, tt[4:6, ])
  }

  return(f)
}

#' plot_f
#'
#' @description
#'  * plots as many plots as defined in main function `grid_triangle()`
#'  * (default = 24) and makes facet
#'
#' @param param
#' @returns ggplot object
#' @details
#'  * get called by main function `grid_triangle()`
#'  * creates some `geom_`s if boolean is set TRUE in @param
#'  * PrettyColors is an option
#'  * calls `mit_schwerpunkt()` if TRUE in @param
#'  * calls `mit_pathline()` if TRUE in @param
#'

plot_f <- function(f, param) {
  if (param$mit_schwerpunkt) {
    sp <- schwerpunkt(f)
  }
  if (param$mit_pathline) {
    ff_path <- f |>
      group_by(label, rw) |>
      group_modify(~ add_row(
        x = 0,
        y = 0,
        .x
      )) |>
      mutate(rw = ifelse(is.na(rw), lag(rw), rw))
  }
  lims <- ceiling(max(max(f$x), max(f$y)))
  #' change to numeric factor for making colors in sequence
  f$label <- as.factor(as.numeric(f$label))
  drei_e <- ggplot(f) +
    geom_polygon(aes(x = x, y = y, fill = label), alpha = param$alpha) +
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
          size = param$size / 5
        )
      }
    } +
    scale_fill_manual(values = get_palette(param$rotation + 8)) +
    facet_wrap(~rw, ncol = 6) +
    coord_equal(xlim = c(-lims, lims), ylim = c(-lims, lims)) +
    theme_void() +
    guides(color = FALSE, fill = FALSE) +
    theme(
      plot.background = element_blank(),
      panel.background = element_rect(
        fill = param$color_back, color = NA
      ),
      plot.margin = margin(0, 0, 0, 0, unit = "cm"),
      strip.text = element_blank(),
      panel.spacing = unit(.1, "lines")
    )

  return(drei_e)
}

#' plot_single
#'
#' @description
#'  * plots one plot with as many triangles as defined
#'      param$rotation
#'
#' @param param, dataframe
#' @returns ggplot object
#' @details
#'  * get called by main function `triangle()`
#'  * creates some `geom_`s if boolean is set TRUE in @param
#'  * PrettyColors is an option
#'  * calls `mit_schwerpunkt()` if TRUE in @param
#'  * calls `mit_pathline()` if TRUE in @param
#'
plot_single <- function(f, param) {
  if (param$mit_schwerpunkt) {
    sp <- schwerpunkt(f)
  }
  if (param$mit_pathline) {
    ff_path <- f |>
      group_by(label, rw) |>
      group_modify(~ add_row(
        x = 0,
        y = 0,
        .x
      )) |>
      mutate(rw = ifelse(is.na(rw), lag(rw), rw))
  }

  eins_e <- ggplot(f) +
    geom_polygon(aes(x = x, y = y, fill = factor(rw)),
      alpha = param$alpha
    ) +
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
          data = sp, aes(x = mp_x, y = mp_y, color = "green"),
          alpha = 1, size = param$size
        )
      }
    } +
    scale_fill_manual(values = get_palette(param$rotation + 3)) +
    coord_equal(xlim = c(-10, 10), ylim = c(-10, 10)) +
    theme_void() +
    guides(color = FALSE, fill = FALSE) +
    theme(
      plot.background = element_blank(),
      panel.background = element_rect(
        fill = param$color_back, color = NA
      ),
      plot.margin = margin(1, 1, 1, 1, unit = "cm")
    )

  return(list(f, eins_e))
}

#| End functions ----

#' grid_24_triangles
#'
#' @description
#'  * creates a list of 24 DF, each contains one set of
#'      triangles
#'  * This list is used to make a DF
#' @param param
#' @return ggplot with 24 facets or as single plot
#' @details
#'  * calls `make_triangles`
#'    * which calls `make_dreieck`
#'    * which calls `drehen_dreieck`
#'  * calls plot_f
#'

grid_24_triangles <- function(param) {
  plot_lst <- vector("list", length = 8)

  for (i in 1:24) {
    g <- make_triangles(param) |>
      mutate(rw = LETTERS[i])
    plot_lst[[i]] <- g
  }

  df <- bind_rows(plot_lst)
  print(plot_f(df, param))
  return(df)
}

#' triangles
#'
#' @description
#'  * creates one single plots, no triangles given in param$rotation
#' @param param, b_point, c_point (origin given at (0,0))
#' @details
#'  * starts with initial triangle
#'  * calls `drehen_dreieck()` as given in param$rotation
#'  * calls `single_plot()`
#' @return ggplot
#'
triangles <- function(b_point, c_point, param) {
  f <- tibble(
    x = c(0, b_point[1], c_point[1]),
    y = c(0, b_point[2], c_point[2]),
    label = c("1", "1", "1"),
    rn = letters[1:3]
  ) |> column_to_rownames("rn")

  tt <- NULL
  f_0 <- f
  rot <- param$rotation
  for (i in seq(rot)) {
    lab <- i + 1
    tt <- drehen_dreieck(
      f_0, param,
      winkel = i * param$degrees, label = as.character(lab)
    )
    f <- bind_rows(f, tt[4:6, ])
  }

  ff <- f |>
    group_by(label) |>
    group_modify(~ add_row(
      x = 0,
      y = 0,
      .x
    )) |>
    mutate(rw = c(LETTERS, letters)[as.numeric(label)])

  print(plot_single(ff, param))
  # return(ff)
}




#' Experimental ----
##' make animation ----

#' triangles(
#'  b_point, c_point, winkel, rotation,
#'  mit_schwerpunkt, mit_pathline, alpha,
#' linewidth, size, degrees, color_back
#' )

#' drehen_dreieck(
#'  f_0, winkel,
#' )
#'
#' plot_single(
#'  f, mit_schwerpunkt, mit_pathline, alpha, linewidth, size, color_back
#' )
