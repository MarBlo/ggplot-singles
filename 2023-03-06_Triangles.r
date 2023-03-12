library(ggplot2)
library(dplyr)
library(tibble)
library(cowplot)
library(gridExtra)

source(here::here("2023-03-06_helper-Triangles.r"))
source(here::here("2023-03-06_worker-Triangles.r"))

param <- list(
  #' makes 24 plots each with several triangles
  #' angle between triangles
  with_facets = FALSE,
  degrees = 10,
  #' number of rotations
  rotation = 12,
  #' coordinates of points b and c
  x_point_b = c(4, 9),
  y_point_b = c(1, 6),
  x_point_c = c(1, 5),
  y_point_c = c(1, 5),
  #' should schwerpunkt be plotted; if set size
  mit_schwerpunkt = TRUE,
  size = 1,
  #' should pathline be made, if set linewidth
  mit_pathline = TRUE,
  linewidth = .2,
  #' should own defined colors be used
  own_col = TRUE,
  color_own = my_cols,
  #' settings for PrettyColors
  color_pal = "Reds",
  color_back = "black",
  #' alpha
  alpha = .5
)

#' MAIN FUNCTIONs ----
#'

##' grid_24_triangels ----
#'
#' @description
#'  * creates 24 plots in facets
#'  * each plots has randomly chosen number of triangles
#' @param param see above
#' @details
#'  * first call is to `make_triangles`
#'
grid_24_triangles(param)

##' triangles ----
#'
#' @description
#'  * creates on plot with any many triangles as given in param$rotation
#' @param param, b_point, c_point
#' @details
#'  * origing at (0,0), b_point & c_point must be given in fct call
#'  * calls plot_single
#'
triangles(b_point = c(9, 2), c_point = c(1, 5), param)
