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
  degrees = 10,
  #' coordinates of points b and c
  x_point_b = c(4, 9),
  y_point_b = c(1, 6),
  x_point_c = c(1, 5),
  y_point_c = c(1, 5),
  #' should schwerpunkt be plotted; if set size
  mit_schwerpunkt = FALSE,
  size = 2,
  #' should pathline be made, if set linewidth
  mit_pathline = TRUE,
  linewidth = .2,
  #' should own defined colors be used
  own_col = TRUE,
  color_own = my_cols,
  #' settings for PrettyColors
  color_pal = "Blues",
  color_back = "darkgray"
)


#' MAIN FUNCTION
#'
#' @description
#'  * creates 24 plots in facets
#'  * each plots has randomly chosen number of triangles
#' @param param see above
#' @details
#'  * first call is to `make_triangles`
#'

grid_24_triangles(param)


#| Experimentell ----
#| F, K
#| dgrees = 10, rotation = 10
# s_p <- df |>
#   filter(rw == "F")
# print(plot_single(s_p, param))


# saveRDS(s_p, file = here::here("schöner_DF.rds"))
# # saveRDS(s_p, file = here::here("schöner_DF_2"))
# f <- s_p[1:3, 1:3]
