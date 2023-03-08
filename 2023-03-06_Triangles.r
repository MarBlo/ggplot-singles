library(ggplot2)
library(dplyr)
library(tibble)

source(here::here("2023-03-06_helper-Triangles.r"))
source(here::here("2023-03-06_worker-Triangles.r"))

param <- list(
  degrees = 10,
  rotation = 10,
  x_point = c(3:10),
  y_point = c(2:5),
  mit_schwerpunkt = TRUE,
  size = 2,
  mit_pathline = TRUE,
  linewidth = .5
)


make_triangles(param)
