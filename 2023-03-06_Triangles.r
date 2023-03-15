library(ggplot2)
library(dplyr)
library(tibble)
library(cowplot)
library(gridExtra)

source(here::here("2023-03-06_helper-Triangles.r"))
source(here::here("2023-03-06_worker-Triangles.r"))
source(here::here("colors.r"))

param <- list(
  #' makes 24 plots each with several triangles
  #' angle between triangles
  degrees = 15,
  #' number of rotations, shouldn`t be more than 360/degrees
  rotation = 18,
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
  #' background
  color_back = "black",
  #' alpha
  alpha = .5
)

#' MAIN FUNCTIONs ----

##' grid_24_triangels ----
#'
#' @description
#'  * creates 24 plots in facets
#'  * each plots has randomly chosen number of triangles
#' @param param see above
#' @details
#'  * first call is to `make_triangles`
#'
p <- grid_24_triangles(param)
# ggsave(p, "plot_b.png")



##' triangles ----
#'
#' @description
#'  * creates on plot with any many triangles as given in param$rotation
#' @param param, b_point, c_point
#' @details
#'  * origin at (0,0), b_point & c_point must be given in fct call
#'  * calls plot_single
#'
triangles(b_point = c(9, 0), c_point = c(5, 5), param)


#' Experimental ----
##' make animation ----


plots <- vector("list")
for (i in seq(10)) {
  param$rotation <- i
  temp <- triangles(b_point = c(9, 2), c_point = c(1, 5), param)
  temp$rr <- letters[i]
  plots[[i]] <- temp
}
plots[[10]][[2]]

library(gganimate)
anim_df <- bind_rows(plots)

pp <- plot_single(anim_df, param)[[2]]
anim <- pp + transition_states(rr,
  transition_length = .1,
  state_length = 1
)
# anim
animate(anim, renderer = gifski_renderer())
anim_save("plot_a.gif")


#'
#'
plots_degree <- vector("list")
deg <- c(10, 15, 20, 24, 30)
rot <- 360 / deg
for (d in seq(deg)) {
  param$degrees <- deg[d]
  param$rotation <- rot[d]
  temp <- triangles(b_point = c(9, 0), c_point = c(5, 5), param)[[1]]
  plots_degree[[d]] <- temp
}
df <- bind_rows(plots_degree)
head(df, 20)
unique(df$rw)
unique(df$label)
nrow(df)

ggplot(df) +
  geom_path(aes(x, y)) +
  facet_grid(~rw)
