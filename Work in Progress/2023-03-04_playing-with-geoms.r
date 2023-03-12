# library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)


# angle <- 90 # in °
# angle_rad <- (2 * pi / 360) * angle

#| DREIECK mit _path und _polygon ----
l_x <- 1
l_y <- .8

df_1 <- tibble(
  x = c(0, l_x, 0, l_x, l_x),
  y = c(0, 0, 0, l_y, 0)
)
df_1
#      x     y
#  <dbl> <dbl>
#  0   0
#  1   0
#  0   0
#  1   0.8
#  1   0

ggplot(df_1) +
  geom_path(aes(x, y))

ggplot(df_1) +
  geom_polygon(aes(x, y))

#| --
#| --

u <- c(0, 0, 1, 1)
# v <- c(0, 0, 1, .5)
v <- c(0.5, 1, -0.5, 1)

make_df <- function(v1, v2) {
  df <- data.frame(
    center_x = c(v1[1], v2[1]),
    center_y = c(v1[2], v2[2]),
    end_x = c(v1[3], v2[3]),
    end_y = c(v1[4], v2[4])
  )
  rownames(df) <- c("v1", "v2")
  df
}

v <- make_df(u, v)
# v
#    center_x center_y end_x end_y
# v1      0.0      0.0     1   1.0
# v2      0.5      0.5     1   0.5
ggplot(v) +
  geom_segment(
    aes(
      x = center_x, y = center_y, xend = end_x, yend = end_y
    ),
    arrow = arrow(length = unit(.5, "cm"))
  )





# u + v = w
(len_u <- sqrt(sum(u^2))) # length == Euclidean norm
(len_v <- sqrt(sum(v^2))) # length == Euclidean norm
(cos_angle_uv <- (sum(u * v) / (len_u * len_v))) # cosine of the angle
(len_w <- sqrt(sum(u + v)^2))
u + v


add_and <- function(v1, v2, what = "sum") {
  df <- data.frame(
    center_x = c(v1[1], v2[1]),
    center_y = c(v1[2], v2[2]),
    end_x = c(v1[3], v2[3]),
    end_y = c(v1[4], v2[4]),
    group = c("u", "v")
  )
  rownames(df) <- c("u", "v")
  if (what == "sum") {
    sum_uv <- v1 + v2
    w <- data.frame(
      center_x = sum_uv[1],
      center_y = sum_uv[2],
      end_x = sum_uv[3],
      end_y = sum_uv[4],
      group = "w"
    )
    df <- bind_rows(df, w)
    rownames(df) <- c("u", "v", "w")
  }
  df
}

sum_two <- add_and(u, v, what = "sum")

ggplot(sum_two) +
  geom_segment(
    aes(
      x = center_x, y = center_y, xend = end_x, yend = end_y, color = group
    ),
    arrow = arrow(length = unit(.5, "cm"))
  )


add_minus_and <- function(v1, v2, what = "plus") {
  sum_uv <- vector()
  df <- data.frame(
    center_x = c(v1[1], v2[1]),
    center_y = c(v1[2], v2[2]),
    end_x = c(v1[3], v2[3]),
    end_y = c(v1[4], v2[4]),
    group = c("u", "v")
  )
  rownames(df) <- c("u", "v")
  if (what == "plus") {
    sum_uv <- v1 + v2
    print(sum_uv)
    print(paste("from plus", sum_uv))
    w <- data.frame(
      # center_x = v1[1] + v2[1] + sum_uv[1],
      # center_y = v1[2] + v2[2] + sum_uv[2],
      # end_y = sum_uv[3] + v1[3] + v2[3],
      # end_x = sum_uv[4] + v1[4] + v2[4],
      center_x = sum_uv[1],
      center_y = sum_uv[2],
      end_x = sum_uv[3],
      end_y = sum_uv[4],
      group = "w"
    )
  }
  if (what == "minus") {
    sum_uv <- v1 - v2
    # sum_uv <- c(v2[c(3, 4)], sum_uv[c(3, 4)])
    print(sum_uv)
    w <- data.frame(
      center_x = sum_uv[1],
      center_y = sum_uv[2],
      end_x = sum_uv[3],
      end_y = sum_uv[4],
      group = "w"
    )
  }
  df <- bind_rows(df, w)
  rownames(df) <- c("u", "v", "w")
  df
}

u <- c(0, 0, 1, 1)
# v <- c(0, 0, 1, .5)
v <- c(0, 0, -1, -2)
pm <- add_minus_and(u, v, what = "minus")
pm
ggplot(pm) +
  geom_segment(
    aes(
      x = center_x, y = center_y, xend = end_x, yend = end_y, color = group
    ),
    arrow = arrow(length = unit(.5, "cm"))
  ) +
  coord_equal()


coord_y_y <- l_y * sin(angle_rad)
coord_y_x <- l_y * cos(angle_rad)

tibble(
  x = c(0, l_x, 0, coord_y_x, l_x),
  y = c(0, 0, 0, coord_y_y, 0)
) |>
  ggplot() +
  geom_path(aes(x, y))


#| make vectors with geom_segment ----
a <- c(2, 0) # the point (2,0)
b <- c(1, 3) # the point (1,3)
center <- c(0, 0)
df <- data.frame(
  x.end = c(a[1], b[1]),
  y.end = c(a[2], b[2]),
  center = center,
  label = c("a", "b")
)
# df
#  x.end y.end center label
#   2     0      0     a
#   1     3      0     b
ggplot(df) +
  geom_segment(
    aes(
      x = center[1], y = center[2], xend = x.end, yend = y.end,
      color = label
    ),
    arrow = arrow()
  ) +
  labs(x = "x-coordinate", y = "y-coordinate") +
  coord_fixed(ratio = 1) +
  theme_bw()
