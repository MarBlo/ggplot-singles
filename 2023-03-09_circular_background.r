library(ggplot2)
library(grid)

# make a plot with blue background
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  theme(
    plot.background = element_rect(fill = "#C4E7FF"),
    panel.background = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  coord_equal()
p


my_circle <- circleGrob(
  x = 1, y = 1, r = 5,
  gp = gpar(col = "red", lty = 3)
)
plot(my_circle)
grid.draw(my_circle)

my_circle <- circleGrob(
  name = "my_circle",
  x = 0.5, y = 0.5, r = 0.45,
  gp = gpar(fill = "gray", lty = 3)
)
grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)
# switch out background grob
g <- ggplotGrob(p)
bg <- g$grobs[[1]]
round_bg <- roundrectGrob(
  x = bg$x, y = bg$y,
  width = 1.414 * bg$width, height = 1.414 * bg$height,
  r = unit(1, "snpc"),
  just = bg$just, name = bg$name, gp = bg$gp, vp = bg$vp
)
g$grobs[[1]] <- round_bg
plot(g)

# draw both plots side by side
cowplot::plot_grid(p, g,
  labels = c("rectangular", "rounded"),
  scale = 1, hjust = 0.5, label_x = 0.5
)

p



grid.draw(rectGrob())
sample_vp <- viewport(
  x = 0.5, y = 0.5,
  width = 0.5, height = 0.5,
  just = c("left", "bottom")
)
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(p)
popViewport()


circles <- data.frame(
  x0 = rep(1:3, 3),
  y0 = rep(1:3, each = 3),
  r = seq(0.1, 1, length.out = 9)
)
circles

ggplot(data = circles[9, ]) +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = "transparent")) +
  coord_fixed() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
  )
