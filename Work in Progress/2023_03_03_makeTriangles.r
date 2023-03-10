library(ggplot2)
library(dplyr)
library(tibble)

source(here::here("2023-03-06_helper-Triangles.r"))

#| Unterschied zwischen geom_path und _line ----
#| Unterschied zw _line und _path
#| geom_path zeichnet Linie von Wertepaar zu Wertepaar
#| geom_line zeichnet Linie in der Reihenfolge der x-Achse
tibble(
  x = c(1, 3, 2, 2),
  y = c(1, 1, 3, 2),
  label = c(1, 2, 3, 4)
) |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_label(aes(label = label)) +
  geom_path()

tibble(
  x = c(1, 3, 2, 2),
  y = c(1, 1, 3, 2),
  label = c(1, 2, 3, 4)
) |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_label(aes(label = label)) +
  geom_line()

#| Einfache Dreiecke ----
dreieck <- tibble(
  x = c(1, 3, 2),
  y = c(1, 1, 3),
  label = c(1, 2, 3)
)

ggplot(dreieck, aes(x, y)) +
  geom_path() +
  geom_label(aes(label = label))

ggplot(dreieck, aes(x, y)) +
  geom_line() +
  geom_label(aes(label = label))

ggplot(dreieck, aes(x, y)) +
  geom_area() +
  geom_label(aes(label = label))

ggplot(dreieck, aes(x, y)) +
  geom_polygon(color = "black", fill = "NA") +
  geom_label(aes(label = label)) +
  coord_equal()

## |   Dreicke drehen      ----
## |   Eingabe mit Punkten ----
a_ecke <- c(0, 0)
b_ecke <- c(7, 2)
c_ecke <- c(5, 5)
a <- c(a_ecke, b_ecke)
b <- c(b_ecke, c_ecke)
c <- c(c_ecke, a_ecke)
df <- tibble(
  a = a,
  b = b,
  c = c
) |>
  t() |>
  data.frame() |>
  setNames(c("x", "y", "xend", "yend")) |>
  mutate(label = rep("start", 3))

#| Mittelpunkt
mp <- 1 / 3 * (a[c(1, 2)] + b[c(1, 2)] + c[c(1, 2)])
#| Seitenhalbierende
a_halb <- 0.5 * (a[c(1, 2)] + a[c(3, 4)])
b_halb <- 0.5 * (a[c(3, 4)] + b[c(3, 4)])
c_halb <- 0.5 * (b[c(3, 4)] + c[c(3, 4)])

seitenhalbe <- tibble(mp, a_halb, b_halb, c_halb) |>
  t() |>
  data.frame() |>
  setNames(c("x", "y")) |>
  mutate(label = rep("start", 3))

drei_e <- ggplot(df, aes(x = x, y = y)) +
  geom_segment(aes(xend = xend, yend = yend)) +
  geom_point(data = seitenhalbe, aes(x = x, y = y)) +
  coord_equal()

plot(drei_e)

## | Dreieck drehen          ----
## | Drehen um den Nullpunkt ----
dreh_winkel <- 40 # in degree

(winkel_a <- degree(angle(t(c_ecke), as.matrix(b_ecke))))
(winkel_a <- degree(angle(t(c_ecke), as.matrix(b_ecke))))
(winkel_b <- degree(angle(t(-b_ecke), as.matrix(c_ecke - b_ecke))))
(winkel_c <- degree(angle(t(-c_ecke), as.matrix(b_ecke - c_ecke))))
winkel_a + winkel_b + winkel_c

(null_winkel_b <- degree(angle(t(c(1, 0)), as.matrix(b_ecke))))
(null_winkel_c <- degree(angle(t(c(1, 0)), as.matrix(c_ecke))))
(winkel_b <- null_winkel_b + dreh_winkel)
(winkel_c <- null_winkel_c + dreh_winkel)

len_b <- sqrt(sum(b_ecke^2))
len_c <- sqrt(sum(c_ecke^2))

neue_b_ecke <- c(
  len_b * cos(radian(winkel_b)),
  len_b * sin(radian(winkel_b))
)
neue_c_ecke <- c(
  len_c * cos(radian(winkel_c)),
  len_c * sin(radian(winkel_c))
)

aa <- c(a_ecke, neue_b_ecke)
bb <- c(neue_b_ecke, neue_c_ecke)
cc <- c(neue_c_ecke, a_ecke)
df_neu <- tibble(
  aa = aa,
  bb = bb,
  cc = cc
) |>
  t() |>
  data.frame() |>
  setNames(c("x", "y", "xend", "yend")) |>
  mutate(label = rep("1. schleife", 3))

df_1 <- bind_rows(df, df_neu)

#| Mittelpunkt
mp_1 <- 1 / 3 * (aa[c(1, 2)] + bb[c(1, 2)] + cc[c(1, 2)])
#| Seitenhalbierende
aa_halb <- 0.5 * (aa[c(1, 2)] + aa[c(3, 4)])
bb_halb <- 0.5 * (aa[c(3, 4)] + bb[c(3, 4)])
cc_halb <- 0.5 * (bb[c(3, 4)] + cc[c(3, 4)])

seitenhalbe_1 <- tibble(mp_1, aa_halb, bb_halb, cc_halb) |>
  t() |>
  data.frame() |>
  setNames(c("x", "y")) |>
  mutate(label = rep("1. schleife", 4))
seitenhalbe_1 <- bind_rows(seitenhalbe, seitenhalbe_1)

drei_e_plus <- ggplot(df_1) +
  geom_polygon(aes(
    x = xend, y = yend, # xend = xend, yend = yend,
    fill = label
  ), alpha = .5) +
  # geom_point(data = seitenhalbe_1, aes(x = x, y = y, color = label)) +
  coord_equal() +
  theme_void() +
  guides(fill = FALSE)

plot(drei_e_plus)










#| Kreissegmente mit Kurven ----
angles <- seq(0, 2 * pi, length.out = 10)

tibble(
  x = cos(angles),
  y = sin(angles)
) |>
  mutate(group = LETTERS[1:10]) |>
  expand.grid() |>
  filter(group == "A") |>
  ggplot(aes(x, y, group = group)) +
  geom_path() +
  coord_equal()


head_df <- tibble(
  xend = cos(angles),
  y0 = 0,
  angles = angles,
  lead_angles = lead(angles)
) |>
  mutate(x0 = 0, yend = sin(angles)) |>
  mutate(
    xcurve_0 = cos(angles), ycurve_0 = sin(angles),
    xcurve_end = cos(lead(angles)), ycurve_end = sin(lead(angles))
  ) |>
  head(n = 2)

ggplot(head_df) +
  geom_curve(aes(
    x = xcurve_0, xend = xcurve_end,
    y = ycurve_0, yend = ycurve_end
  )) +
  geom_segment(aes(
    x = x0, xend = xend, y = y0, yend = yend
  )) +
  coord_equal()


# Versuch mit geom_ribbon Fläche zu füllen
df <- tibble(
  angles = angles,
  x = cos(angles)
)

base <-
  ggplot(df) +
  xlim(0, 1)
base + geom_path(aes(x = angles, y = x)) +
  coord_equal()




# Tests ----

a
b
c

# V₁·V₂ = x₁·x₂ + y₁·y₂ + z₁·z₂ = |V₁| · |V₂| · cos(θ);
# |V₁| = √(x₁² + y₁² + z₁²), and
# |V₂| = √(x₂² + y₂² + z₂²),
# cos(θ) = (x₁·x₂ + y₁·y₂ + z₁·z₂) ÷ (|V₁|·|V₂|),

ac <- a * c
a_ <- sqrt(sum(a^2))
c_ <- sqrt(sum(c^2))

(degree(cos_t <- sum(ac) / (a_ * c_)))


(ac <- c(7, 0) * b_punkt)

a_ <- sqrt(sum(a^2))
c_ <- sqrt(sum(c^2))

(degree(cos_t <- sum(ac) / (a_ * c_)))


nu <- as.matrix(c(0, 0))
a <- as.matrix(a_punkt)
b <- as.matrix(b_punkt)
c <- as.matrix(c_punkt)

degree(angle(t(b), c))
degree(angle(t(c), a))
acos(
  sum(a_ecke * b_ecke) / (sqrt(sum(
    a_ecke * a_ecke
  )) * sqrt(sum(b_ecke * b_ecke)))
) |> degree()
acos(
  sum(a_ecke * c_ecke) / (sqrt(sum(
    a_ecke * a_ecke
  )) * sqrt(sum(c_ecke * c_ecke)))
) |> degree()
acos(
  sum(b_ecke * c_ecke) / (sqrt(sum(
    b_ecke * b_ecke
  )) * sqrt(sum(c_ecke * c_ecke)))
) |> degree()

angle(t(c_ecke), as.matrix(b_ecke))




acos(
  sum(c_ecke * b_ecke) / (sqrt(sum(
    b_ecke * b_ecke
  )) * sqrt(sum(c_ecke * c_ecke)))
) |> degree()
acos(
  sum(b_ecke * c_ecke) / (sqrt(sum(
    c_ecke * c_ecke
  )) * sqrt(sum(b_ecke * b_ecke)))
) |> degree()
acos(
  sum(c(0, 0) * c_ecke) / (sqrt(sum(
    c_ecke * c_ecke
  )) * sqrt(sum(c(1, 1) * c(1, 1))))
) |> degree()
