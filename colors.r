#' Ref:
#' https://www.r-bloggers.com/2013/09/
#' how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/

library(RColorBrewer)
library(colorRamps)

## See available palettes
# display.brewer.all()

## You need to expand palette size
get_palette <- colorRampPalette(brewer.pal(9, "PuRd"))

# rev(get_palette(12))
# get_palette(12)

# n <- 20
# tibble(
#   x = seq(n),
#   y = 0,
#   hex = get_palette(n)
# ) |> ggplot() +
#   geom_tile(aes(
#     x = x, y = y, fill = hex
#   )) +
#   scale_fill_manual(values = get_palette(n))

#| Colors ----
library(PrettyCols)
my_cols <- c(
  "#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84",
  "#fc8d59", "#ef6548", "#d7301f", "#b30000",
  "#7f0000", "#fff7ec", "#fee8c8", "#fdd49e",
  "#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84",
  "#fc8d59", "#ef6548", "#d7301f", "#b30000",
  "#7f0000", "#fff7ec", "#fee8c8", "#fdd49e",
  "#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84",
  "#fc8d59", "#ef6548", "#d7301f", "#b30000"
)

my_greens <- c(
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc",
  "#22577a", "#38a3a5", "#57cc99", "#80ed99", "#c7f9cc"
)

many_greens <- c(
  "#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00",
  "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f",
  "#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00",
  "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f",
  "#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00",
  "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f",
  "#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00",
  "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f"
)
