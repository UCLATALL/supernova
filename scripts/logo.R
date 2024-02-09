extrafont::loadfonts(device = "win")
library(ggplot2)
library(mathart)
library(imager)

hex_coords <- function(width) {
  data.frame(
    x = width * c(1, 1, 0, -1, -1, 0),
    y = width / sqrt(3) * c(1, -1, -2, -1, 1, 2)
  )
}

translate <- function(df, dx = 0, dy = 0) {
  df[["x"]] <- df[["x"]] + dx
  df[["y"]] <- df[["y"]] + dy
  df
}

rotate_180 <- function(df) {
  df[["y"]] <- -1 * df[["y"]]
  df
}

color <- list(
  primary = "#19ACED",
  secondary = "#F0314E",
  dark = "#2C3E50",
  light = "#FFFFFF"
)

nova <- harmonograph(
  A1 = 1, A2 = 1, A3 = 1, A4 = 1,
  d1 = 0.004, d2 = 0.0065, d3 = 0.008, d4 = 0.019,
  f1 = 3.001, f2 = 2, f3 = 3, f4 = 2,
  p1 = 0, p2 = 0, p3 = pi / 2, p4 = 3 * pi / 2
)

novas <- list(nova, rotate_180(nova))

# Plot
ggplot(mapping = aes(x, y)) +
  geom_polygon(data = hex_coords(4) |> translate(dy = -1.3), fill = color$primary) +
  geom_polygon(data = hex_coords(3.6) |> translate(dy = -1.3), fill = color$dark) +
  geom_text(
    data = data.frame(x = 0, y = -2.8), label = "supernova", size = 20,
    color = color$light, family = "Consolas"
  ) +
  geom_path(aes(x, y), novas[[1]], alpha = 0.75, size = .5, color = color$secondary) +
  geom_path(aes(x, y), novas[[2]], alpha = 0.75, size = .5, color = color$primary) +
  coord_equal() +
  theme_blankcanvas()

if (!dir.exists("man/figures")) dir.create("man/figures")

ggsave(
  "man/figures/logo.png",
  width = 10.8,
  height = 9.6,
  units = "in",
  bg = "transparent"
)

load.image("man/figures/logo.png") |>
  autocrop() |>
  save.image("man/figures/logo.png")
