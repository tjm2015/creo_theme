library(tidyverse)
library(palmerpenguins)
library(extrafont)


install.packages('remotes', dependencies=TRUE, verbose=TRUE)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import("Calibri")

font_import("Calibri")

theme_set(theme_classic(base_family = 'Calibri'))

# Creo color theme codes
creo_colors <- c(
  `dark blue`  = "#1A2944",
  `light blue` = "#1EBCE8",
  `dark grey`  = "#787878",
  `light grey` = "#ACB3b8",
  `green`      = "#009999",
  `orange`     = "#FF9610",
  `creo logo`  = "#2d2d72"
  )

#' Function to extract Creo colors as hex codes
#'
#' @param ... Character names of creo_colors 
#'
creo_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (creocolors)
  
  creo_colors[cols]
}

creo_palettes <- list(
  `main`  = creo_cols("dark blue", "light blue", "dark grey", "light grey", "green", "orange"),
  
  `cool`  = creo_cols("creo logo", "green"),
  
  `hot`   = creo_cols("dark blue", "green", "orange"),
  
  `grey`  = creo_cols("light grey", "dark grey")
)

#' Return function to interpolate a Creo color palette
#'
#' @param palette Character name of palette in creo_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'

creo_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- creo_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'

scale_color_creo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- creo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("creo_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for creo colors
#'
#' @param palette Character name of palette in creo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_creo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- creo_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("creo_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# Color by discrete variable using default palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_color_creo()

# Color by numeric variable with cool palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_color_creo(discrete = FALSE, palette = "cool")

# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_creo(palette = "hot", guide = "none", reverse = FALSE)


# Scatter Example
penguins %>% 
  ggplot(
    aes(
      x = bill_depth_mm,
      y = bill_length_mm,
      color = species
    )
  ) +
  geom_point(alpha = .8) +
  scale_color_creo("hot") +
  xlab("Bill Depth\n(mm)") + ylab("Bill Length\n(mm)") +
  labs(
    title = "Bill Length versus Depth by Species",
    caption = "Source: Palmer Penguins") +
  theme(legend.position = "top")


# Histogram Example  
penguins %>% 
  filter(sex != "NA") %>% 
  ggplot(
    aes(
      x = body_mass_g,
      fill = island
    )
  ) +
  geom_histogram(color = "white") +
  scale_fill_creo("cool") +
  facet_grid(sex~island) +
  guides(fill = FALSE)

# Line Chart
penguins %>% 
  group_by(year, island) %>% 
  count(island) %>% 
  ggplot(
    aes(
      x = year,
      y = n,
      color = island
    )
  ) +
  geom_point() +
  geom_line() +
  scale_color_creo("hot") +
  theme(legend.position = "top")

# Boxplots

penguins %>% 
  ggplot(
    aes(
      x = factor(island),
      y = body_mass_g,
      fill = island,
      )
  ) +
  geom_boxplot() +
  scale_fill_creo("hot") +
  guides(fill = FALSE)
  theme(legend.position = "top")
