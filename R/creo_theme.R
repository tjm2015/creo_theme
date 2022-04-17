theme_creo <- function(){ 
  font <- "Calibri"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = .5,               #center
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

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
