#' Motif
#' 
#' Customise the chart motif.
#' 
#' @inheritParams geoms
#' @param ... Motif options.
#' 
#' @examples
#' g <- g2(iris, asp(Sepal.Length, Sepal.Width)) %>% 
#'   fig_point(asp(color = Species)) 
#' 
#' g %>% motif_dark()
#' 
#' g %>% custom_motif(colors = c("red", "green", "blue"))
#' 
#' @name theme
#' @export
motif_dark <- function(g2) {
  check_g2(g2)
  g2$x$opts$theme <- "dark"
  return(g2)
}

#' @rdname theme
#' @export
custom_motif <- function(g2, ...){
  check_g2(g2)
  g2$x$theme <- list(...)
  return(g2)
}

#' Font
#' 
#' Set chart font.
#' 
#' @inheritParams geoms
#' @param font Font family to use for chart text.
#' 
#' @export
conf_font <- function(g2, font){
  check_g2(g2)
  if(missing(font))
    stop("missing font", call. = FALSE)
  
  g2$x$font <- font
  return(g2)
}

#' Style
#' 
#' Customise general style of the chart.
#' 
#' @inheritParams geoms
#' @param coord_type,coord_rotate,coord_sx,coord_sy,coord_reflect,coord_transpose 
#'   Customise global coordinates of chart.
#' @param width,height Chart dimensions.
#' @param padding Padding around chart, a vector of length \code{4} corresponding to top, right, bottom, and left.
#' @param bg_fill,bg_opacity,bg_fill_opacity,bg_stroke,bg_stroke_opacity,bg_line_width,bg_radius
#'   Customise background of global chart.
#' @param plot_fill,plot_fill_opacity,plot_stroke,plot_stroke_opacity,plot_opacity,plot_line_width,plot_radius
#'   Customise plot background.
#' @param fit Whether to force graph to fit parent element.
#' @param animate Whether to animate the chart.
#' @param pixel_ratio Device pixel ratio, defaults to \code{window.devicePixelRatio}. 
#' @param renderer Renderer, \code{canvas} or \code{svg}.
#' @param font Font to use on chart.
#'
#' @examples
#' g2(cars, asp(speed, dist)) %>% 
#'   fig_point() %>% 
#'   motif(plot_fill = "grey")
#' 
#' @export
motif <- function(g2, coord_type = NULL, coord_rotate = NULL, coord_sx = NULL, coord_sy = NULL,
  coord_reflect = NULL, coord_transpose = NULL, width = NULL, height = NULL, padding = rep(10, 4), bg_fill = NULL, bg_opacity = NULL, 
  bg_fill_opacity = NULL, bg_stroke = NULL, bg_stroke_opacity = NULL, bg_line_width = NULL, bg_radius = NULL, plot_fill = NULL,
  plot_fill_opacity = NULL, plot_stroke = NULL, plot_stroke_opacity = NULL, plot_opacity = NULL, plot_line_width = NULL, 
  plot_radius = NULL, fit = TRUE, animate = TRUE, pixel_ratio = NULL, renderer = NULL, font = NULL){

  # coord
  if(!is.null(coord_type)) g2$x$coord <- coord_type
  if(!is.null(coord_rotate)) g2$x$coordRotate <- coord_rotate
  if(!is.null(coord_sx) && !is.null(coord_sy)) g2$x$coordScale <- list(coord_sx, coord_sx)
  if(!is.null(coord_reflect)) g2$x$coordReflect <- coord_reflect
  if(!is.null(coord_transpose)) g2$x$coordTranspose <- coord_transpose

  # chart options
  if(!is.null(width)) g2$x$opts$width <- width
  if(!is.null(height)) g2$x$opts$height <- height
  if(!is.null(padding)) g2$x$opts$padding <- padding
  if(!is.null(pixel_ratio)) g2$x$opts$pixelRatio <- pixel_ratio
  if(!is.null(renderer)) g2$x$opts$renderer <- renderer
  g2$x$opts$forceFit <- fit
  g2$x$opts$animate <- animate

  # background
  bg <- list(
    fill = bg_fill,
    fillOpacity = bg_fill_opacity,
    stroke = bg_stroke,
    strokeOpacity = bg_stroke_opacity,
    opacity = bg_opacity,
    lineWidth = bg_line_width,
    radius = bg_radius
  )
  bg <- bg[lapply(bg, length) > 0]
  if(length(bg)) g2$x$opts$background <- bg

  # plot
  plot <- list(
    fill = plot_fill,
    fillOpacity = plot_fill_opacity,
    stroke = plot_stroke,
    strokeOpacity = plot_stroke_opacity,
    opacity = plot_opacity,
    lineWidth = plot_line_width,
    radius = plot_radius
  )
  plot <- plot[lapply(plot, length) > 0]
  if(length(plot)) g2$x$opts$plotBackground <- plot

  if(!is.null(font)) g2$x$font <- font
  
  return(g2)
}