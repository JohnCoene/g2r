#' Gauge colour
#'
#' Gauge colour, similar to the \code{scale_colour_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param colors A vector of colors.
#' @param callback A JavaScript callback function (see \code{\link{cb}}) which returns a color.
#' 
#' @export
gauge_color <- function(g2, colors = NULL, callback = NULL){
  opts <- list(colors = colors, callback = callback)
  make_scale(g2, vars = opts, method = "color")
}

#' Gauge size
#'
#' Gauge size.
#' 
#' @inheritParams geoms
#' @param range A vector indicating the minimum and maximum sizes.
#' @inheritParams gauge_color
#' 
#' @export
gauge_size <- function(g2, range = NULL, callback = NULL){
  opts <- list(range = range, callback = callback)
  make_scale(g2, vars = opts, method = "size")
}

#' Gauge opacity
#'
#' Gauge opacity.
#' 
#' @inheritParams geoms
#' @inheritParams gauge_color
#' 
#' @export
gauge_opacity <- function(g2, callback = NULL){
  opts <- list(callback = callback)
  make_scale(g2, vars = opts, method = "opacity")
}

#' Gauge shape
#'
#' Gauge shapes.
#' 
#' @inheritParams geoms
#' @param shapes A vector of shapes.
#' @inheritParams gauge_color
#' 
#' @export
gauge_shape <- function(g2, shapes = NULL, callback = NULL){
  opts <- list(shapes = shapes, callback = callback)
  make_scale(g2, vars = opts, method = "shape")
}

#' Gauge label
#'
#' Gauge label.
#' 
#' @inheritParams geoms
#' @inheritParams gauge_color
#' 
#' @export
gauge_label <- function(g2, callback, ...){
  opts <- list(callback = callback, cfg = list(...))
  make_scale(g2, vars = opts, method = "label")
}

#' Gauge tooltip
#'
#' Gauge tooltip.
#' 
#' @inheritParams geoms
#' @inheritParams gauge_color
#' 
#' @export
gauge_tooltip <- function(g2, callback = NULL){
  make_scale(g2, vars = list(callback = callback), method = "tooltip")
}

# make scale
make_scale <- function(g2, vars, method = "color"){

  vars <- vars[lapply(vars, length) > 0]
  
  # insert or replace
  g2$x$scales[[method]] <- vars
    
  
  return(g2)
}
