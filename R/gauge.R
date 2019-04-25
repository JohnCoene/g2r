#' Gauge colour
#'
#' Gauge colour, similar to the \code{scale_colour_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param colors A vector of colors or a JavaScript function.
#' 
#' @name scale-color
#' @export
gauge_colour <- function(g2, colors){
  make_scale(g2, vars = colors, method = "color")
}

#' @rdname scale-color
#' @export
gauge_color <- gauge_colour

#' Gauge shape
#'
#' Gauge shapes.
#' 
#' @inheritParams geoms
#' @param shapes A vector of shapes or a JavaScript function.
#' 
#' @export
gauge_shape <- function(g2, shapes){
  make_scale(g2, vars = shapes, method = "shape")
}

#' Gauge size
#'
#' Gauge size.
#' 
#' @inheritParams geoms
#' @param size A vector of sizes (\code{min, max}) or a JavaScript function.
#' 
#' @export
gauge_size <- function(g2, size){
  make_scale(g2, vars = size, method = "size")
}

#' Gauge opacity
#'
#' Gauge opacity.
#' 
#' @inheritParams geoms
#' @param opacity A JavaScript function.
#' 
#' @export
gauge_opacity <- function(g2, opacity){
  make_scale(g2, vars = opacity, method = "opacity")
}

#' Gauge label
#'
#' Gauge label.
#' 
#' @inheritParams geoms
#' @param label A configuration list or a JavaScript funtion.
#' 
#' @export
gauge_label <- function(g2, label){
  make_scale(g2, vars = label, method = "label")
}

#' Gauge tooltip
#'
#' Gauge tooltip.
#' 
#' @inheritParams geoms
#' @param tooltip A JavaScript function.
#' 
#' @export
gauge_tooltip <- function(g2, tooltip){
  make_scale(g2, vars = tooltip, method = "tooltip")
}

# make scale
make_scale <- function(g2, vars, method = "color"){

  if(missing(vars))
    stop("missing arguments", call. = FALSE)

  if(!inherits(vars, "JS_EVAL") && length(vars) > 1)
    vars <- as.list(vars)

  scale <- list()
  scale[[method]] <- vars
  
  if(is.null(g2$x$scales[[method]]))
    g2$x$scales <- append(g2$x$scales, scale)
  else
    g2$x$scales[[method]] <- scale
  
  return(g2)
}