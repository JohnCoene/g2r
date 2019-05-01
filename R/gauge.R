#' Gauge colour
#'
#' Gauge colour, similar to the \code{scale_colour_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param colors A vector of colors.
#' @param callback A JavaScript callback function (see \code{\link{cb}}) which returns a color.
#' 
#' @examples
#' g2(mtcars, plan(mpg, qsec, color = drat)) %>% 
#'   fig_point() %>% 
#'   gauge_color(c("red", "white", "blue"))
#' 
#' @export
gauge_color <- function(g2, colors = NULL, callback = NULL){
  opts <- list(colors = colors, callback = callback)
  make_scale(g2, vars = opts, method = "color")
}

#' Gauge size
#'
#' Gauge size given a range or a JavaScript callback function.
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
#' @examples
#' g2(mtcars, plan(mpg, qsec, opacity = drat, size = 10)) %>% 
#'   fig_point()
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
#' @examples
#' g2(mtcars, plan(mpg, qsec, shape = am)) %>% 
#'   fig_point() %>% 
#'   gauge_shape(c("hollowDiamond", "hollowBowtie"))
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
#' @examples
#' g2(mtcars, plan(mpg, qsec, shape = drat)) %>% 
#'   fig_point() %>% 
#'   gauge_label(textStyle = list(rotate = 30))
#' 
#' @export
gauge_label <- function(g2, ..., callback = NULL){
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
#' @examples
#' callback <- cb(
#'   "function(drat, qsec){
#'     percent = qsec / 100 + '%';
#'     return {
#'       name: drat,
#'       value: percent
#'     };
#'   }"
#' )
#' 
#' template <- '<li>{name}: {value}</li>'
#' 
#' g2(mtcars, plan(mpg, qsec, tooltip = drat, tooltip = qsec, color = qsec)) %>% 
#'   fig_point() %>% 
#'   gauge_tooltip(callback) %>% 
#'   conf_tooltip(itemTpl = template, showTitle = FALSE)
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
