#' Gauge colour
#'
#' Gauge colour, similar to the \code{scale_colour_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param option Palette name.
#' @param reverse Whether to reverse the palette.
#' @param colors A vector of colors.
#' @param callback A JavaScript callback function (see \code{\link{cb}}) which returns a color.
#' 
#' @examples
#' g2(mtcars, asp(mpg, qsec, color = drat)) %>% 
#'   fig_point() %>% 
#'   gauge_color(c("red", "white", "blue"))
#' 
#' @name gauge_color
#' @export
gauge_color <- function(g2, colors = NULL, callback = NULL){
  opts <- list(colors = colors, callback = callback)
  make_scale(g2, vars = opts, method = "color")
}

#' @name gauge_color
#' @export
gauge_color_viridis <- function(g2, option = c("viridis", "magma", "plasma", "inferno", "cividis"), reverse = FALSE, callback = NULL){
  colors <- get_viridis_color(match.arg(option), reverse)
  opts <- list(colors = colors, callback = callback)
  make_scale(g2, vars = opts, method = "color")
}

get_viridis_color <- function(opts, rev = FALSE){
  viridis <- c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", 
    "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF")
  magma <- c("#000004FF", "#180F3EFF", "#451077FF", "#721F81FF", "#9F2F7FFF", 
    "#CD4071FF", "#F1605DFF", "#FD9567FF", "#FEC98DFF", "#FCFDBFFF")
  inferno <- c("#000004FF", "#1B0C42FF", "#4B0C6BFF", "#781C6DFF", "#A52C60FF", 
    "#CF4446FF", "#ED6925FF", "#FB9A06FF", "#F7D03CFF", "#FCFFA4FF") 
  plasma <- c("#0D0887FF", "#47039FFF", "#7301A8FF", "#9C179EFF", "#BD3786FF", 
    "#D8576BFF", "#ED7953FF", "#FA9E3BFF", "#FDC926FF", "#F0F921FF")
  cividis <- c("#00204DFF", "#00336FFF", "#39486BFF", "#575C6DFF", "#707173FF", 
    "#8A8779FF", "#A69D75FF", "#C4B56CFF", "#E4CF5BFF", "#FFEA46FF")

  color <- viridis
  if(opts == "magma")
    color <- magma
  else if(opts == "inferno")
    color <- inferno
  else if(opts == "plasma")
    color <- plasma
  else if(opts == "cividis")
    color <- cividis

  if(rev)
    color <- rev(color)
  
  return(color)
}

#' Gauge size
#'
#' Gauge size given a range or a JavaScript callback function.
#' 
#' @inheritParams geoms
#' @param range A vector indicating the minimum and maximum sizes.
#' @inheritParams gauge_color
#' 
#' @examples
#' func <- cb("function(dist){
#'   if(dist > 60)
#'     return 20
#'   return 5
#' }")
#' 
#' g2(cars, asp(speed, dist, size = dist)) %>% 
#'   fig_point() %>% 
#'   gauge_size(callback = func)
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
#' func <- cb("function(dist){
#'   if(dist > 60)
#'     return 1
#'   return .3
#' }")
#' 
#' g2(cars, asp(speed, dist, opacity = dist)) %>% 
#'   fig_point() %>% 
#'   gauge_opacity(callback = func)
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
#' g2(mtcars, asp(mpg, qsec, shape = am)) %>% 
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
#' g2(mtcars, asp(mpg, qsec, shape = drat)) %>% 
#'   fig_point() %>% 
#'   gauge_label(textStyle = list(rotate = 30))
#' 
#' @export
gauge_label <- function(g2, ..., callback = NULL){
  opts <- list(callback = callback, cfg = list(...))
  make_scale(g2, vars = opts, method = "label")
}

#' Gauge label
#'
#' Gauge label.
#' 
#' @inheritParams geoms
#' @inheritParams gauge_color
#' 
#' @examples
#' g2(mtcars, asp(mpg, qsec, style = qsec)) %>% 
#'   fig_point() %>% 
#'   gauge_style(fill = "red")
#' 
#' @export
gauge_style <- function(g2, ...){
  opts <- list(cfg = list(...))
  make_scale(g2, vars = opts, method = "style")
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
#' g2(mtcars, asp(mpg, qsec, tooltip = drat, tooltip = qsec, color = qsec)) %>% 
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
