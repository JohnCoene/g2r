#' General Options
#' 
#' Set general chart options.
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any options, see \href{https://www.yuque.com/antv/g2-docs-en/api-chart}{official documentation}.
#' 
#' @export
conf_globals <- function(g2, ...){
  g2$x$opts <- append(g2$x$opts, list(...))
  return(g2)
}

#' Legend
#' 
#' Configure the chart's legend.
#' 
#' @inheritParams geoms
#' @param var Bare column name of legend to apply changes to.
#' @param ... Configuration options or a logical indicating whether to show the legend.
#' 
#' @examples
#' g <- g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point()
#' 
#' g %>% conf_legend(dist, FALSE)
#' 
#' g %>% conf_legend(dist, position = "right")
#' 
#' @export
conf_legend <- function(g2, var, ...){
  if(missing(var))
    stop("missing variable", call. = FALSE)

  var <- rlang::enquo(var) %>% rlang::quo_name()

  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    legend <- list(var, list(...))
  else
    legend <- list(var, unlist(...))

  g2$x$legend <- append(g2$x$legend, list(legend))

  return(g2)
}

#' Tooltip
#' 
#' Configure the tooltip.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the tooltip.
#' 
#' @examples
#' g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point() %>% 
#'   conf_tooltip(crosshairs = "path")
#' 
#' @export
conf_tooltip <- function(g2, ...){
  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    g2$x$tooltip <- list(...)
  else
    g2$x$tooltip <- unlist(...)

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
#' 
#' @export
style <- function(g2, coord_type = c("rect", "polar", "theta", "helix"), coord_rotate = NULL, coord_sx = NULL, coord_sy = NULL,
  coord_reflect = NULL, coord_transpose = NULL, width = NULL, height = NULL, padding = NULL, bg_fill = NULL, bg_opacity = NULL, 
  bg_fill_opacity = NULL, bg_stroke = NULL, bg_stroke_opacity = NULL, bg_line_width = NULL, bg_radius = NULL, plot_fill = NULL,
  plot_fill_opacity = NULL, plot_stroke = NULL, plot_stroke_opacity = NULL, plot_opacity = NULL, plot_line_width = NULL, 
  plot_radius = NULL, fit = TRUE, animate = TRUE, pixel_ratio = NULL, renderer = c("canvas", "svg")){

  # coord
  g2$x$coord <- list(type = match.arg(coord_type)) 
  if(!is.null(coord_rotate)) g2$x$coordRotate <- coord_rotate
  if(!is.null(coord_sx) && !is.null(coord_sy)) g2$x$coordScale <- list(coord_sx, coord_sx)
  if(!is.null(coord_reflect)) g2$x$coordReflect <- coord_reflect
  if(!is.null(coord_transpose)) g2$x$coordTranspose <- coord_transpose

  # chart options
  if(!is.null(width)) g2$x$opts$width <- width
  if(!is.null(height)) g2$x$opts$height <- height
  if(!is.null(padding)) g2$x$opts$padding <- padding
  if(!is.null(pixel_ratio)) g2$x$opts$pixelRatio <- pixel_ratio
  g2$x$opts$renderer <- match.arg(renderer)
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
  g2$x$opts$background <- bg

  # plot
  plot <- list(
    fill = bg_fill,
    fillOpacity = bg_fill_opacity,
    stroke = bg_stroke,
    strokeOpacity = bg_stroke_opacity,
    opacity = bg_opacity,
    lineWidth = bg_line_width,
    radius = bg_radius
  )
  plot <- plot[lapply(plot, length) > 0]
  g2$x$opts$plotBackground <- plot
  
  return(g2)
}

#' Coordinates
#' 
#' Configure chart coordinates axis.
#' 
#' @inheritParams geoms
#' @param type Type of coordinate axis.
#' @param angle Angle of axis rotation.
#' @param sx,sy Scale of axis along \code{x} and \code{y} axis.
#' @param axis Axis to reflect (reverse).
#' @param coord,rotate,reflect,transpose Corresponds to arguments of other functions listed here.
#' @param ... Any other options.
#' 
#' @examples
#' g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point() %>% 
#'   coord_type("helix")
#' 
#' g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point(coord("helix")) 
#' 
#' @name coord
#' @export
coord_type <- function(g2, type = c("rect", "polar", "theta", "helix"), ...){
  g2$x$coord <- list(
    type = match.arg(type),
    opts = list(...)
  ) 
  return(g2)
}

#' @rdname coord
#' @export
coord_rotate <- function(g2, angle = 90){
  if(!length(g2$x$coord))
    g2 <- coord_type(g2)
  g2$x$coordRotate <- angle
  return(g2)
}

#' @rdname coord
#' @export
coord_scale <- function(g2, sx, sy){
  if(missing(sx) || missing(sy))
    stop("missing sx or sy", call. = FALSE)

  if(!length(g2$x$coord))
    g2 <- coord_type(g2)
  g2$x$coordScale <- list(sx, sy)
  return(g2)
}

#' @rdname coord
#' @export
coord_reflect <- function(g2, axis = "xy"){
  if(!length(g2$x$coord))
    g2 <- coord_type(g2)
  g2$x$coordReflect <- axis
  return(g2)
}

#' @rdname coord
#' @export
coord_transpose <- function(g2){
  if(!length(g2$x$coord))
    g2 <- coord_type(g2)
  g2$x$coordTranspose <- TRUE
  return(g2)
}

#' @rdname coord
#' @export
coord <- function(type = c("rect", "polar", "theta", "helix"), rotate = NULL, sx = NULL, sy = NULL, reflect = NULL, transpose = FALSE, ...){
  opts <- list(
    NAME = "coord",
    opts = list(
      type = match.arg(type),
      actions = list(),
      ...
    )
  ) 

  if(!is.null(rotate))
    opts$opts$actions <- append(opts$opts$actions, list(list("rotate", rotate)))

  if(!is.null(reflect))
    opts$opts$actions <- append(opts$opts$actions, list(list("reflect", reflect)))

  if(!is.null(sx) && !is.null(sy))
    opts$opts$actions <- append(opts$opts$actions, list(list("scale", sx, sy)))

  if(isTRUE(transpose))
    opts$opts$actions <- append(opts$opts$actions, list(list("transpose")))

  .construct_options(opts, "coord")
}
