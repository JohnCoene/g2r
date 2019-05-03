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
#' g <- g2(cars, asp(speed, dist, color = dist)) %>% 
#'   fig_point()
#' 
#' g %>% conf_legend(dist, FALSE)
#' 
#' g %>% conf_legend(dist, position = "right")
#' 
#' @name legend
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

#' @rdname legend
#' @export
leg <- function(var, ...){
  
  if(missing(var))
    stop("missing variable", call. = FALSE)

  var <- rlang::enquo(var) %>% rlang::quo_name()

  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    leg <- list(list(...))
  else
    leg <- list(unlist(...))

  names(leg) <- var

  opts <- list(
    NAME = "legends"
  )

  opts$opts <- leg

  .construct_options(opts, "legend")
}

#' Tooltip
#' 
#' Configure the tooltip.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the tooltip.
#' 
#' @examples
#' g2(cars, asp(speed, dist, color = dist)) %>% 
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

#' Renderer
#' 
#' Define renderer.
#' 
#' @inheritParams geoms
#' @param renderer Renderer, \code{canvas} or \code{svg}.
#' 
#' @note The "g" in g2r stands for svg.
#' 
#' @export
conf_renderer <- function(g2, renderer = c("svg", "canvas")){
  check_g2(g2)
  g2$x$renderer <- match.arg(renderer)
  return(g2)
}

#' Globals
#'
#' Set global options, all charts in session will use these options.
#' 
#' @param ... Passed to \code{\link{custom_motif}}
#' @param font Passed to \code{\link{conf_font}}
#' @param renderer Passed to \code{\link{conf_renderer}}
#' 
#' @examples
#' g2r_globals(font = "Comic Sans")
#' 
#' @export
g2r_globals <- function(..., font = NULL, renderer = NULL){
  theme <- list(...)
  if(length(theme) >= 1)
    options(g2_motif = theme)
  
  if(!is.null(font))
    options(g2_font = font)

  if(!is.null(renderer))
    options(g2_renderer = renderer)
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
#' g2(cars, asp(speed, dist, color = dist)) %>% 
#'   fig_point() %>% 
#'   coord_type("helix")
#' 
#' g2(cars, asp(speed, dist, color = dist)) %>% 
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
