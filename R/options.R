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
#' @param ... Configuration options or a logical indicating whether to show the legend.
#' 
#' @examples
#' g <- g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point()
#' 
#' g %>% conf_legend(FALSE)
#' 
#' g %>% conf_legend(position = "right")
#' 
#' @export
conf_legend <- function(g2, ...){
  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    g2$x$legend <- list(...)
  else
    g2$x$legend <- unlist(...)

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
#' @param ... Any other options.
#' 
#' @examples
#' g2(cars, plan(speed, dist, color = dist)) %>% 
#'   fig_point() %>% 
#'   conf_coord("helix")
#' 
#' @name coord
#' @export
conf_coord <- function(g2, type = c("rect", "polar", "theta", "helix"), ...){
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
    g2 <- conf_coord(g2)
  g2$x$coordRotate <- angle
  return(g2)
}

#' @rdname coord
#' @export
coord_scale <- function(g2, sx, sy){

  if(missing(sx) || missing(sy))
    stop("missing sx or sy", call. = FALSE)

  if(!length(g2$x$coord))
    g2 <- conf_coord(g2)
  g2$x$coordScale <- list(sx, sy)
  return(g2)
}

#' @rdname coord
#' @export
coord_reflect <- function(g2, axis = "xy"){
  if(!length(g2$x$coord))
    g2 <- conf_coord(g2)
  g2$x$coordReflect <- axis
  return(g2)
}

#' @rdname coord
#' @export
coord_transpose <- function(g2){
  if(!length(g2$x$coord))
    g2 <- conf_coord(g2)
  g2$x$coordTranspose <- TRUE
  return(g2)
}

#' Tooltip
#' 
#' Configure the tooltip.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the legend.
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