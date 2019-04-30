#' Motif
#' 
#' Customise the chart motif.
#' 
#' @inheritParams geoms
#' @param ... Motif options.
#' 
#' @examples
#' g <- g2(iris, plan(Sepal.Length, Sepal.Width)) %>% 
#'   fig_point(plan(color = Species)) 
#' 
#' g %>% darken()
#' 
#' g %>% motif(colors = c("red", "green", "blue"))
#' 
#' @name theme
#' @export
darken <- function(g2) {
  check_g2(g2)
  g2$x$opts$theme <- "dark"
  return(g2)
}

#' @rdname theme
#' @export
motif <- function(g2, ...){
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
set_font <- function(g2, font){
  check_g2(g2)
  if(missing(font))
    stop("missing font", call. = FALSE)
  
  g2$x$font <- font
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
set_renderer <- function(g2, renderer = c("svg", "canvas")){
  check_g2(g2)
  g2$x$renderer <- match.arg(renderer)
  return(g2)
}

#' Globals
#'
#' Set global options, all charts in session will use these options.
#' 
#' @param ... Passed to \code{\link{motif}}
#' @param font Passed to \code{\link{set_font}}
#' @param renderer Passed to \code{\link{set_renderer}}
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