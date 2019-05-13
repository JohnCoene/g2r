#' Brush
#' 
#' Add brush interactions.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' range <- range(cars$speed)
#' 
#' g2(cars, asp(speed, dist)) %>% 
#'   fig_point() %>% 
#'   interact_brush()
#' 
#' @name interact
#' @export
interact_brush <- function(g2) {
  check_g2(g2)
  g2$x$brush <- TRUE
  g2
}

#' @name interact
#' @export
interact_slider <- function(g2, ...) {
  check_g2(g2)
  g2$x$slider <- list(...)
  g2
}

#' @name interact
#' @export
interact_zoom <- function(g2, ...) {
  check_g2(g2)
  g2$x$zoom <- list(...)
  g2
}

#' @name interact
#' @export
interact_drag <- function(g2, ...) {
  check_g2(g2)
  g2$x$drag <- list(...)
  g2
}