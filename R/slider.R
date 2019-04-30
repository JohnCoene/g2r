#' Slider
#' 
#' Create a slider.
#' 
#' @inheritParams geoms
#' @param position Position of the slider relative to the chart.
#' @param id DOM id of slider.
#' 
#' @examples
#' range <- range(cars$speed)
#' 
#' g2(cars, plan(speed, dist)) %>% 
#'   fig_point() %>% 
#'   slider(start = range[1], end = range[2])
#' 
#' @export
slider <- function(g2, ..., position = c("bottom", "top"), id = NULL) {
  check_g2(g2)
  
  if(is.null(id))
    id <- .generate_id()

  g2$x$slider <- list(
    container = id,
    ...
  )
  g2

}