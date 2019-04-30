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
