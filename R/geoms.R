#' Bar
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any option or aesthetic.
#' @param data A \code{data.frame} containing data to chart.
#' 
#' @export
g2_bar <- function(g2, ..., data = NULL) {
  make_geom(g2, ..., data, chart_type = "interval")
}

#' @export
g2_line <- function(g2, ..., data = NULL) {
  make_geom(g2, ..., data, chart_type = "line")
}

#' @export
g2_point <- function(g2, ..., data = NULL) {
  make_geom(g2, ..., data, chart_type = "point")
}

make_geom <- function(g2, ..., data = NULL, chart_type = "interval") {
  specs <- list(chart_type = chart_type)

  aes <- get_aes(...)
  
  if(length(aes))
    specs$mapping <- aes
  
  if(!is.null(data))
    specs$data <- keep_data(data)

  g2$x$layers <- append(g2$x$layers, list(specs))
  g2
}
