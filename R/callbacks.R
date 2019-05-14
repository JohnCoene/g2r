#' Plot Callbacks
#' 
#' Add basic plot callbacks, will only be available in Shiny.
#' 
#' @inheritParams geoms
#' 
#' @details
#' This function will add the following callbacks
#' \itemize{
#'   \item{\code{plot_enter}}
#'   \item{\code{plot_leave}}
#'   \item{\code{plot_click}}  
#' }
#' 
#' @return A \code{list} containing \code{x} and \code{y} pixel coordinates of clicked/entered/left.
#' 
#' @export
plot_callbacks <- function(g2) {
  check_g2(g2)
  g2$x$plotCallbacks <- TRUE
  return(g2)
}


snap_records <- function(g2, figure = NULL, action = "click") {
  check_g2(g2)
  on <- paste0(c(figure, action), collapse = ":")
  opts <- list(on = on, action = match.arg(action))
  g2$x$getSnapRecords <- append(g2$x$getSnapRecords, list(opts))
  return(g2)
}

