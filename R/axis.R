#' Axis
#' 
#' Scale and modify axis.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the axis.
#' @param var Variable to scale.
#' @param figure Name of figure to apply axis to, if \code{NULL} it is applied to all figures.
#'
#' @examples
#' g2(cars, plan(speed, dist)) %>% 
#'   fig_point() %>% 
#'   gauge_axis(speed, FALSE) 
#' 
#' @export
gauge_axis <- function(g2, var, ..., figure = NULL){
  if(missing(var))
    stop("missing var", call. = FALSE)

  if(is.null(figure))
    figure <- ""

  var <- rlang::enquo(var) %>% 
    rlang::quo_name()

  var <- list(var = var)

  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    var$opts <- list(...)
  else
    var$opts <- unlist(...)

  var$figure <- figure

  g2$x$axis <- append(g2$x$axis, list(var))

  return(g2)
}
