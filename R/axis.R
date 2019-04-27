#' Axis
#' 
#' Scale and modify axis.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the axis.
#' @param var Variable to scale.
#'
#' @examples
#' g2(cars, plan(speed, dist)) %>% 
#'   fig_point() %>% 
#'   gauge_axis(speed, FALSE) 
#' 
#' @export
gauge_axis <- function(g2, var, ...){
  if(missing(var))
    stop("missing var", call. = FALSE)

  var <- rlang::enquo(var) %>% 
    rlang::quo_name()

  var <- list(var = var)

  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(!length(g2$x$axis))
    g2$x$axis <- list()

  if(is.null(lgl))
    var$opts <- list(...)
  else
    var$opts <- unlist(...)

  g2$x$axis <- append(g2$x$axis, list(var))

  return(g2)
}
