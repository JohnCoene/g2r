#' Plane
#' 
#' This function is similar to the \code{facet_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param facets A vector of dimensions, may include \code{NULL}.
#' @param type The type of facet to user.
#' @param ... Any other option.
#' 
#' @name plane
#' @export
plane_wrap <- function(g2, facets, type = c("list", "rect", "circle", "tree", "mirror", "matrix"), ...) {
  
  if(missing(facets))
    stop("missing facets", call. = FALSE)

  # add to main mapping
  facets_aes <- keep(facets, function(x){
    if(!is.null(x))
      return(TRUE)
    return(FALSE)
  }) %>% 
    rlang::syms() %>% 
    rlang::quos_auto_name()
  
  g2$x$mapping <- append(g2$x$mapping, facets_aes)

  g2$x$facet <- list(
    type = match.arg(type),
    opts = list(
      fields = facets %>% map(js_null),
      ...
    )
  )

  return(g2)
}
