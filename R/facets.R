#' Plane
#' 
#' Plane wrap is similar to the \code{facet_*} family of functions from the \code{ggplot2} package.
#' 
#' @export
plane_wrap <- function(g2, facets, type = c("rect", "list", "circle", "tree", "mirror", "matrix"), ...) {
  
  if(missing(facets))
    stop("missing facets", call. = FALSE)

  # add to main mapping
  facets_aes <- keep(facets, function(x){
    if(!inherits(x, "JS_EVAL"))
      return(TRUE)
    return(FALSE)
  }) %>% 
    rlang::syms() %>% 
    rlang::quos_auto_name()
  g2$x$mapping <- append(g2$x$mapping, facets_aes)

  g2$x$facet <- list(
    type = match.arg(type),
    facet = facets,
    ...
  )

  g2
}
