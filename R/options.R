#' General Options
#' 
#' Set general chart options.
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any options, see \href{https://www.yuque.com/antv/g2-docs-en/api-chart}{official documentation}.
#' 
#' @export
g2_options <- function(g2, ...){
  g2$x$opts <- append(g2$x$opts, list(...))
  g2
}