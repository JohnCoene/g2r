#' Info
#' 
#' Add informational elements to the chart.
#' 
#' @inheritParams geoms
#' @param ... Info options.
#' @param figure A figure name to apply the informational element to, if
#'   \code{NULL} then it is applied to al.
#' 
#' @examples
#' g2(mtcars, plan(mpg, qsec)) %>% 
#'   fig_point() %>% 
#'   info_marker(
#'     content = "Marker",
#'     position = c(20, 20)
#'   )
#' 
#' @name info
#' @export
info_line <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "line",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_text <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "text",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_image <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "image",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_region <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "region",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_html <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "html",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_arc <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "arc",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_marker <- function(g2, ..., figure = NULL) {
  if(is.null(figure))
    figure <- ""
  guide <- list(
    name = figure,
    type = "dataMarker",
    opts = list(...)
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}
