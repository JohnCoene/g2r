#' Info
#' 
#' Add informational elements to the chart.
#' 
#' @inheritParams geoms
#' @param ... Info options.
#' @param figures Vector of names or indices of figure(s) to apply the information to, if \code{NULL} applies only to the first figure.
#' 
#' @examples
#' g2(mtcars, plan(mpg, qsec)) %>% 
#'   fig_point() %>% 
#'   info_data_marker(
#'     content = "Marker",
#'     position = c(20, 20)
#'   )
#' 
#' @name info
#' @export
info_line <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "line",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_text <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "text",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_image <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "image",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_region <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "region",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_html <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "html",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_arc <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "arc",
      ...     
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_data_marker <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "dataMarker",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_region_filter <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "regionFilter",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}

#' @rdname info
#' @export
info_data_region <- function(g2, ..., figures = NULL) {
  guide <- list(
    figures = figures,
    guide = list(
      type = "dataRegion",
      ...
    )
  )
  g2$x$guides <- append(g2$x$guides, list(guide))
  return(g2)
}