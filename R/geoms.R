#' Bar
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any option or aesthetic.
#' @param data A \code{data.frame} containing data to chart.
#' @param inherit_gaes Whether to inherit aesthetics from \code{g2r}.
#' 
#' @examples
#' cars %>% 
#'   g2r(gaes(speed, dist)) %>% 
#'   g2_point()
#' 
#' @rdname geoms
#' @export
g2_interval <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "interval", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_line <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "line", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_point <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "point", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_path <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "path", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_area <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "area", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_polygon <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "polygon", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_schema <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "schema", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_edge <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "edge", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_heatmap <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "heatmap", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_point_jitter <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "pointJitter", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_point_stack <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "pointStack", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_point_dodge <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "pointDodge", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_interval_stack <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "intervalStack", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_interval_dodge <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "intervalDodge", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_interval_symmetric <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "intervalSymmetric", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_area_stack <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "areaStack", inherit_aes = inherit_gaes)
}

#' @rdname geoms
#' @export
g2_schema_dodge <- function(g2, ..., data = NULL, inherit_gaes = TRUE) {
  make_geom(g2, ..., data = data, chart_type = "schemaDodge", inherit_aes = inherit_gaes)
}

make_geom <- function(g2, ..., data = NULL, chart_type = "interval", inherit_aes = TRUE) {
  specs <- list(chart_type = chart_type, inherit_aes = inherit_aes)

  aes <- get_aes(...)
  
  if(length(aes))
    specs$mapping <- aes
  
  if(!is.null(data))
    specs$data <- keep_data(data)

  g2$x$layers <- append(g2$x$layers, list(specs))
  g2
}
