#' Firgures
#' 
#' Figures, equivalent to \code{ggplot2}'s geometries.
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any option, aesthetic (\code{\link{plan}}), or animation (\code{\link{Animation}}).
#' @param data A \code{data.frame} containing data to chart.
#' @param inherit_plan Whether to inherit aesthetics from \code{g2r}.
#' 
#' @examples
#' iris %>% 
#'   g2(plan(Sepal.Length, Sepal.Width, color = Species)) %>% 
#'   fig_point()
#' 
#' @name geoms
#' @export
fig_interval <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "interval", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_line <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "line", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_point <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "point", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_path <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "path", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_area <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "area", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_polygon <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "polygon", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_schema <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "schema", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_edge <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "edge", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_heatmap <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "heatmap", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_point_jitter <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "pointJitter", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_point_stack <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "pointStack", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_point_dodge <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "pointDodge", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_interval_stack <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "intervalStack", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_interval_dodge <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "intervalDodge", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_interval_symmetric <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "intervalSymmetric", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_area_stack <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "areaStack", inherit_aes = inherit_plan, name = name)
}

#' @rdname geoms
#' @export
fig_schema_dodge <- function(g2, ..., data = NULL, inherit_plan = TRUE, name = NULL) {
  make_geom(g2, ..., data = data, chart_type = "schemaDodge", inherit_aes = inherit_plan, name = name)
}

make_geom <- function(g2, ..., data = NULL, chart_type = "interval", inherit_aes = TRUE, name = NULL) {
  
  if(is.null(name))
    name <- ""

  if(!is.null(data) && isTRUE(inherit_aes))
    inherit_aes <- FALSE

  specs <- list(chart_type = chart_type, inherit_aes = inherit_aes, name = name)

  specs$data <- keep_data(data)
  aes <- get_aes(...)
  
  if(length(aes))
    specs$mapping <- aes

  anim <- get_animation(...)

  specs$animation <- anim

  g2$x$layers <- append(g2$x$layers, list(specs))
  g2
}

