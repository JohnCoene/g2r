#' Firgures
#' 
#' Figures, equivalent to \code{ggplot2}'s geometries.
#' 
#' @param g2 An object of class \code{g2r} as returned by \code{\link{g2r}}.
#' @param ... Any option, aesthetic (\code{\link{plan}}), or animation (\code{\link{Animation}}).
#' @param data A \code{data.frame} containing data to chart.
#' @param inherit_plan Whether to inherit aesthetics from \code{g2r}.
#' @param name Name of figure, useful to apply functions to specific figures.
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
  if(is.null(name)) name <- ""

  # force FALSE when new dataset passed
  if(!is.null(data)) inherit_aes <- FALSE

  view <- list(
    type = chart_type, 
    inherit_aes = inherit_aes, 
    name = name, 
    opts = list(
      animate = get_animation(...)
    )
  )

  additional_opts <- rm_anim_aes(...)
  if(length(additional_opts))
    view$opts <- append(view$opts, additional_opts)

  view$data <- keep_data(data)
  aes <- get_aes(...)
  
  if(length(aes))
    view$mapping <- aes

  g2$x$layers <- append(g2$x$layers, list(view))
  g2
}

