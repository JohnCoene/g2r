#' Map
#'
#' Add a map layer.
#' 
#' @inheritParams geoms
#' @param map Name of country as defined by ISO Standard 3166-1, if \code{NULL} uses world map.
#' @param axes,tooltip Whether to show the tooltip and the axis.
#' @param ... Additional options passed to \code{\link{fig_polygon}}.
#' 
#' @examples
#' \dontrun{
#' map <- get_map("fra")
#' 
#' g2(map, asp(longitude, latitude)) %>% 
#'   fig_polygon(axes = FALSE, tooltip = FALSE) %>% 
#'   style(height = cb("window.innerHeight"))
#' 
#' g2() %>% 
#'   fig_map(map = "fra") %>% 
#'   style(height = cb("window.innerHeight"))
#' }
#' 
#' @name fig_map
#' @export
fig_map <- function(g2, ..., map = NULL, axes = FALSE, tooltip = FALSE) {
  
  if(inherits(map, "character") || is.null(map))
    map <- get_map(map)

  fig_polygon(g2, asp(longitude, latitude), data = map, axes = axes, tooltip = tooltip, inherit_plan = FALSE, ...) %>% 
    sync(longitude, latitude)
}

#' @name fig_map
#' @export
get_map <- function(map = NULL){
  path <- system.file("world.geo.json", package = "g2r") # defaults to world map
  if(!is.null(map))
    path <- paste0("https://raw.githubusercontent.com/mledoze/countries/master/data/", tolower(map), ".geo.json")
  map <- jsonlite::read_json(path)
  alter(map, type = "map", source = list(type = "GeoJSON"), .rename = FALSE)
}