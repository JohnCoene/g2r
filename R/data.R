#' Temperature
#'
#' Temperatures in London and Berlin, \emph{probably made up}.
#'
#' @format A \code{tibble} of 3 columns and 16 rows.
#' \describe{
#'   \item{\code{city}}{ City where temperature was taken}
#'   \item{\code{month}}{ Month}
#'   \item{\code{temp}}{ Temperature in Celsius}
#' }
#' @source \url{https://antv.alipay.com/zh-cn/g2/3.x/demo/column/grouped-column.html}
"temp"

#' Fruits
#'
#' Fruits someone probably bought during a given week.
#'
#' @format A \code{tibble} of 2 columns and 4 rows.
#' \describe{
#'   \item{\code{fruit}}{ Fruit}
#'   \item{\code{value}}{ Percentage of fruit bought}
#' }
#' @source Totally made up
"fruits"

#' Dataset
#' 
#' Dataset with state management \url{https://github.com/antvis/data-set}
#' 
#' @param data A data.frame to alter.
#' @param ... Altering options.
#' @param .rename Whether to rename the columns of the altered data.frame (recommended).
#' @param source A \code{list} of source options.
#' @param .rows Wheter to return the whole altered object or just the rows.
#' 
#' @examples
#' head(fruits)
#' 
#' fr <- fruits %>% 
#'   dplyr::mutate(value = value * 100) %>% 
#'   dplyr::select(name = fruit, value) %>% 
#'   alter(type = "waffle", maxCount = 500, rows = 12) 
#' 
#' head(fr)
#' 
#' g2(fr, asp(x, y, shape = "square", size = 10, color = name)) %>% 
#'   fig_point(axes = FALSE) %>% 
#'   gauge_axis(x, FALSE)
#' @export
alter <- function(data, ..., source = NULL, .rename = TRUE, .rows = TRUE){
  if(missing(data))
    stop(" missing data", call. = FALSE)
  ctx <- V8::new_context()
  ctx$source(system.file("datasets.min.js", package = "g2r"))
  ctx$assign("data", data)

  if(!is.null(source)){
    ctx$assign("src", source)
    ctx$eval("var dv = new DataSet.View().source(data, src);")
  } else {
    ctx$eval("var dv = new DataSet.View().source(data);")
  }

  opts <- list(...)
  if(length(opts)){
    ctx$assign("opts", opts)
    ctx$eval("var dv = dv.transform(opts);")
  }
  
  if(.rows)
    transformed <- ctx$get("dv.rows")
  else
    transformed <- ctx$get("dv")
  if(.rename){
    n <- tryCatch(gsub("^_", "", names(transformed)))
    if(length(n))
      transformed <- set_names(transformed, n)
  }
  return(transformed)
}