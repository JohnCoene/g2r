#' Histogram
#' 
#' Create a histogram.
#' 
#' @inheritParams geoms
#' @param bin_width Width of bins.
#' 
#' @export
fig_histogram <- function(g2, ..., bin_width = 1, data = NULL, inherit_plan = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_plan, ...)
  aes <- aes[names(aes) == "y" || names(aes) == "color"]

  if(rlang::is_empty(aes))
    stop("no `y` or `color` aspect", call. = FALSE)

  if(is.null(data))
    data <- g2$x$data

  if(length(aes$color))
    data <- alter(data, type = "bin.histogram", groupBy = rlang::quo_name(aes$color), fields = rlang::quo_name(aes$y), binWidth = bin_width, .rows = FALSE)
  else
    data <- alter(data, type = "bin.histogram", fields = rlang::quo_name(aes$y), binWidth = bin_width, .rows = FALSE)

  print(data)

}
