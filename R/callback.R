#' Callback
#' 
#' Create a callback function.
#' 
#' @param js A JavaScript function as string.
#' 
#' @export
cb <- function(js) {
  if(missing(js))
    stop("missing JavaScript js", call. = FALSE)

  htmlwidgets::JS(js)
}