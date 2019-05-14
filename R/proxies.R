#' Manipulate Chart
#' 
#' Repaint, clear, render, or destroy the chart.
#' 
#' @param proxy A g2r proxy as returned by \code{\link{g2Proxy}}
#' 
#' @name chart-proxies
#' @export
repaint <- function(proxy) {
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  proxy$session$sendCustomMessage("repaint", data)
  
  return(proxy)
}

#' @rdname chart-proxies
#' @export
clear <- function(proxy) {
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  proxy$session$sendCustomMessage("clear", data)
  
  return(proxy)
}

#' @rdname chart-proxies
#' @export
render <- function(proxy) {
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  proxy$session$sendCustomMessage("render", data)
  
  return(proxy)
}

#' @rdname chart-proxies
#' @export
destroy <- function(proxy) {
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  proxy$session$sendCustomMessage("destroy", data)
  
  return(proxy)
}

#' Change Data
#' 
#' Change the data displayed on the chart.
#' 
#' @inheritParams chart-proxies
#' @param data A data.frame or \code{tibble}
#' @param ... Bare names of column to select.
#' @param figures Index of figures to apply the change to.
#' 
#' @note The variable names must be identical to the initial data.
#' 
#' @examples
#' library(shiny)
#' 
#' .make_data <- function(){
#'   dplyr::tibble(
#'     x = 1:20,
#'     y = runif(20)
#'   )
#' }
#' 
#' ui <- fluidPage(
#'   fluidRow(
#'     column(10, g2Output("chart")),
#'     column(2, actionButton("chg", "chg"))
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   output$chart <- renderG2({
#'     g2(.make_data(), asp(x, y)) %>% 
#'       fig_point()
#'   })
#' 
#'   observeEvent(input$chg, {
#'     g2Proxy("chart") %>% 
#'       change_data(.make_data(), x, y)
#'   })
#' }
#' 
#' if(interactive()) shinyApp(ui, server)
#'
#' @export
change_data <- function(proxy, data, ..., figures = NULL){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- data %>% 
    select(...) %>% 
    pmap(list)

  # -1 for JavaScript
  figures <- ifelse(is.null(figures), "*", figures - 1)

  msg <- list(id = proxy$id, data = data, figures = figures)
  proxy$session$sendCustomMessage("changeData", msg)

  return(proxy)
}

#' Change Size
#' 
#' Change size of chart.
#' 
#' @inheritParams chart-proxies
#' @param width,height New dimensions of chart.
#' 
#' @examples
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   sliderInput("width", "width", min = 100, max = 700, value = 250),
#'   g2Output("chart")
#' )
#' 
#' server <- function(input, output) {
#'   output$chart <- renderG2({
#'     g2(cars, asp(speed, dist)) %>% 
#'       fig_point()
#'   })
#' 
#'   observeEvent(input$width, {
#'     g2Proxy("chart") %>% 
#'       change_width(input$width)
#'   })
#' }
#' 
#' if(interactive()) shinyApp(ui, server)
#' 
#' @name change-size
#' @export
change_size <- function(proxy, width, height){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)

  msg <- list(id = proxy$id, opts = list(width = width, height = height))
  proxy$session$sendCustomMessage("changeSize", msg)

  return(proxy)
}

#' @rdname change-size
#' @export
change_width <- function(proxy, width){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)

  msg <- list(id = proxy$id, opts = width)
  proxy$session$sendCustomMessage("changeWidth", msg)

  return(proxy)
}

#' @rdname change-size
#' @export
change_height <- function(proxy, height){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)

  msg <- list(id = proxy$id, opts = height)
  proxy$session$sendCustomMessage("changeHeight", msg)

  return(proxy)
}

#' Convert
#' 
#' Convert chart to a dataURL or an image.
#' 
#' @inheritParams chart-proxies
#' @param name Name of file.
#' 
#' @examples
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("dl", "download"),
#'   g2Output("chart")
#' )
#' 
#' server <- function(input, output) {
#'   output$chart <- renderG2({
#'     g2(cars, asp(speed, dist)) %>% 
#'       fig_point()
#'   })
#' 
#'   observeEvent(input$dl, {
#'     g2Proxy("chart") %>% 
#'       download_image()
#'   })
#' }
#' if(interactive()) shinyApp(ui, server)
#' 
#' @name convert
#' @export
to_dataURL <- function(proxy){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id)
  proxy$session$sendCustomMessage("toDataURL", data)
  
  return(proxy)
}

#' @rdname convert
#' @export
download_image <- function(proxy, name = "g2r"){
  if (!"g2Proxy" %in% class(proxy)) 
    stop("must pass g2Proxy object", call. = FALSE)
  
  data <- list(id = proxy$id, name = name)
  proxy$session$sendCustomMessage("downloadImage", data)
  
  return(proxy)
}