#' Initialise
#'
#' Initialise a \code{g2r} chart.
#' 
#' @param data A \code{data.frame} containing data to chart.
#' @param width,height Dimensions of chart.
#' @param elementId ID of \code{DOM} container.
#' @param ... Any general options.
#' @param asp Mapping aesthetics as returned by \code{\link{asp}}.
#' @param render Whether to render the chart.
#'
#' @import htmlwidgets
#' @import purrr
#' @import dplyr
#' @importFrom graphics hist boxplot
#' @importFrom stats density fitted
#'
#' @examples
#' g2(cars, asp(speed, dist)) %>% 
#'   fig_point()
#' 
#' @name g2r
#' @export
g2 <- function(data = NULL, asp = NULL, ..., render = TRUE, 
  width = NULL, height = NULL, elementId = NULL) {

  x = list(
    opts = list(
      ...,
      padding = "auto",
      forceFit = TRUE
    ),
    render = render,
    mapping = asp,
    data = NULL,
    dataOpts = list()
  )

  if(!is.null(data)) x$data <- keep_data(data)

  if(!is.null(asp)){
    asp <- asp %>% 
      keep(rlang::is_quosure)
    sync <- sync_it(asp, TRUE)
    x$dataOpts <- upsert_data_opts(list(), sync)
  }

  htmlwidgets::createWidget(
    name = 'g2r',
    x,
    width = width,
    height = height,
    package = 'g2r',
    elementId = elementId,
    preRenderHook = render_g2r,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 0
    )
  )
}

#' Shiny bindings
#'
#' Output and render functions for using g2r within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a g2r.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param session A valid shiny session.
#'
#' @name g2r-shiny
#'
#' @export
g2Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'g2r', width, height, package = 'g2r')
}

#' @rdname g2r-shiny
#' @export
renderG2 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, g2Output, env, quoted = TRUE)
}

#' @rdname g2r-shiny
#' @export
g2Proxy <- function(outputId, session = shiny::getDefaultReactiveDomain()){
  
  proxy <- list(id = outputId, session = session)
  class(proxy) <- "g2Proxy"
  
  return(proxy)
}