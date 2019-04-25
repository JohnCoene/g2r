#' Initialise
#'
#' Initialise a g2 chart.
#' 
#' @param data A \code{data.frame} containing data to chart.
#' @param width,height Dimensions of chart.
#' @param elementId ID of \code{DOM} container.
#' @param ... Any general options.
#' @param mapping Mapping aesthetics as returned by \code{\link{gaes}}.
#' @param render Whether to render the chart.
#'
#' @import htmlwidgets
#' @import purrr
#' @import dplyr
#'
#' @examples
#' cars %>% 
#'   g2r(gaes(speed, dist)) %>% 
#'   g2_point()
#' 
#' @export
g2r <- function(data = NULL, mapping = NULL, ..., render = TRUE, width = NULL, height = NULL, 
  elementId = NULL) {

  x = list(
    opts = list(
      ...,
      forceFit = TRUE
    ),
    scales = list(),
    mapping = mapping,
    render = render,
    data = NULL
  )

  if(!is.null(data))
    x$data <- keep_data(data)

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

#' Shiny bindings for g2r
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
#'
#' @name g2r-shiny
#'
#' @export
g2rOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'g2r', width, height, package = 'g2r')
}

#' @rdname g2r-shiny
#' @export
renderG2r <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, g2rOutput, env, quoted = TRUE)
}
