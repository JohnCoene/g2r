#' Axis
#' 
#' Scale and modify axis.
#' 
#' @inheritParams geoms
#' @param ... Configuration options or a logical indicating whether to show the axis.
#' @param var Variable to scale.
#' @param type Type of data plotted against the axis \code{identity}, \code{linear}, \code{cat}, \code{time}, \code{timeCat}, \code{log}, \code{pow}. 
#'   See type section for details.
#' @param nice The default is \code{TRUE}, which is used to optimize the range of values so that the plotted axe is evenly distributed. 
#'   For example, the range of raw data is \code{c(3, 97)}, and if nice is true, the range of values is adjusted to \code{c(0, 100]}.
#' @param range The range of output data, defaults to \code{c(0, 1)}.
#' @param ticks,tick_count,tick_interval Number of ticks to show or interval between ticks, only one of those can be set.
#' @param min,max Minimum and Maximum of axes.
#' @param formatter A callback function as returned by \code{\link{cb}}.
#' @param base,exponent Used for \code{type} \code{log} and \code{pow}.
#' @param values Values of axes to use for categorical variables.
#' @param sync Wehter to unify axes scales across \code{\link{planes}}.
#' @param mask Date time format.
#' @param figure Name of figure to apply axis to, if \code{NULL} it is applied to all figures.
#'
#' @section Type:
#' \itemize{
#'   \item{\code{identity}: Constant.}
#'   \item{\code{linear}: Continuous variable.}
#'   \item{\code{cat}: Categorical variable.}
#'   \item{\code{time}: Continous time variable.}
#'   \item{\code{timeCat}: Non-continuous time variable.}
#'   \item{\code{log}: Logarithmic data.}
#'   \item{\code{pow}: Exponential data.}  
#' }
#'
#' @examples
#' g <- g2(cars, asp(speed, dist)) %>% 
#'   fig_point() 
#' 
#' g %>% gauge_x_linear(min = 0) 
#' g %>% gauge_y_log(title = "Log")
#' g %>% gauge_x_linear(tick_count = 4) 
#' 
#' @name gauge_axis
#' @export
gauge_axis <- function(g2, var, ..., nice = TRUE, range = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, 
  min = NULL, max = NULL, type = NULL, formatter = NULL, sync = TRUE, values = NULL, 
  base = NULL, exponent = NULL, mask = NULL, figure = NULL){

  var <- rlang::enquo(var) %>% 
    rlang::quo_name() %>% 
    unlist() %>% 
    unname() 

  axes(
    g2, var, ..., nice = nice, range = range, ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, 
    min = min, max = max, type = type, formatter = formatter, sync = sync, values = values, 
    base = base, exponent = exponent, figure = figure
  )
}

#' @rdname gauge_axis
#' @export
gauge_x_identity <- function(g2, ..., figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "identity", figure = figure)
  } 

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_identity <- function(g2, ..., figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "identity", figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_linear <- function(g2, ..., nice = TRUE, range = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, 
  min = NULL, max = NULL, formatter = NULL, sync = TRUE, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "linear", nice = nice, range = range, ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, 
      min = min, max = max, formatter = formatter, sync = sync, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_linear <- function(g2, ..., nice = TRUE, range = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, 
  min = NULL, max = NULL, formatter = NULL, sync = TRUE, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "linear", nice = nice, range = range, ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, 
      min = min, max = max, formatter = formatter, sync = sync, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_discrete <- function(g2, ...,  sync = TRUE, range = NULL, formatter = NULL, ticks = NULL, tick_count = NULL, values = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "cat", sync = sync, range = range, formatter = formatter, ticks = ticks, tick_count = tick_count, values = values, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_discrete <- function(g2, ..., sync = TRUE, range = NULL, formatter = NULL, ticks = NULL, tick_count = NULL, values = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "cat", sync = sync, range = range, formatter = formatter, ticks = ticks, tick_count = tick_count, values = values, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_log <- function(g2, ..., base = 2, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "log", base = base, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_log <- function(g2, ..., base = 2, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "log", base = base, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_pow <- function(g2, ..., exponent = 2, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "pow", exponent = exponent, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_pow <- function(g2, ..., exponent = 2, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "pow", exponent = exponent, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_time <- function(g2, ..., mask = NULL, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "time", mask = mask, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_time <- function(g2, ..., mask = NULL, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "time", mask = mask, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, tick_interval = tick_interval, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_x_discrete_time <- function(g2, ..., mask = NULL, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "x")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "timeCat", mask = mask, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_y_discrete_time <- function(g2, ..., mask = NULL, nice = TRUE, sync = TRUE, formatter = NULL, ticks = NULL, tick_count = NULL, figure = NULL){

  var <- .get_map(g2$x$mapping, "y")
  
  for(v in var){
    g2 <- axes(g2, v, ..., type = "timeCat", mask = mask, nice = nice, sync = sync, formatter = formatter, 
      ticks = ticks, tick_count = tick_count, figure = figure)
  }

  return(g2)
}

#' @rdname gauge_axis
#' @export
gauge_all_axis <- function(g2, ...){
  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    g2$x$axes <- list(...)
  else
    g2$x$axes <- unlist(...)
  g2
}

#' @rdname gauge_axis
#' @export
hide_axes <- function(g2){
  check_g2(g2)
  g2$x$allAxes <- FALSE
  g2
}

#' @rdname gauge_axis
#' @export
hide_axis <- function(g2){
  .Deprecated("hide_axes")
}

axes <- function(g2, var, ..., nice = TRUE, range = NULL, ticks = NULL, tick_count = NULL, tick_interval = NULL, 
  min = NULL, max = NULL, type = NULL, formatter = NULL, sync = TRUE, values = NULL, 
  base = NULL, exponent = NULL, mask = NULL, figure = NULL){
  check_g2(g2)

  if(is.null(figure))
    figure <- ""

  check_g2(g2)

  opts <- list(
    nice = nice, range = range, ticks = ticks, tickCount = tick_count, tickInterval = tick_interval, 
    min = min, max = max, type = type, formatter = formatter, sync = sync,
    base = base, exponent = exponent, mask = mask
  )

  g2$x$dataOpts <- upsert_scale(var, g2$x$dataOpts, opts)

  var <- rlang::enquo(var) %>% 
    rlang::quo_name()

  var <- list(var = var)

  lgl <- tryCatch(rlang::is_logical(...), error = function(e) NULL)

  if(is.null(lgl))
    var$opts <- list(...)
  else
    var$opts <- unlist(...)

  var$figure <- figure

  g2$x$axis <- append(g2$x$axis, list(var))
  
  return(g2)
}