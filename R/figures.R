#' Histogram
#' 
#' Create a histogram.
#' 
#' @inheritParams geoms
#' @param bin_width Width of bins.
#' 
#' @examples
#' g2(iris, asp(Sepal.Length, color = Species))  %>% 
#'   fig_histogram(bin_width = .3)
#' 
#' @export
fig_histogram <- function(g2, ..., bin_width = 1, data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "color")]

  if(rlang::is_empty(aes))
    stop("no `x` or `color` aspect", call. = FALSE)

  if(is.null(data))
    data <- g2$x$data

  if(length(aes$color))
    data <- alter(
      data,
      type = "bin.histogram",
      field = rlang::quo_name(aes$x),
      groupBy = list(rlang::quo_name(aes$color)),
      binWidth = bin_width,
      as = list("x", "y")
    )
  else
    data <- alter(
      data,
      type = "bin.histogram",
      field = rlang::quo_name(aes$x),
      binWidth = bin_width,
      as = list("x", "y")
    )

  aes$x <- "x"
  aes$y <- "y"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "intervalStack", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Density
#' 
#' Create a density plot.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' g2(iris, asp(Petal.Length, color = Species))  %>% 
#'   fig_density()
#'  
#' @export
fig_density <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "color")]

  if(rlang::is_empty(aes))
    stop("no `x` or `color` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(length(aes$color))
    data <- group_split(data, !!aes$color)
  else
    data <- list(data)

  data <- data %>% 
    map(function(x, val, col = NULL){
      dens <- x %>% 
        pull(!!val) %>% 
        density()
      
      t <- tibble(
        x = dens$x,
        y = dens$y
      )

      if(!is.null(col))
        t[[rlang::quo_name(col)]] <- x %>% pull(!!col) %>% unique()
      
      return(t)
    }, val = aes$x, col = aes$color) %>% 
    map_dfr(bind_rows)

  aes$x <- "x"
  aes$y <- "y"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Voronoi
#' 
#' Create a voronoi plot.
#' 
#' @inheritParams geoms
#'
#' @examples
#' df <- dplyr::tibble(
#'   x = runif(25, 1, 500),
#'   y = runif(25, 1, 500),
#'   value = runif(25, 1, 500)
#' )
#' 
#' g2(df, asp(x, y, color = value)) %>% 
#'   fig_voronoi()
#' 
#' @export
fig_voronoi <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "y", "color")]

  if(rlang::is_empty(aes))
    stop("no `x`, `y` or `color` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data
  x <- tryCatch(pull(data, !!aes$x), error = function(e) e)
  y <- tryCatch(pull(data, !!aes$y), error = function(e) e)
  if(!inherits(x, "error"))
    maxX <- x %>% max()
  else
    maxX <- aes$x
  if(!inherits(y, "error"))
    maxY <- y %>% max()
  else
    maxY <- aes$y

  data <- alter(data, type = "diagram.voronoi", fields = list(rlang::quo_name(aes$x), rlang::quo_name(aes$y)), size = list(maxX, maxY), as = list("x_", "y_"), .rename = FALSE)

  aes$x <- "x_"
  aes$y <- "y_"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "polygon", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Linear Regression
#' 
#' Add a regression line.
#' 
#' @inheritParams geoms
#' @param method Smoothing method (function) to use, accepts either a character vector, e.g. 
#'   \code{"lm"}, \code{"glm"}, \code{"gam"}, \code{"loess"} or a function, e.g. \code{MASS::rlm}
#'    or \code{mgcv::gam}, \code{base::lm}, or \code{base::loess}.
#' 
#' @examples
#' g2(cars, asp(speed, dist)) %>% 
#'   fig_point() %>% 
#'   fig_smooth()
#' 
#' @export
fig_smooth <- function(g2, ..., method = "lm", data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(aes))
    stop("no `x`, `y` aspects", call. = FALSE)

  if(is.null(data)) data <- g2$x$data
  
  formula <- aes %>% 
    map(rlang::quo_name) %>% 
    unlist() %>%
    .[names(.) %in% c("x", "y")] %>% 
    .[order(names(.), decreasing = TRUE)] %>% 
    unname() %>% 
    paste0(collapse = "~")

  model <- tryCatch(
    do.call(method, list(formula, data = data)),
    error = function(e) e
  )

  if(inherits(model, "error"))
    stop("can't fit model", call. = FALSE)
  
  y <- fitted(model)
  x <- pull(data, !!unname(aes$x))

  data <- dplyr::tibble(
    x = x,
    y = y
  )

  aes$x <- "x"
  aes$y <- "y"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "line", inherit_aes = TRUE, name = name, mapping = aes)

}