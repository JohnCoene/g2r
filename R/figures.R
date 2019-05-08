#' Histogram
#' 
#' Create a histogram based on \code{x} and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' @param bin_width Width of bins.
#' 
#' @examples
#' g2(iris, asp(Sepal.Length, group = Species, color = Species))  %>% 
#'   fig_histogram(bin_width = .3)
#' 
#' @export
fig_histogram <- function(g2, ..., bin_width = 1, data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "group")]

  if(rlang::is_empty(aes))
    stop("no `x` or `group` aspect", call. = FALSE)

  if(is.null(data))
    data <- g2$x$data

  if(length(aes$group))
    data <- alter(
      data,
      type = "bin.histogram",
      field = rlang::quo_name(aes$x),
      groupBy = list(rlang::quo_name(aes$group)),
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
#' Create a density plot based on \code{x} and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' g2(iris, asp(Petal.Length, color = Species)) %>% 
#'   fig_density()
#'  
#' @export
fig_density <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "group")]

  if(rlang::is_empty(aes))
    stop("no `x` or `group` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(length(aes$group))
    data <- group_split(data, !!aes$group)
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
    }, val = aes$x, col = aes$group) %>% 
    map_dfr(bind_rows)

  aes$x <- "x"
  aes$y <- "y"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Voronoi
#' 
#' Create a voronoi plot based on \code{x} and \code{y} aspects.
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
  aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(aes))
    stop("no `x`, or `y` aspect", call. = FALSE)

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

#' Smooth
#' 
#' Add smoothing methods based on \code{x} and \code{y} and, optionally \code{group} aspects.
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
  aes <- aes[names(aes) %in% c("x", "y", "group")]

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

  if(length(aes$group)){
    data <- data %>%
      group_split(!!aes$group) %>% 
      map_df(function(data, formula, aes){

        grp <- pull(data, !!aes$group)

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

        # add group
        data[[rlang::quo_name(aes$group)]] <- grp
        return(data)

      }, formula, aes)
  } else {
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
  }

  aes$x <- "x"
  aes$y <- "y"

  if(length(aes$group)){
    grouped <- group_split(data, !!aes$group)
    for(i in 1:length(grouped)){
      g2 <- make_geom(g2, ..., data = pmap(grouped[[i]], list), chart_type = "line", inherit_aes = TRUE, name = name, mapping = aes)
    }
  } else
    g2 <- make_geom(g2, ..., data = pmap(data, list), chart_type = "line", inherit_aes = TRUE, name = name, mapping = aes)

  g2
}

#' Ribbon
#' 
#' Add a ribbon based on \code{x}, \code{ymin}, \code{ymax}, and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' df <- dplyr::tibble(
#'   x = 1:100,
#'   y = runif(100, 7, 18),
#'   y1 = runif(100, 5, 10),
#'   y2 = runif(100, 12, 20)
#' ) %>% 
#'   dplyr::mutate(
#'     y = runif(dplyr::n(), y1 + 1, y2 - 1)
#'   )
#' 
#' g2(df, asp(x, y, ymin = y1, ymax = y2)) %>% 
#'   fig_line() %>% 
#'   fig_ribbon()
#' 
#' @export
fig_ribbon <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) %in% c("x", "ymin", "ymax", "group")]

  if(rlang::is_empty(aes))
    stop("no `x`, `ymin`, or `ymax` aspects", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(length(aes$group))
    data <- group_split(data, !!aes$group)
  else 
    data <- list(data)

  data <- data %>% 
    map(function(x, aes){
      x <- x %>% 
        tidyr::nest(!!aes$ymin, !!aes$ymax, .key = "ribbon")
      
      x$ribbon <- x$ribbon %>% 
        map(unlist) %>% 
        map(unname)
      return(x)
    }, aes)

  aes$y <- "ribbon"

  for(i in 1:length(data)){
    g2 <- make_geom(g2, ..., data = pmap(data[[i]], list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)
  }
 
  return(g2)
}

#' Error bar
#' 
#' Add error bars based \code{ymin}, \code{ymax}, and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' @param line_size,tip_size Defines size of error bars.
#' 
#' @examples
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#' 
#' g2(df, asp(trt, ymin = lower, ymax = upper, color = group)) %>% 
#'   fig_error_bar()
#'
#' @export
fig_error_bar <- function(g2, ..., line_size = 1, tip_size = 5, data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "ymin", "ymax", "group")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, `ymin`, or `ymax` aspects", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(length(aes$group))
    data <- group_split(data, !!aes$group)
  else 
    data <- list(data)

  data <- data %>% 
    map(function(x, aes){
      x <- x %>% 
        tidyr::nest(!!aes$ymin, !!aes$ymax, .key = "error")
      
      x$error <- x$error %>% 
        map(unlist) %>% 
        map(unname)
      return(x)
    }, aes)

  aes$y <- "error"
  aes$size <- line_size
  aes_point <- aes
  aes_point$shape <- "line"
  aes_point$size <- tip_size

  for(i in 1:length(data)){
    g2 <- make_geom(g2, ..., style(rotate = 90), data = pmap(data[[i]], list), chart_type = "point", inherit_aes = TRUE, name = name, mapping = aes_point)
    g2 <- make_geom(g2, ..., data = pmap(data[[i]], list), chart_type = "interval", inherit_aes = TRUE, name = name, mapping = aes)
  }
 
  sync(g2, error)
}