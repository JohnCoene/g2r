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
  has_aes <- aes[names(aes) %in% c("x", "group")]

  if(rlang::is_empty(has_aes))
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
#' g2(iris, asp(Petal.Length, group = Species, color = Species)) %>%
#'   fig_density()
#'  
#' @export
fig_density <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "group")]

  if(rlang::is_empty(has_aes))
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

  g2 <- make_geom(g2, ..., data = pmap(data, list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)

  g2
}

#' Boxplot
#' 
#' Create a density plot based on, \code{y} \code{x} and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' df <- mtcars %>% 
#'   dplyr::mutate(
#'     cyl = as.factor(cyl),
#'     am = as.factor(am)
#'   )
#' 
#' g2(df, asp(cyl , mpg, color = am)) %>% 
#'   fig_boxplot()
#'  
#' @export
fig_boxplot <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("y")]

  if(rlang::is_empty(has_aes))
    stop("no `y` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  grps <- aes[names(aes) %in% c("x", "color", "group")]

  if(length(aes$x))
    data <- group_split(data, !!!grps)
  else
    data <- list(data)

  data <- data %>% 
    map_dfr(function(df, aes){
      qs <- df %>% 
        pull(!!aes$y) %>% 
        boxplot(plot = FALSE) %>%
        .["stats"] %>%  
        unlist() %>% 
        unname() 

      box <- tibble(y = list(qs))

      if(length(aes$x))
        box$x <- df %>% pull(!!aes$x) %>% unique()

      if(length(aes$color))
        box[[rlang::quo_name(aes$color)]] <- df %>% pull(!!aes$color) %>% unique()
      
      return(box)
    }, aes)

  aes$x <- "x"
  aes$y <- "y"
  aes$adjust <- "dodge"
  aes$shape <- "box"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "schema", inherit_aes = TRUE, name = name, mapping = aes)

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
#'   fig_voronoi(axes = FALSE)
#' 
#' @export
fig_voronoi <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
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
#' @name smooth
#' @export
fig_smooth <- function(g2, ..., method = "lm", data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y", "group")]

  if(rlang::is_empty(has_aes))
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
    # pull x before forcing numeric conversion
    x <- pull(data, !!unname(aes$x))

    data <- data %>%
      mutate(!!aes$x := as.numeric(!!aes$x)) %>% 
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

        data <- dplyr::tibble(
          x = x,
          y = y
        )

        # add group
        data[[rlang::quo_name(aes$group)]] <- grp
        return(data)

      }, formula, aes)
  } else {
    x <- pull(data, !!unname(aes$x))

    data <- data %>%
      mutate(!!aes$x := as.numeric(!!aes$x))
      
    model <- tryCatch(
      do.call(method, list(formula, data = data)),
      error = function(e) e
    )

    if(inherits(model, "error"))
      stop("can't fit model", call. = FALSE)
    
    y <- fitted(model)

    data <- dplyr::tibble(
      x = x,
      y = y
    )
  }

  names(data) <- c(rlang::quo_name(aes$x), rlang::quo_name(aes$y))

  if(length(aes$group)){
    grouped <- group_split(data, !!aes$group)
    for(i in 1:length(grouped)){
      g2 <- make_geom(g2, ..., data = pmap(grouped[[i]], list), chart_type = "line", inherit_aes = TRUE, name = name, mapping = aes)
    }
  } else
    g2 <- make_geom(g2, ..., data = pmap(data, list), chart_type = "line", inherit_aes = TRUE, name = name, mapping = aes)

  g2
}

# fig_se <- function(g2, ..., method = "lm", level = .95, data = NULL, inherit_asp = TRUE, name = NULL){
  
#   aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
#   has_aes <- aes[names(aes) %in% c("x", "y", "group")]

#   if(rlang::is_empty(has_aes))
#     stop("no `x`, `y` aspects", call. = FALSE)

#   if(is.null(data)) data <- g2$x$data

#   formula <- aes %>% 
#     map(rlang::quo_name) %>% 
#     unlist() %>%
#     .[names(.) %in% c("x", "y")] %>% 
#     .[order(names(.), decreasing = TRUE)] %>% 
#     unname() %>% 
#     paste0(collapse = "~")

#   x <- pull(data, !!aes$x)

#   model <- tryCatch(
#     do.call(method, list(formula, data = data)),
#     error = function(e) e
#   )

#   if(inherits(model, "error"))
#     stop("can't fit model", call. = FALSE)

#   key <- "data"
#   if(length(aes$y)) key <- rlang::quo_name(aes$y)

#   new <- data %>% select(!!aes$x)
#   conf_interval <- predict(model, newdata = new, interval = "confidence", level = level)
#   conf_interval <- broom::tidy(conf_interval)
#   conf_interval$fit <- NULL
#   conf_interval$lwr <- x
#   conf_interval <- bind_cols(new, conf_interval) %>%
#     distinct() %>% 
#     tidyr::nest(lwr, upr, .key = !!key)

#   conf_interval[[key]] <- conf_interval[[key]] %>% 
#     map(unlist) %>% 
#     map(unname)

#   aes$y <- key

#   make_geom(g2, ..., data = pmap(conf_interval, list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)
# }

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
#'   fig_ribbon(axes = FALSE)
#' 
#' @export
fig_ribbon <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "ymin", "ymax", "group")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, `ymin`, or `ymax` aspects", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(rlang::is_quosure(aes$y))
    data <- select(data, -!!aes$y)

  if(length(aes$group))
    data <- group_split(data, !!aes$group)
  else 
    data <- list(data)

  data <- data %>% 
    map(function(x, aes){
      x <- x %>% 
        tidyr::nest(!!aes$ymin, !!aes$ymax)
      
      x$data <- x$data %>% 
        map(unlist) %>% 
        map(unname)
      return(x)
    }, aes)

  ribbon_name <- "data"
  if(length(aes$y)) ribbon_name <- rlang::quo_name(aes$y)

  aes$y <- ribbon_name

  for(i in 1:length(data)){
    names(data[[i]])[ncol(data[[i]])] <- ribbon_name
    g2 <- make_geom(g2, ..., data = pmap(data[[i]], list), chart_type = "area", inherit_aes = TRUE, name = name, mapping = aes)
  }
 
  return(g2)
}

#' Error bar
#' 
#' Add error bars based \code{y}, \code{ymin}, \code{ymax}, and optionally \code{group} aspects.
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
#'   fig_errorbar()
#'
#' @export
fig_errorbar <- function(g2, ..., line_size = 1, tip_size = 5, data = NULL, inherit_asp = TRUE, name = NULL){

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

#' Bins
#' 
#' Add bins based on \code{x}, and \code{y}.
#' 
#' @inheritParams geoms
#' @param type Type of bins to create.
#' @param bins A vector or list defining the bin size.
#' @param size_count Whether to size the bins by count.
#' 
#' @details Note that the function adds an aspect \code{count} which can be used, (see example). 
#' 
#' @examples
#' # issues warning in interactive session 
#' \dontrun{
#' g2(gaus, asp(x, y)) %>% 
#'   fig_bin(type = "hexagon")
#' }
#' 
#' @export
fig_bin <- function(g2, ..., type = c("rectangle", "hexagon"), bins = c(10, 5), size_count = TRUE, data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, or `y` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data
  type <- match.arg(type)
  type <- paste0("bin.", type)

  data <- alter(data, type = type, fields = list(rlang::quo_name(aes$x), rlang::quo_name(aes$y)), bins = bins, sizeByCount = size_count, as = c(rlang::quo_name(aes$x), rlang::quo_name(aes$y), "count"), .rename = FALSE)

  if(interactive())
    warning("Adding `count` aspect")

  aes$count <- "count"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "polygon", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Dot plot
#' 
#' Create a dot plot based on \code{x}, \code{y}, and optionally \code{color} aspects.
#' 
#' @inheritParams geoms
#' @param count Number of dots to use.
#' @param dot_size Size of dots.
#' @param gap_ratio Gap between dots.
#'
#' @examples
#' g2(states, asp(name, value, color = State)) %>% 
#'   fig_dotplot(asp(shape = "square")) 
#' 
#' @export
fig_dotplot <- function(g2, ..., count = 500, dot_size = list(1, 1), gap_ratio = 0.1, data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, `y`, or `color` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  if(length(aes$x)) grp <- rlang::quo_name(aes$x)

  args <- list(
    data = data, 
    type = "waffle", 
    maxCount = count,
    size = dot_size,
    gapRatio = gap_ratio,
    as = list("dotx", "doty"),
    fields = list(rlang::quo_name(aes$color), rlang::quo_name(aes$y))
  )

  if(exists(grp)) args$groupBy <- list(grp)

  data <- do.call("alter", args)

  aes$x <- "dotx"
  aes$y <- "doty"
  if(!"shape" %in% names(aes))
    aes$shape <- "circle"

  if(!"size" %in% names(aes))
    aes$size <- "hStep"

  g2 <- make_geom(g2, ..., data = pmap(data, list), chart_type = "point", inherit_aes = TRUE, name = name, mapping = aes)

  cb <- cb("function(hStep) {
    return Math.min((window.innerHeight - 100) * 0.3 * hStep, 5);
  }")

  if(aes$size == "hStep")
    g2 <- gauge_size(g2, callback = cb)

  g2
}

#' Waffle
#' 
#' Create a waffle plot based on \code{x}, \code{y}, and optionally \code{color} aspects.
#' 
#' @inheritParams geoms
#' @param count Number of squares to use.
#' @param rows Number of rows.
#' @param waffle_size Size of squares.
#' @param gap_ratio Gap between squares.
#'
#' @examples
#' fruits %>% 
#'   dplyr::mutate(value = value * 100) %>% 
#'   g2(asp(fruit, value)) %>% 
 #'  fig_waffle(asp(color = fruit))
#' 
#' @export
fig_waffle <- function(g2, ..., rows = 10, count = 500, waffle_size = list(1, 1), gap_ratio = 0.1, data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, `y`, or `color` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  data <- alter(
    data = data, 
    type = "waffle", 
    maxCount = count,
    rows = rows,
    size = waffle_size,
    gapRatio = gap_ratio,
    as = list("wafflex", "waffley"),
    fields = list(rlang::quo_name(aes$x), rlang::quo_name(aes$y))
  )

  aes$x <- "wafflex"
  aes$y <- "waffley"
  if(!"shape" %in% names(aes))
    aes$shape <- "square"

  if(!"size" %in% names(aes))
    aes$size <- "hStep"

  g2 <- make_geom(g2, ..., axes = FALSE, data = pmap(data, list), chart_type = "point", inherit_aes = TRUE, name = name, mapping = aes)

  cb <- cb("function(hStep) {
    return Math.min((window.innerHeight - 100) * 0.4 * hStep, 15);
  }")

  if(aes$size == "hStep")
    g2 <- gauge_size(g2, callback = cb) %>% conf_legend("hStep", FALSE)

  g2
}

#' Guitar
#' 
#' Create a density plot based on \code{x}, \code{y} and optionally \code{group} aspects.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' df <- mtcars %>% 
#'   dplyr::mutate(
#'     cyl = as.factor(cyl),
#'     am = as.factor(am)
#'   )
#' 
#' g2(df, asp(cyl , mpg, color = am)) %>% 
#'   fig_guitar(tooltip = FALSE)
#'  
#' @export
fig_guitar <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
    stop("no `x` `y` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  grps <- aes[names(aes) %in% c("x", "color", "group")]

  if(length(aes$x))
    data <- group_split(data, !!!grps)
  else
    data <- list(data)

  data <- data %>% 
    map_dfr(function(df, aes){
      ds <- df %>% 
        pull(!!aes$y) %>% 
        density()

      box <- tibble(
        y = list(ds$x),
        size = list(ds$y)
      )

      box$x <- df %>% pull(!!aes$x) %>% unique()

      if(length(aes$color))
        box[[rlang::quo_name(aes$color)]] <- df %>% pull(!!aes$color) %>% unique()
      
      return(box)
    }, aes)

  aes$x <- "x"
  aes$y <- "y"
  aes$size <- "size"
  aes$adjust <- "dodge"

  make_geom(g2, ..., data = pmap(data, list), chart_type = "violin", inherit_aes = TRUE, name = name, mapping = aes)

}

#' Rug
#' 
#' Add rugs
#' 
#' @inheritParams geoms
#' @param x_axis,y_axis Whether to show the ruf on the x and y axis.
#' @param opacity Opacity of individual ticks, passed to \code{\link{asp}}.
#' 
#' @examples
#' cars2 <- cars %>% 
#'   dplyr::bind_rows(cars) %>% 
#'   dplyr::mutate(
#'     speed = jitter(speed),
#'     dist = jitter(dist)
#'   )
#' 
#' g2(cars2, asp(speed, dist)) %>% 
#'   fig_point() %>% 
#'   fig_rug()
#' 
#' @export
fig_rug <- function(g2, ..., x_axis = TRUE, y_axis = TRUE, opacity = .5, data = NULL, inherit_asp = TRUE, name = NULL){

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  has_aes <- aes[names(aes) %in% c("x", "y")]

  if(rlang::is_empty(has_aes))
    stop("no `x`, or `y` aspect", call. = FALSE)

  if(is.null(data)) data <- g2$x$data

  grps <- aes[names(aes) %in% c("x", "y", "group")]

  if(length(aes$group))
    data <- group_split(data, !!!grps)
  else
    data <- list(data)

  if(x_axis){
    df <- data %>% 
      map_dfr(function(df, aes){
        df[[rlang::quo_name(aes$y)]] <- 0
        return(df)
      }, aes)

    if(!length(aes$opacity)) aes$opacity <- opacity
    aes$shape <- "line"

    g2 <- make_geom(g2, ..., data = pmap(df, list), chart_type = "point", inherit_aes = TRUE, name = name, mapping = aes)
  }

  if(y_axis){
    df <- data %>% 
      map_dfr(function(df, aes){
        df[[rlang::quo_name(aes$x)]] <- 0
        return(df)
      }, aes)

    if(!length(aes$opacity)) aes$opacity <- opacity
    aes$shape <- "hyphen"

    g2 <- make_geom(g2, ..., data = pmap(df, list), chart_type = "point", inherit_aes = TRUE, name = name, mapping = aes)
  }
  return(g2)
}
