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
  aes <- aes[names(aes) == "x" || names(aes) == "color"]

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
  aes <- aes[names(aes) == "x" || names(aes) == "color"]

  if(rlang::is_empty(aes))
    stop("no `x` or `color` aspect", call. = FALSE)

  if(is.null(data))
    data <- g2$x$data

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

#' Violin
#' 
#' Create a violin plot.
#' 
#' @inheritParams geoms
#' 
#' @examples
#' g2(iris, asp(Petal.Length, color = Species))  %>% 
#'   fig_violin2()
#'  
#' @export
fig_violin2 <- function(g2, ..., data = NULL, inherit_asp = TRUE, name = NULL) {

  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  aes <- aes[names(aes) == "x" || names(aes) == "color"]

  if(rlang::is_empty(aes))
    stop("no `x`, `y` or `color` aspect", call. = FALSE)

  if(is.null(data))
    data <- g2$x$data

  if(length(aes$color))
    data <- supressWarnings(data %>% group_by(!!aes$x) %>% group_split(!!aes$color))
  else
    data <- data %>% group_by(!!aes$x)

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

  make_geom(g2, ..., data = pmap(data, list), chart_type = "violin", inherit_aes = TRUE, name = name, mapping = aes)

}