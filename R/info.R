#' Info
#' 
#' Add informational elements to the chart.
#' 
#' @inheritParams geoms
#' @param ... Info options and \code{\link{asp}}.
#' @param figures Vector of names or indices of figure(s) to apply the information to, if \code{NULL} applies only to the first figure.
#' @param data A \code{data.frame} containing data for \code{\link{asp}} if \code{NULL} is inherited from \code{\link{g2}}.
#' 
#' @examples
#' g2(mtcars, asp(mpg, qsec)) %>% 
#'   fig_point() %>% 
#'   info_data_marker(
#'     content = "Marker",
#'     position = c(20, 20)
#'   )
#' 
#' newcars <- cars[1:26,]
#' newcars$text <- LETTERS
#' 
#' g2(cars, asp(speed, dist)) %>% 
#'   fig_point() %>% 
#'   info_text(asp(speed, dist, content = text), data = newcars, offsetY = -20)
#'  
#' @name info
#' @export
info_line <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "line")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @name info
#' @export
info_vline <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(length(aes)){
    aes$xend <- "min"
    aes$yend <- "max"
    aes$y <- rlang::quo_name(aes$x)
  }

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "line")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @name info
#' @export
info_hline <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(length(aes)){
    y <- aes$y
    aes$x <- "min"
    aes$y <- "max"
    aes$xend <- y
    aes$yend <- rlang::quo_name(y)
  }

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "line")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_text <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)
  
  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "text")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_image <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "image")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_region <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "region")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_html <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "html")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_arc <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "arc")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_data_marker <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "dataMarker")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_region_filter <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "regionFilter")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

#' @rdname info
#' @export
info_data_region <- function(g2, ..., data = NULL, figures = NULL, inherit_asp = TRUE) {
  aes <- combine_aes_for_geom(g2$x$mapping, inherit_asp, ...)
  opts <- rm_anim_aes_opts(...)

  if(is.null(data)) data <- g2$x$data

  if(!length(data) && length(aes)) stop("missing data", call. = FALSE)

  guide <- make_guide(data = data, aes = aes, opts = opts, figures = figures, type = "dataRegion")

  g2$x$guides <- append(g2$x$guides, guide)
  return(g2)
}

make_guide <- function(data, aes, opts, figures, type){
  if(length(aes)){
    data <- process_info_data(data, aes)

    guide <- data %>% 
      map(function(x, opts, figures, type){
        x$type <- type
        list(
          figures = figures,
          guide = append(x, opts)
        )
      }, opts, figures, type)
  } else {
    opts$type <- type
    guide <- list(
      figures = figures,
      guide = opts
    )
    guide <- list(guide)
  }
  return(guide)
}