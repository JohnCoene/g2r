#' Plane
#' 
#' This function is similar to the \code{facet_*} family of functions from the \code{ggplot2} package.
#' 
#' @inheritParams geoms
#' @param type The type of facet to user.
#' @param ... Planes, bare column names or \code{NULL}.
#' 
#' @name plane
#' @export
plane_wrap <- function(g2, ..., type = c("list", "rect", "circle", "tree", "mirror", "matrix")) {
  
  plane_aes <- get_planes(...)

  if(!length(plane_aes))
    stop("no planes specified, see `planes`", call. = FALSE)

  sync <- sync_planes(plane_aes)
  g2$x$dataOpts <- upsert_data_opts(g2$x$dataOpts, sync)

  # add to ensure we select columns
  g2$x$mapping <- append(g2$x$mapping, plane_aes) 

  # add to main mapping
  plane_names <- plane_aes %>% 
    map(rlang::quo_name) %>% 
    unlist() %>% 
    unname()

  # extract fields
  fields <- plane_names %>% 
    map(js_null) 
  
  # fields needs to be an array
  if(length(fields) == 1)
    fields <- list(fields)

  # options
  g2$x$facet <- list(
    type = match.arg(type),
    opts = list(
      fields = fields
    )
  )

  # remove planes 
  dots <- rlang::dots_list(...) %>% 
    discard(inherits, "gaes")
  dots[[1]] <- NULL
  g2$x$facet$opts <- append(g2$x$facet$opts, dots)

  return(g2)
}

#' @rdname plane
#' @export
planes <- function(...){
  exprs <- rlang::enquos(..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  .construct_aesthetics(aes, "planes")
}

sync_planes <- function(plane_aes){
  vars <- plane_aes %>% 
    map(rlang::quo_name) %>% 
    unlist() %>% 
    unname() %>% 
    .[. != "NULL"]

  vars %>% 
    map(function(x){
      list(sync = TRUE)
    }) %>% 
    set_names(vars)
}