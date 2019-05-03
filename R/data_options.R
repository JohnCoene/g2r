#' Sync & Unsync
#' 
#' Synchronises variables across planes (\code{\link{plane_wrap}}), in order to have them share scales.
#' 
#' @inheritParams geoms
#' @param ... Bare Column names of variables to synchronise.
#' 
#' @details By default \code{plane_wrap} will sync variables. You can unsync them if undesired.
#' 
#' @examples
#' iris %>% 
#'   g2(asp(Sepal.Length, Sepal.Width)) %>% 
#'   fig_point(asp(color = Species)) %>% 
#'   plane_wrap(planes(Species)) %>% 
#'   unsync(Petal.Length)
#' 
#' @name sync
#' @export
sync <- function(g2, ...){
  
  exprs <- rlang::enquos(..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  aes <- .construct_aesthetics(aes, "sync")

  sync <- sync_it(aes, TRUE)
  g2$x$dataOpts <- upsert_data_opts(g2$x$dataOpts, sync)

  return(g2)
}

#' @name sync
#' @export
unsync <- function(g2, ...){
  
  exprs <- rlang::enquos(..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  aes <- .construct_aesthetics(aes, "sync")

  sync <- sync_it(aes, FALSE)
  g2$x$dataOpts <- upsert_data_opts(g2$x$dataOpts, sync)

  return(g2)
}

sync_it <- function(vars, v = TRUE){
  vars <- vars %>% 
    map(rlang::quo_name) %>% 
    unlist() %>% 
    unname() %>% 
    .[. != "NULL"]

  vars %>% 
    map(function(x, v){
      list(sync = v)
    }, v) %>% 
    set_names(vars)
}

# add data options
upsert_data_opts <- function(data_opts, sync){

  # split
  to_add <- sync[!names(sync) %in% names(data_opts)]
  to_change <- sync[names(sync) %in% names(data_opts)]

  # insert
  data_opts <- append(data_opts, to_add)

  if(!length(to_change))
    return(data_opts)

  # update
  for(i in 1:length(to_change)){
    n <- names(to_change)[i]
    data_opts[[n]] <- NULL
    data_opts <- append(data_opts, to_change)
  }

  return(data_opts)
}