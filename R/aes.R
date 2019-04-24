#' Aesthetics
#' 
#' Mapping aesthetics.
#' 
#' @param x,y,... List of name value pairs giving aesthetics to map to
#'  variables. The names for x and y aesthetics are typically omitted because
#'  they are so common; all other aesthetics must be named.
#' 
#' @section Aesthetics:
#' \itemize{
#'   \item{\code{x}, \code{y}}
#'   \item{\code{size}}
#'   \item{\code{color}}
#'   \item{\code{shape}}
#'   \item{\code{opacity}}
#'   \item{\code{adjust}}
#'   \item{\code{tooltip}}
#'   \item{\code{label}}
#'   \item{\code{style}}
#' }
#' 
#' @export
gaes <- function(x, y, ...) {
  exprs <- rlang::enquos(x = x, y = y, ..., .ignore_empty = "all")
  aes <- new_aes(exprs, env = parent.frame())
  structure(aes, class = c("gaes", class(aes)))
}

# Wrap symbolic objects in quosures but pull out constants out of
# quosures for backward-compatibility
new_aesthetic <- function(x, env = globalenv()) {
  if (rlang::is_quosure(x)) {
    if (!rlang::quo_is_symbolic(x)) {
      x <- rlang::quo_get_expr(x)
    }
    return(x)
  }

  if (rlang::is_symbolic(x)) {
    x <- rlang::new_quosure(x, env = env)
    return(x)
  }

  x
}

new_aes <- function(x, env = globalenv()) {
  stopifnot(is.list(x))
  x <- lapply(x, new_aesthetic, env = env)
  structure(x, class = c("uneval"))
}

#' @export
print.uneval <- function(x, ...) {
  cat("Aesthetics: \n")

  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, rlang::quo_label, character(1))
    bullets <- paste0("* ", format(paste0("`", names(x), "`")), " -> ", values, "\n")

    cat(bullets, sep = "")
  }

  invisible(x)
}

#' @export
"[.uneval" <- function(x, i, ...) {
  new_aes(NextMethod())
}

# If necessary coerce replacements to quosures for compatibility
#' @export
"[[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}
#' @export
"$<-.uneval" <- function(x, i, value) {
  # Can't use NextMethod() because of a bug in R 3.1
  x <- unclass(x)
  x[[i]] <- value
  new_aes(x)
}
#' @export
"[<-.uneval" <- function(x, i, value) {
  new_aes(NextMethod())
}

# returns TRUE if aes is found
has_aes <- function(...){
  x <- list(...) %>% 
    map(is_aes)

  TRUE %in% x  
}

# aes to keep
is_aes <- function(x){
  aes <- FALSE
  if(inherits(x, "gaes"))
    aes <- TRUE
  return(aes)
}

# retrieve aesthetics
get_aes <- function(...){
  aes <- list(...) %>% 
    keep(is_aes) 

  if(length(aes))
    aes[[1]]
  else
    list()
}

# mutate aesthetics
mutate_aes <- function(main_aes = NULL, aes = NULL, inherit = TRUE){

  if(is.null(aes) && isTRUE(inherit))
    return(main_aes)

  if(isTRUE(inherit)){
    combined <- append(aes, main_aes)
    # will remove main_aes duplicates => aes overrides
    return(combined[!duplicated(combined)]) 
  }

  return(aes)
}

# build basic geom method
build_geom_method <- function(aes, vars){
  is_present <- names(aes) %in% vars
  aes <- aes[is_present]

  if(!length(aes)) return(NULL)

  map(aes, rlang::quo_name) %>% 
    unlist() %>% 
    .[order(., decreasing = TRUE)] %>% 
    unname() %>% 
    as.list()
}

# build position()
build_position <- function(aes){
  build_geom_method(aes, c("x", "y"))
}

# build size()
build_size <- function(aes){
  build_geom_method(aes, c("size"))
}