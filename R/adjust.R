#' Adjust
#' 
#' Adjust figures.
#' 
#' @param type A vector of types of adjustement to apply to the figure, see the "types" section below for valid values.
#' @param margin Margin, between \code{0} and \code{1}.
#' @param dodge_by Bare column name of 
#' 
#' @section Types:
#' Valid values for the \code{type} argument.
#' \itemize{
#'   \item{\code{stack}}
#'   \item{\code{dodge}}
#'   \item{\code{jitter}}
#'   \item{\code{symmetric}}
#' }
#' 
#' @export
adjust <- function(type, margin = NULL, dodge_by = NULL) {

  if(missing(type))
    stop("missing type")
  
  validity <- sum(type %in% c("stack", "dodge", "jitter", "symmetric"))

  if(!identical(validity, length(type)))
    stop("invalid type specified")

  dodge_enquo <- rlang::enquo(dodge_by)

  options <- list(type = type)
  if(!is.null(margin))
    options$marginRatio <- margin
  if(!rlang::quo_is_null(dodge_enquo))
    options$dodgeBy <- rlang::quo_text(dodge_enquo)

  structure(options, class = c("adjust", class(options)))
}

is_adjust <- function(x){
  aes <- FALSE
  if(inherits(x, "adjust"))
    aes <- TRUE
  return(aes)
}

get_adjust <- function(...){
  list(...) %>% 
    keep(is_adjust)
}
