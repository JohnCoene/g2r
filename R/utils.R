globalVariables(c("."))

get_data <- function(main_data = NULL, data = NULL){
  if(is.null(main_data) && is.null(data))
    stop("Missing data", call. = FALSE)
  
  if(!is.null(data))
    return(data)
  else
    return(main_data)
}

keep_data <- function(data){
  row.names(data) <- NULL
  return(data)
}

is_quo_sym <- function(x){
  if(rlang::is_quosure(x) || rlang::is_symbol(x))
    return(TRUE)
  return(FALSE)
}

process_data <- function(data, aes){
  aes <- keep(aes, is_quo_sym)
  select(data, !!!unname(aes))
}