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

process_data <- function(data, aes){
  aes <- keep(aes, rlang::is_quosure)
  select(data, !!!unname(aes))
}