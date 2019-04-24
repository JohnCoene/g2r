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