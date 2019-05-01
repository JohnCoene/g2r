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
  if(is.null(data))
    return(NULL)

  row.names(data) <- NULL
  return(data)
}

process_data <- function(data, aes){
  aes <- keep(aes, rlang::is_quosure)
  select(data, !!!unname(aes))
}

# JavaScript NULL
js_null <- function(x){
  if(is.null(x) || x == "NULL")
    x <- htmlwidgets::JS("null")
  return(x)
}

# print useful messages
debug_mode <- function(){
  x <- getOption("g2r_debug")
  if(is.null(x))
    return(FALSE)
  x
}

is_g2 <- function(g2){
  if(!inherits(g2, "g2r"))
    stop("g2 must be an object of class g2 as returned by `g2`", call. = FALSE)
}

check_g2 <- function(g2){
  if(missing(g2))
    stop("missing g2", call. = FALSE)
  
  is_g2(g2)
}

.generate_id <- function(){
  LETTERS %>%
    append(letters) %>%  
    sample(45) %>% 
    paste0(collapse = "")
}

# remove animation and aes to easily append remaining options
rm_anim_aes_opts <- function(...){
  list(...) %>% 
    discard(is_animation) %>% 
    discard(is_aes) %>% 
    discard(is_opts) %>% 
    discard(is_adjust)
}

is_opts <- function(x){
  aes <- FALSE
  if(inherits(x, "option"))
    aes <- TRUE
  return(aes)
}

# construct options
.construct_options <- function(option, name = "name"){
  structure(option, class = c(name, "option", class(option)))
}

get_opts <- function(...){
  opts <- list(...) %>% 
    keep(is_opts)

  names <- opts %>% 
    map_chr("NAME") %>% 
    unlist()
  
  opts <- opts %>% 
    map("opts")
  
  names(opts) <- names
  return(opts)
}