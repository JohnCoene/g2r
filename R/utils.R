globalVariables(
  c(
    ".", 
    "error",
    "x", "y",
    "longitude", 
    "latitude"
  )
)

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

  if(inherits(data, "data.frame")) row.names(data) <- NULL
  return(data)
}

process_data <- function(data, aes){
  aes <- keep(aes, rlang::is_quosure)
  if(inherits(data, "data.frame"))
    data <- select(data, !!!unname(aes)) %>% pmap(list)
  return(data)
}

modify_guide_data <- function(data){
  data %>% 
    pmap(list) %>% 
    map(function(x){
      pos <- x[names(x) %in% c("x", "y")] %>% unname %>% unlist
      if(length(pos) == 2) x$position <- list(pos[[1]], pos[[2]])
      st <- x[names(x) %in% c("x", "xend")] %>% unname %>% unlist
      if(length(st) == 2) x$start <- list(st[[1]], st[[2]])
      nd <- x[names(x) %in% c("y", "yend")] %>% unname %>% unlist
      if(length(nd) == 2) x$end <- list(nd[[1]], nd[[2]])
      x$x <- NULL
      x$y <- NULL
      x$xend <- NULL
      x$yend <- NULL
      return(x)
    })
}

bind_aes <- function(data, aes){
  aes_keep <- keep(aes, rlang::is_quosure)
  if(inherits(data, "data.frame"))
    data <- select(data, !!!aes_keep) 

  aes <- discard(aes, rlang::is_quosure)
  if(length(aes)){
    x <- as.character(aes)
    names(x) <- names(aes)
    data <- cbind.data.frame(data, dplyr::as_tibble(t(x)))
  }
  
  modify_guide_data(data)
}

process_info_data <- function(data, aes){
  bind_aes(data, aes)
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
    discard(is_adjust) %>% 
    discard(is_style)
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

.get_map <- function(mapping, var){
  mapping[names(mapping) %in% var] %>% 
    map_chr(rlang::quo_name) %>% 
    unname
}

tidy_histogram <- function(h){
  hist <- tibble()
  for(i in 1:length(h$counts)){
    t <- tibble(y = h$counts[i])
    t$x <- list(c(h$breaks[i], h$breaks[i + 1]))
    hist <- bind_rows(hist, t)
  }
  return(hist)
}