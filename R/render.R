render_g2r <- function(g2){

  main_mapping <- g2$x$mapping
  scales <- g2$x$scales

  if(length(g2$x$layers)){
    keep_main_mapping <- combine_aes(main_mapping, g2$x$layers)

    # loop over layers
    for(i in 1:length(g2$x$layers)){
      layer <- g2$x$layers[[i]] # extract layer

      aes <- mutate_aes(main_mapping, layer$mapping, layer$inherit_aes)
      layer$methods <- list()

      # data
      if(!is.null(layer$data))
        layer$data <- layer$data %>% 
          process_data(aes) %>% 
          pmap(list)

      # methods
      for(method in method_and_aes$method){
        layer$methods <- .add_geom_method(
          method, 
          aes, 
          layer$methods, 
          scales
        )
      }

      # remove useless vars
      layer$mapping <- NULL
      layer$inherit_aes <- NULL

      g2$x$layers[[i]] <- layer # override
    }

    # build facet
    if(!is.null(g2$x$facet)){
      each_view <- g2$x$layers[[1]]

      view_func <- paste0("view.", each_view$chart_type, "()")
      each_view_func <- paste_facet(each_view$methods)
      each_view_func <- paste0("function eachView(view){", view_func, each_view_func, ";}")
      each_view_func <- htmlwidgets::JS(each_view_func)

      g2$x$facet$opts$eachView <- each_view_func
      g2$x$facet$facet <- NULL
    }
  }

  g2$x$data <- g2$x$data %>% 
    process_data(keep_main_mapping) %>% 
    pmap(list)

  g2$x$scales <- NULL
  g2$x$mapping <- NULL

  if(debug_mode()){
    g2_json <- g2
    g2_json$preRenderHook <- NULL
    print(jsonlite::toJSON(g2_json, force = TRUE, auto_unbox = TRUE, pretty = TRUE))
  }

  g2
}

paste_facet <- function(methods){
  methods %>% 
    map(function(x){
      paste0(
        x$name, "(", 
        paste0(
          convert_to_json(x$args), 
          collapse = ","
        ), 
        ")"
      )
    }) %>% 
  paste0(collapse = ".") %>% 
  paste0(".", .)
}

convert_to_json <- function(x){
  map(x, function(x){
    if(length(x) <= 1)
      x <- unlist(x)

    jsonlite::toJSON(x, auto_unbox = TRUE)
  })
}

# build basic geom method
build_geom_method <- function(aes, vars){
  is_present <- names(aes) %in% vars
  aes <- aes[is_present]

  if(!length(aes)) return(NULL)

  map(aes, function(m){
    if(rlang::is_quosure(m))
      rlang::quo_name(m)
    else
      m
  }) %>% 
    unlist() %>% 
    .[order(.)] %>% # for position method: x comes before y
    unname() %>% 
    as.list() %>% 
    list()
}

# add geom
.add_geom_method <- function(name, aes, layer_methods, scales){

  # build arguments based on gaes
  vars <- method_and_aes %>% filter(method == name) %>% pull(aes) %>% unlist()
  arguments <- build_geom_method(aes, vars)

  # add relevant arguments (from scales) to method
  is_relevant_scale <- name %in% names(scales)
  if(is_relevant_scale){
    is_relevant_to_aes <- sum(name %in% names(aes))
    if(is_relevant_to_aes > 0){
      scale <- scales[is_relevant_to_aes]
      if(!inherits(scale[[1]], "JS_EVAL"))
        scale <- scale %>% unlist() %>% unname()
      else
        scale <- unname(scale)
      arguments <- append(arguments, list(scale))
    }
  }

  method <- list(name = name, args = arguments)

  if(!is.null(arguments))
    layer_methods <- append(layer_methods, list(method))

  return(layer_methods)
}