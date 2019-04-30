render_g2r <- function(g2){

  main_mapping <- g2$x$mapping
  scales <- g2$x$scales

  combined_mapping <- combine_aes(main_mapping, g2$x$layers)

  # globals
  if(is.null(g2$x$font)) g2$x$font <- getOption("g2_font")
  if(is.null(g2$x$renderer)) g2$x$renderer <- getOption("g2_renderer")
  if(is.null(g2$x$theme)) g2$x$theme <- getOption("g2_theme")

  views <- list()
  for(i in 1:length(g2$x$layers)){
    layer <- g2$x$layers[[i]]

    aes <- mutate_aes(main_mapping, layer$mapping, layer$inherit_aes)

    # geoms
    geom <- list(type = layer$type)
    for(method in method_and_aes$method){
      meth <- add_geom_method(method, aes, scales)
      meth <- meth[lapply(meth, length) > 0]
      geom <- append(geom, meth)
    }

    guides <- get_guides(g2$x$guides, layer$name, index = i)

    # if data passed, turn to row list
    if(!is.null(layer$data))
      layer$data <- layer$data %>% 
        process_data(aes) %>% 
        pmap(list)

    view <- list(
      layer = list(
        options = list(
          geoms = list(geom)
        )
      )
    )

    if(length(layer$data))
      view$data <- layer$data

    if(length(layer$opts))
      view$layer$options <- append(view$layer$options, layer$opts)

    if(length(guides))
      view$layer$options$guides <- guides

    views <- append(views, list(view))
  }

  g2$x$layers <- views # replace layers

  if(!is.null(g2$x$facet)){

    final_func <- ""
    for(v in 1:length(views)){
      type_func <- paste0("view.", views[[v]]$layer$options$geoms[[1]]$type, "()")
      method_func <- paste_facet(views[[v]]$layer$options$geoms[[1]])
      view_func <- paste0(type_func, method_func, ";\n")
      final_func <- paste0(final_func, view_func)
    }

    each_view_func <- paste0("function eachView(view){", final_func, "}")
    each_view_func <- htmlwidgets::JS(each_view_func)

    g2$x$layers <- NULL
    g2$x$facet$opts$eachView <- each_view_func
    g2$x$facet$facet <- NULL
  }

  # data as list
  g2$x$data <- g2$x$data %>% 
    process_data(combined_mapping) %>% 
    pmap(list)

  if(debug_mode())
    print(jsonlite::toJSON(g2$x, auto_unbox = TRUE, pretty = TRUE, force = TRUE))

  # remove unwanted
  g2$x$scales <- NULL
  g2$x$mapping <- NULL
  g2$x$guides <- NULL

  g2
}

paste_facet <- function(methods){
  methods <- methods %>% 
    map2(names(methods), function(x, y){
      paste0(
        y, "(", 
        paste0(
          convert_to_json(x), 
          collapse = ","
        ), 
        ")"
      )
    })
  
  # this is the chart type and is done differently
  methods[[1]] <- NULL 

  methods %>% 
    paste0(collapse = ".") %>% 
    paste0(".", .)
}

convert_to_json <- function(x){
  if(length(x) > 1)
    jsonlite::toJSON(unlist(x), auto_unbox = TRUE)
  
  jsonlite::toJSON(x, auto_unbox = TRUE)
}

# get guides
get_guides <- function(guides, name, index = 1){
  guides_included <- list()
  for(i in 1:length(guides)){
    if(is.null(guides[[i]]$figures) && index == 1) {
      guides_included <- append(guides_included, list(guides[[i]]$guide))
    } else if(sum(index %in% guides[[i]]$figures) >= 1 || sum(name %in% guides[[i]]$figures) >= 1)
      guides_included <- append(guides_included, list(guides[[i]]$guide))
  }
  guides_included[lapply(guides_included, length) > 0]
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
    .[order(names(.))] %>% # for position method: x comes before y
    unname() %>% 
    list()
}

# add geom
add_geom_method <- function(name, aes, scales){

  # build arguments based on plan
  vars <- method_and_aes %>% filter(method == name) %>% pull(aes) %>% unlist()
  method <- build_geom_method(aes, vars)

  # add relevant arguments (from scales) to method
  is_relevant_scale <- name %in% names(scales)
  if(is_relevant_scale){
    is_relevant_to_aes <- sum(name %in% names(aes))
    if(is_relevant_to_aes > 0){
      scl <- scales[is_relevant_to_aes] %>% unname() %>% .[[1]]
      names(method) <- "field"
      method <- append(method, scl) %>% 
        list()
      print(method)
    }
  }

  if(!is.null(method))
    names(method) <- name

  method
}