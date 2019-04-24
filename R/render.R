render_g2r <- function(g2){

  main_mapping <- g2$x$mapping

  if(length(g2$x$layers)){
    for(i in 1:length(g2$x$layers)){
      layer <- g2$x$layers[[i]] # extract layer

      aes <- mutate_aes(main_mapping, layer$mapping, layer$inherit_aes)
      layer$methods <- list()

      # methods
      position <- build_position(aes)
      layer$methods <- .add_geom_method("position", position, layer$methods)

      size <- build_size(aes)
      layer$methods <- .add_geom_method("size", size, layer$methods)

      color <- build_size(aes)
      layer$methods <- .add_geom_method("color", color, layer$methods)

      shape <- build_shape(aes)
      layer$methods <- .add_geom_method("shape", shape, layer$methods)

      label <- build_label(aes)
      layer$methods <- .add_geom_method("label", label, layer$methods)

      # remove useless vars
      layer$mapping <- NULL
      layer$inherit_aes <- NULL

      g2$x$layers[[i]] <- layer # override
    }
  }

  g2$x$data <- apply(g2$x$data, 1, as.list)

  g2$x$mapping <- NULL
  g2
}

.add_geom_method <- function(name, argument, layer_methods){

  method <- list(name = name, arg = argument)

  if(!is.null(argument))
    layer_methods <- append(layer_methods, list(method))

  return(layer_methods)
}