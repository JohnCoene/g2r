render_g2r <- function(g2){

  main_mapping <- g2$x$mapping

  if(length(g2$x$layers)){
    for(i in 1:length(g2$x$layers)){
      aes <- mutate_aes(main_mapping, g2$x$layers[[i]]$mapping)
      g2$x$layers[[i]]$position <- build_position(aes) %>% as.list
      g2$x$layers[[i]]$mapping <- NULL
    }
  }

  g2$x$data <- apply(g2$x$data, 1, as.list)

  g2$x$mapping <- NULL
  g2
}