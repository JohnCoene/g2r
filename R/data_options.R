# add data options
upsert_data_opts <- function(data_opts, sync){
  # insert
  to_add <- sync[!names(sync) %in% names(data_opts)]
  data_opts <- append(data_opts, to_add)

  # update
  to_change <- sync[names(sync) %in% names(data_opts)]
  for(i in 1:length(to_change)){
    n <- names(to_change)[i]
    data_opts[[n]] <- to_change
  }

  return(data_opts)
}