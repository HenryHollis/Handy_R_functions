average_duplicates = function(mixed_df,skip_rows = NULL){
  mixed_df = as.data.frame(mixed_df)
  if(!is.null(skip_rows)){
    untouched_rows = unname(mixed_df[skip_rows,])
    mixed_df = mixed_df[-skip_rows, ]
  }
  mixed_df[, -1] = as.data.frame(sapply(mixed_df[, -1], as.numeric))
  mixed_df = aggregate(mixed_df[,-1], list(Gene=mixed_df[,1]), FUN = mean)
  if(!is.null(skip_rows)){
    mixed_df = rbind(untouched_rows, mixed_df)
  }
  return(mixed_df)
}

