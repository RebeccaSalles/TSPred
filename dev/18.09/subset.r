train_test_subset <- function(data, train_perc=0.8, test_len=NULL, na.action=na.omit)
{
  if(is.data.frame(data)||is.matrix(data)) data_len <- nrow(data)
  else data_len <- length(data)
  
  if (is.null(test_len))
    idx = 1:(as.integer(train_perc*data_len))
  else
    idx = 1:(data_len-test_len)
  
  train = tryCatch( data[idx,],  error=function(c) data[idx])
  test = tryCatch( data[-idx,],  error=function(c) data[-idx])
  
  train = na.action(train)
  
  return (list(train=train, test=test))
}