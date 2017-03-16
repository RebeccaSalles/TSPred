NMSE <- 
function(actual, prediction, train.actual) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- sum( (actual-prediction)^2 ) / sum( (actual-mean(train.actual))^2 )
  
  res
}