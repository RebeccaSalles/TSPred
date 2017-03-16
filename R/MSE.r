MSE <- 
function(actual, prediction) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- mean((actual-prediction)^2)

  res
}