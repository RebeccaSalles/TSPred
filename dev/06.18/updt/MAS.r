MAS <- function(x,order){
  n <- length(x)
  xt <- NULL
  for(t in 1:(n-order+1)){
    xt <- c(xt, sum(x[t:(t+order-1)])/order)
  }
  ts(xt)
}