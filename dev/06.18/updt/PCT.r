PCT <- function(x){
  lag <- 1
  n <- length(x)
  xt <- x[(1+lag):n]
  xt_1 <- x[1:(n-lag)]
  log(xt)-log(xt_1)
}