PCT <- function(x){
  lag <- 1
  n <- length(x)
  xt <- x[(1+lag):n]
  xt_1 <- x[1:(n-lag)]
  log(xt)-log(xt_1)
}

PCT.rev <- function(p,x0){
  xt <- exp(log(x0)+p[1])
  for(i in 2:length(p)) xt <- c(xt, exp(log(xt[length(xt)])+p[i]) )
  xt
}

#PCT.rev <- function(p,x0){
#  xt <- x0*(1+p[1])
#  for(i in 2:length(p)) xt <- c(xt, (1+p[i])*xt[length(xt)] )
#  xt
#}

