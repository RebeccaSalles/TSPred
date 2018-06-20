MAS <- function(x,order){
  n <- length(x)
  xt <- NULL
  for(t in 1:(n-order+1)){
    xt <- c(xt, sum(x[t:(t+order-1)])/order)
  }
  ts(xt)
}

MAS.rev <- function(xm,xinit,order,addinit=TRUE){
  n <- length(xm)
  x <- xinit
  if(order>1){ 
    for(t in order:(n+order-1)){
      x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
    }
  }
  else x <- xm
  if(addinit) ts(x)
  else ts( tail(x,n) )
}