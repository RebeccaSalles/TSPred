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