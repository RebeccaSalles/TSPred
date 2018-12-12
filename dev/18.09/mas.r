mas <- function(x,order,...){
  
  if(is.null(order)){
    fmas <- TSPred::fittestMAS(x,order=order,...)
    order <- fmas$order
  }
  
  n <- length(x)
  xt <- NULL
  for(t in 1:(n-order+1)){
    xt <- c(xt, sum(x[t:(t+order-1)])/order)
  }
  
  xt <- ts(xt)
  attr(xt,"order") <- order
  attr(xt,"xi") <- head(x,order-1)
  attr(xt,"xf") <- tail(x,order-1)
  
  return(xt)
}

mas.rev <- function(xm,xi,order,addinit=TRUE){
  n <- length(xm)
  x <- xi
  if(order>1){ 
    for(t in order:(n+order-1)){
      x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
    }
  }
  else x <- xm
  if(addinit) ts(x)
  else ts( tail(x,n) )
}