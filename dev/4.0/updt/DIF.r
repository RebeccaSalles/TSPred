DIF <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = NULL, type = c("simple","seasonal"), ...){
  #require(forecast)
  type <- match.arg(type)
  
  if(is.null(differences)) {
    ndiff <- ifelse(type=="simple", 
                    forecast::ndiffs(x,...), 
                    forecast::nsdiffs(x,...))
    if(ndiff > 0){
      d <- diff(x,lag=lag,differences=ndiff)
    }
    else{
      d <- x
    }
    
    attr(d, "ndiffs") <- ndiff
  }
  else{
    d <- diff(x,lag=lag,differences=differences)
    
    attr(d, "ndiffs") <- differences
  }
  
  attr(d, "lag") <- lag
  attr(d, "type") <- type
  
  return(d)
}

DIF.rev <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = 1, xi, type=c("simple","seasonal")){
  type <- match.arg(type)
  d <- diffinv(x,lag=lag,differences=differences,xi=xi)
  return(d)
}