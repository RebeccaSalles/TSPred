diff <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = NULL, type = c("simple","seasonal"), ...){
  #require(forecast)
  type <- match.arg(type)
  #browser()
  if(missing(lag) || is.null(lag)) lag <- ifelse(type=="simple", 1, frequency(x))
  
  if(is.null(differences)) {
    ndiff <- ifelse(type=="simple", 
                    forecast::ndiffs(x,...), 
                    forecast::nsdiffs(x,...))
    if(ndiff > 0){
      d <- base::diff(x,lag=lag,differences=ndiff)
    }
    else{
      d <- x
    }
    
    differences <- ndiff
  }
  else{
    d <- base::diff(x,lag=lag,differences=differences)
  }
  
  attr(d, "differences") <- differences
  attr(d, "lag") <- lag
  attr(d, "type") <- type
  attr(d,"xi") <- head(x,lag*differences)
  attr(d,"xf") <- tail(x,lag*differences)
  
  return(d)
}

diff.rev <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = 1, xi, type=c("simple","seasonal"),addinit=TRUE){
  type <- match.arg(type)
  #browser()
  d <- stats::diffinv(x,lag=lag,differences=differences,xi=as.matrix(xi))
  
  if(!addinit) d <- tail(d,length(d)-length(xi))
  
  return(d)
}