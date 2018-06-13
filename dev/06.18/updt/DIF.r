DIF <- function(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = NULL, type = c("simple","seasonal"), ...){
  require(forecast)
  if(is.null(differences)) {
    ndiff <- ifelse(type=="simple", 
                    forecast::ndiffs(x,...), 
                    forecast::nsdiffs(x,...))
    if(ndiff > 0){
      d <- list(diffseries=diff(x,lag=lag,differences=ndiff),
                lag = lag,
                ndiffs=ndiff,
                type=type)
    }
    else{
      d <- list(diffseries=x,
                lag = lag,
                ndiffs=ndiff,
                type=type)
    }
  }
  else{
    d <- list(diffseries=diff(x,lag=lag,differences=differences),
              lag = lag,
              ndiffs=differences,
              type=type)
  }
  return(d)
}