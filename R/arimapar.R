arimapar <-
function(timeseries, na.action=na.omit, xreg=NULL){
  if(is.null(timeseries)) stop("timeseries is required and must have positive length")
  .Deprecated("arimaparameters","TSPred")
  
  ts <- ts(na.action(timeseries))
  
  nobs <- length(ts)
  reg <- cbind(1:nobs,xreg)
  
  fit <- auto.arima(ts,xreg=ts(reg,start=1))
  
  ARIMAModelInfo <- TSPred::arimaparameters(fit)
  
  return (ARIMAModelInfo)
}
