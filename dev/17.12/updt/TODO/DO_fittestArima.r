fittestArima <- 
function(timeseries, timeseries.test, level=c(80,95), na.action=na.omit){
  
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
  n.ahead <- length(ts.test)

  #Best fit ARIMA
  fitARIMA <- forecast::auto.arima(ts)
  
  #Stats
  AIC <- fitARIMA$aic
  BIC <- fitARIMA$bic
  AICc <- fitARIMA$aicc
  ll <- fitARIMA$loglik
  
  #Prediction errors
  prediction <- forecast::forecast(fitARIMA, h=n.ahead, level=level)
  prediction <- list(pred=prediction$mean,lower=prediction$lower,upper=prediction$upper)

  pred <- ts(prediction$pred,start=(nobs+1))
  
  MSE <- TSPred::MSE(ts.test, pred)
  NMSE <- TSPred::NMSE(ts.test, pred, ts)
  MAPE <- TSPred::MAPE(ts.test, pred)
  sMAPE <- TSPred::sMAPE(ts.test, pred)
  MaxError <- TSPred::MAXError(ts.test, pred)
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
}