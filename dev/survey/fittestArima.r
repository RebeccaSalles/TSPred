fittestArima <- 
function(timeseries, timeseries.valid, level=c(80,95), na.action=na.omit){
  
  if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
  n.ahead <- length(ts.valid)

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
  
  MSE <- TSPred::MSE(ts.valid, pred)
  NMSE <- TSPred::NMSE(ts.valid, pred, ts)
  MAPE <- TSPred::MAPE(ts.valid, pred)
  sMAPE <- TSPred::sMAPE(ts.valid, pred)
  MaxError <- TSPred::MAXError(ts.valid, pred)
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
}