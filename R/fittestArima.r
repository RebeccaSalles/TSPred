fittestArima <- 
function(timeseries, timeseries.test, na.action=na.omit, se.fit=FALSE){
  
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
  n.ahead <- length(ts.test)

  #Best fit ARIMA
  fitARIMA <- auto.arima(ts)
  
  #Stats
  AIC <- fitARIMA$aic
  BIC <- fitARIMA$bic
  AICc <- fitARIMA$aicc
  ll <- fitARIMA$loglik
  
  #Prediction errors
  prediction <- predict(fitARIMA, n.ahead=n.ahead,se.fit=se.fit)

  if(se.fit) pred <- ts(prediction$pred,start=(nobs+1))
  else pred <- ts(prediction,start=(nobs+1))
  
  MSE <- TSPred::MSE(ts.test, pred)
  NMSE <- TSPred::NMSE(ts.test, pred, ts)
  MAPE <- TSPred::MAPE(ts.test, pred)
  sMAPE <- TSPred::sMAPE(ts.test, pred)
  MaxError <- TSPred::MAXError(ts.test, pred)
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
}