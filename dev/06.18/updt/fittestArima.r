fittestArima <- 
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, level=c(80,95), ...){
    
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    require("forecast")
    
    #prepare the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    i.n.ahead <- nobs+1
    #prepare the test time series (if present) and set the prediction horizon
    n.ahead <- ts.test <- NULL
    if(!is.null(timeseries.test)) {
      ts.test <- ts(na.action(timeseries.test),start=i.n.ahead)
      n.ahead <- length(ts.test)
      if(!is.null(h)){
        if(h < n.ahead){
          ts.test <- head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h

    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries,...){
      model <- forecast::auto.arima(timeseries,...)
      return(model)
    }
    
    #computes quality measures
    fitness.criteria <- function(model){
      AIC <- model$aic
      BIC <- model$bic
      AICc <- model$aicc
      ll <- model$loglik
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,n.ahead,level,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- forecast::forecast(model, h=n.ahead, level=level)
      pred <- list(mean=pred$mean,lower=pred$lower,upper=pred$upper)
      pred.mean <- ts(pred$mean,start=i.n.ahead)
      
      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)
        
        return(list(pred=pred,errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }
      
      return(list(pred=pred))
    }

    #generates and optimizes Model based on optim parameter values
    model <- optim.model(ts, ...)
    
    modelpar <- arimaparameters(model)
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,n.ahead,level,i.n.ahead,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), list(parameters=modelpar), fit.measures, list(pred=prediction), errors.measures )
    
    return(results)
  }