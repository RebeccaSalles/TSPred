#Transforms time series, performs ARIMA prediction and reverse transforms
fittestArimaTrans <- 
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, max.d=2, max.D=1, stationary=FALSE, 
           trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    require("forecast")
    
    #prepare the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    #prepare the test time series (if present) and set the prediction horizon
    n.ahead <- ts.test <- NULL
    if(!is.null(timeseries.test)) {
      ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
      n.ahead <- length(ts.test)
      if(!is.null(h)){
        if(h < n.ahead){
          ts.test <- head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h
    
    #transforms the time series if a transformation function is given
    transform <- function(timeseries,trans,transPar){
      if(!is.null(trans)) timeseries <- do.call(trans, c(list(timeseries),transPar))
      return(timeseries)
    }
    
    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries,max.d,max.D,stationary){
      #Best fit ARIMA
      fitARIMA <- forecast::auto.arima(timeseries, max.d=max.d, max.D=max.D, stationary=stationary)
      return(fitARIMA)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(fitARIMA){
      #computes quality measures acoording to rank.by
      AIC <- fitARIMA$aic
      BIC <- fitARIMA$bic
      AICc <- fitARIMA$aicc
      ll <- fitARIMA$loglik
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, reverse transforms the predictions and computes prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(fitARIMA,n.ahead,trans,revTrans,revTransPar,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      prediction <- forecast::forecast(fitARIMA, h=n.ahead)
      prediction <- list(pred=prediction$mean,lower=prediction$lower,upper=prediction$upper)
      
      if(!is.null(trans) & !is.null(revTrans)){
        for(i in 1:length(prediction)) prediction[[i]] <- do.call(revTrans, c(list(prediction[[i]]),revTransPar))
      }
      
      pred.mean <- ts(prediction$pred,start=i.n.ahead)
      
      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)
        
        return(list(pred=prediction,errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }
      
      return(list(pred=prediction))
    }
    
    #transforms the time series if a transformation function is given
    t_ts <- transform(ts,trans,transPar)
    
    #generates and optimizes Model based on optim parameter values
    fitARIMA <- optim.model(t_ts,max.d,max.D,stationary)
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(fitARIMA)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(fitARIMA,n.ahead,trans,revTrans,revTransPar,(nobs+1),ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=fitARIMA), fit.measures, list(pred=prediction), errors.measures )
    
    return(results)
  }