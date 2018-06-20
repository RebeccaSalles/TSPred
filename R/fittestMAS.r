#Finds and returns the fittest MAS
#maxorder from the sma function of smooth package
fittestMAS <- 
  function(timeseries, timeseries.test=NULL, h=NULL, order=NULL, minorder=1, maxorder=min(36,length(ts(na.action(timeseries)))/2), 
           model=c("ets","arima"), level=0.95, na.action=na.omit,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
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
    
    # evaluate choice of model for prediction
    modelType <- match.arg(model)
    
    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #transformation function (moving average smoothing)
    MAS <- TSPred::MAS
    #reverse transformation function (moving average smoothing)
    revMAS <- TSPred::MAS.rev
    
    #transforms series, optimize Model given a set of initial parameters and predicts n.ahead observations
    optim.model <- function(timeseries,order,modelType,n.ahead,level,...){
      #moving average smoothing transformation
      sma <- MAS(timeseries,order)
      
      #require(forecast)
      if(modelType=="ets") {
        fc <- forecast::forecast(sma, h=n.ahead, level=level,...)
      }
      else if(modelType=="arima"){
        #Best fit ARIMA
        fc <- forecast::forecast(forecast::auto.arima(sma,...), h=n.ahead, level=level)
      }
      
      return(fc)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model){
      #computes quality measures acoording to rank.by
      AIC <- model$aic
      BIC <- model$bic
      AICc <- model$aicc
      ll <- model$loglik
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions (reverse transforms predictions), and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(fc,i.n.ahead,ts.test,ts,ord){
      #computes predictions using the candidate model
      prediction <- list(mean=fc$mean,lower=fc$lower,upper=fc$upper)
      
      xinit <- tail(as.numeric(ts),ord-1)
      prediction$mean <- revMAS(prediction$mean,xinit,ord,addinit=FALSE)
      attributes(prediction$mean) <- attributes(fc$mean)
      prediction$lower <- revMAS(prediction$lower,xinit,ord,addinit=FALSE)
      attributes(prediction$lower) <- attributes(fc$lower)
      prediction$upper <- revMAS(prediction$upper,xinit,ord,addinit=FALSE)
      attributes(prediction$upper) <- attributes(fc$upper)
      
      pred.mean <- ts(prediction$mean,start=i.n.ahead)
      
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
    
    #ranks candidate models
    ranking.models <- function(rank,rank.by,models){
      rownames(rank) <- NULL
      
      #create ranking criteria based on all measures referenced by rank.by
      criteria <- rank[ , (names(rank) %in% rank.by), drop = FALSE]
      if("logLik" %in% names(criteria)) criteria["logLik"] <- -criteria["logLik"]
      TSPredC <- 0
      for(c in names(criteria)) TSPredC <- TSPredC + rank(criteria[c])
      names(TSPredC) <- NULL
      
      #ranking the candidate models based on all measures referenced by rank.by
      rank <- cbind(rank,rank.position.sum=TSPredC)
      rank <- rank[with(rank,order(rank.position.sum)),]
      
      #candidate models are ranked and included as attribute of rank dataframe 
      models <- models[rank$ModelId]
      attr(rank,"ranked.models") <- models
      
      return(rank)
    }
    
    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    if(is.null(order)){
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #initial options of pars
      pars.opt <- c(minorder:maxorder)
      
      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(par in pars.opt){
        #generates and optimizes candidate Model based on initial parameter values
        fc <- optim.model(ts.tmp,order=par,modelType=modelType,n.ahead=n.ahead,level=level,...)
        model <- fc$model
        
        #creates candidate model id and saves it in the list models
        ModelId <- paste(paste("MAOrd:",par),modelType,sep="_")
        models[[ModelId]] <- model
        
        if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
          #computes fitness measures and returns a dataframe with them
          rank.measures <- fitness.criteria(model)
        }
        else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
          #computes predictions and prediction error measures
          rank.measures <- pred.criteria(fc,length(ts.tmp)+1,ts.val,ts.tmp,par)$errors
        }
        
        #combine results of the candidate models in the dataframe rank
        rank <- rbind(rank, data.frame(ModelId=ModelId,order=par,rank.measures))
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,models)
      
      #if rank.by is fitness
      order.optim <- rank[1,]$order
    }
    else{
      order.optim <- order
    }
    
    #gets previously optimized Model based on optim parameter values
    #if a ranking based on fitness measures was performed, the whole time series was used for training and the model generated can be reused
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by) & !is.null(rank)){
      model <- attr(rank,"ranked.models")[[1]]
      fc <- forecast::forecast(model, h=n.ahead, level=level)
    }
    #generates and optimizes Model based on optim parameter values
    else{
      fc <- optim.model(ts,order=order.optim,modelType=modelType,n.ahead=n.ahead,level=level,...)
      model <- fc$model
    }
    
    #moving average smoothed time series
    ma <- MAS(ts,order.optim)
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(fc,(nobs+1),ts.test,ts,order.optim)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), order=order.optim, list(ma=ma), fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }