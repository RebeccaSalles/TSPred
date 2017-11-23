fittestFunction <- 
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, level=0.9, se.fit=FALSE, ..., pars=NULL,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    #prepare the training time series
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

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    
    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries, ...){
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model,npar,nobs){
      #computes quality measures acoording to rank.by
      ll <- logLik(model, marginal = TRUE)
      AIC <- -2*ll+2*npar
      BIC <- -2*ll+log(nobs)*npar
      AICc <- AIC + 2*npar*(npar+1)/(nobs-npar-1)
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,n.ahead,level,se.fit,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- predict(model,n.ahead=n.ahead,interval="prediction",level=level, se.fit=se.fit)
      pred.mean <- ts(pred[,1],start=i.n.ahead)
      
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
    if(is.null(pars)){
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #initial options of pars
      pars.opt <- c(...)
      
      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(par in pars.opt){
        #generates and optimizes candidate Model based on initial parameter values
        model <- optim.model(ts.tmp, ...)
        
        #creates candidate model id and saves it in the list models
        ModelId <- paste(...)
        models[[ModelId]] <- model
        
        if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
          #computes fitness measures and returns a dataframe with them
          rank.measures <- fitness.criteria(model,npar,length(ts.tmp))
        }
        else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
          #computes predictions and prediction error measures
          rank.measures <- pred.criteria(model,n.ahead,level,filtered,TRUE,length(ts.tmp)+1,ts.val,ts.tmp)$errors
        }
        
        #combine results of the candidate models in the dataframe rank
        rank <- rbind(rank, data.frame(ModelId=ModelId,par=par,rank.measures))
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,models)
      
      #if rank.by is fitness
      par.optim <- rank[1,]$par
    }
    else{
      par.optim <- pars
    }
    
    #generates and optimizes Model based on optim parameter values
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
      model <- models[[1]]
    }
    else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
      model <- optim.model(ts, par.optim, ...)
    }
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model,npar,nobs)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,n.ahead,level,se.fit,i.n.ahead,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), pars=par.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank))
    
    return(results)
  }