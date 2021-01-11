#Finds and returns the fittest wavelet  #DO!
#References:
#[1] Day-Ahead Electricity Price Forecasting Using the Wavelet Transform and ARIMA Models, Conejo et al.
#[2] Time series forecasting based on wavelet filtering, Joo and Kim
#[3] A Wavelet Based Prediction Method for Time Series, Stolojescu et al.
fittestWavelet <- 
  function(timeseries, timeseries.test=NULL, h=NULL, filters=c("haar", "d4", "la8", "bl14", "c6"), n.levels=NULL, maxlevel=NULL, 
           model=c("ets","arima"), conf.level=0.95, na.action=na.omit,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){
    
    #require(wavelets)
    #require(forecast)
    
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
    
    # if no set of filters is provided, all possibilities are tested
    if(is.null(filters)){
      filters <- c("haar", "d4", "d6", "d8", "d10", "d12", "d14", "d16", "d18", "d20",
                   "la8", "la10", "la12", "la14", "la16", "la18", "la20", 
                   "bl14", "bl18", "bl20", 
                   "c6", "c12", "c18", "c24", "c30") #Filters (mother wavelets) characteristics can be seen in [3]
    }

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #set maximal level of decomposition
    set.maxlevel <- function(filter,maxlevel){
      # get Wavelet Transform Filter
      filter <- wavelets::wt.filter(filter, modwt=TRUE)
      # get the length of the wavelet and scaling filters
      L <- filter@L
      
      # determine the maximal level of decomposition
      if(missing(maxlevel) | is.null(maxlevel)){
        maxlevel <- as.integer(floor(log(((nobs-1)/(L-1))+1)/log(2)))
      }
      
      return(maxlevel)
    }
    
    #decompose the time series by maximal overlap discrete wavelet transform
    optim.wt <- function(timeseries,filter,n.levels){
      wdecomp <- wavelets::modwt(timeseries, filter=filter, n.levels=n.levels)
      
      return(wdecomp)
    }
    
    #transform time series and optimize Models given a set of initial parameters
    optim.models <- function(timeseries,filter,maxlevel,modelType,...){
      #decompose the time series by maximal overlap discrete wavelet transform
      wdecomp <- optim.wt(timeseries,filter,maxlevel)
      
      #model 
      Wmodels <- Vmodels <- list()
      for(level in 1:maxlevel){
        if(modelType=="ets") {
          Wmodels[[level]] <- forecast::ets(wdecomp@W[[level]],...)
          Vmodels[[level]] <- forecast::ets(wdecomp@V[[level]],...)
        }
        else if(modelType=="arima"){
          Wmodels[[level]] <- forecast::auto.arima(wdecomp@W[[level]],...)
          Vmodels[[level]] <- forecast::auto.arima(wdecomp@V[[level]],...)
        }
      }
      
      return(list(Wmodels=Wmodels,Vmodels=Vmodels))
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(models,level){
      #Vi is the main component of the WT of level i acoording to [1]
      Vimodel <- models$Vmodels[[level]]
      #computes quality measures acoording to rank.by
      AIC <- Vimodel$aic
      BIC <- Vimodel$bic
      AICc <- Vimodel$aicc
      ll <- Vimodel$loglik
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions for each decomposed time series
    decomp.pred <- function(models,maxlevel,n.ahead,confLevel){
      prediction <- list()
      for(level in 1:maxlevel){
        fcW <- forecast::forecast(models$Wmodels[[level]], h=n.ahead, level=confLevel)
        fcV <- forecast::forecast(models$Vmodels[[level]], h=n.ahead, level=confLevel)
        prediction[[level]] <- list(W=list(pred=fcW$mean,lower=fcW$lower,upper=fcW$upper),
                                    V=list(pred=fcV$mean,lower=fcV$lower,upper=fcV$upper))
      }
      
      return(prediction)
    }
    
    #inverse transform the time series, computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(wdecomp,decomp.level,prediction,n.ahead,i.n.ahead,ts.test,ts){
      #edits modwt object: adds n.ahead NAs to the end of original time series values
      newseries <- c(wdecomp@series,rep(NA,n.ahead))
      wdecomp@series <- as.matrix(newseries)
      wdecomp@attr.X <- attributes(ts(newseries))
      #saves state of modwt object until this point
      wdecompPred <- wdecomp
      
      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$pred))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$pred))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets prediction time series
      pred <- ts(tail(iwdecomp,n.ahead),start=i.n.ahead)
      
      #Obtain lower and upper predictions analogously
      #retrieves previous state of modwt object
      wdecomp <- wdecompPred
      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$lower))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$lower))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets lower prediction time series
      lower <- ts(tail(iwdecomp,n.ahead),start=i.n.ahead)
      
      #retrieves previous state of modwt object
      wdecomp <- wdecompPred
      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$upper))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$upper))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets upper prediction time series
      upper <- ts(tail(iwdecomp,n.ahead),start=i.n.ahead)
      
      
      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred)
        NMSE <- TSPred::NMSE(ts.test, pred, ts)
        MAPE <- TSPred::MAPE(ts.test, pred)
        sMAPE <- TSPred::sMAPE(ts.test, pred)
        MaxError <- TSPred::MAXError(ts.test, pred)
        
        return(list(pred=list(mean=pred,lower=lower,upper=upper),errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }
      
      return(list(pred=list(mean=pred,lower=lower,upper=upper)))
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
      attr(rank,"ranked.wt") <- models
      
      return(rank)
    }
    
    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    if(length(filters)==1 & !is.null(n.levels)){
      level.optim <- n.levels
      filter.optim <- filters[1]
    }
    else{
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #level options to be tested is equal to c(n.levels) if it is not null
      if(!is.null(n.levels)){
        #set maximal level of decomposition to be tested
        maxlevel <- n.levels
        #initial options of levels
        levels.opt <- n.levels
      }
      
      for(filter in filters){
        #set level options to be tested
        if(is.null(n.levels)){
          #set maximal level of decomposition to be tested
          maxlevel <- set.maxlevel(filter,maxlevel)
          #initial options of levels
          levels.opt <- c(1:maxlevel)
        }
        
        #decompose time series up to maxlevel and optimize Models of each decomposed series given a set of initial parameters
        models <- optim.models(ts.tmp,filter,maxlevel,modelType,...)
        
        #computes predictions for each decomposed time series if rank.by includes error measures
        if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)) prediction <- decomp.pred(models,maxlevel,n.ahead,conf.level)
        
        #produces candidate models and measures in order to select "best" parameters
        wts <- list()
        for(level in levels.opt){
          #generates and optimizes candidate Model based on initial parameter values
          wt <- optim.wt(ts.tmp,filter,level)
          
          #creates candidate model id and saves it in the list models
          ModelId <- paste(paste("Filter:",filter),paste("Level:",level),sep="_")
          wts[[ModelId]] <- wt
          
          if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
            #computes fitness measures and returns a dataframe with them
            rank.measures <- fitness.criteria(models,level)
          }
          else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
            #computes predictions and prediction error measures
            rank.measures <- pred.criteria(wt,level,prediction,n.ahead,length(ts.tmp)+1,ts.val,ts.tmp)$errors
          }
          
          #combine results of the candidate models in the dataframe rank
          rank <- rbind(rank, data.frame(ModelId=ModelId,filter=filter,level=level,rank.measures))
        }
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,wts)
      
      #if rank.by is fitness
      level.optim <- rank[1,]$level
      filter.optim <- as.character(rank[1,]$filter)
    }
    
    
    
    
    #decompose time series up to maxlevel and optimize Models of each decomposed series given a set of initial parameters
    models <- optim.models(ts,filter.optim,level.optim,modelType,...)
    
    #computes predictions for each decomposed time series if rank.by includes error measures
    prediction <- decomp.pred(models,level.optim,n.ahead,conf.level)
    
    #generates and optimizes candidate Model based on initial parameter values
    wt <- optim.wt(ts,filter.optim,level.optim)
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(models,level.optim)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(wt,level.optim,prediction,n.ahead,nobs+1,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(WT=wt), level=level.optim, filter=filter.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }