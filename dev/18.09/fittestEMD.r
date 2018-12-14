#Finds and returns the fittest EMD #DO!
#References:
#[1] A Hilbert-Huang transform approach for predicting cyber-attacks, Kim et al.
fittestEMD <- 
  function(timeseries, timeseries.test=NULL, h=NULL, num_imfs=0, S_number=4L, num_siftings=50L, level=0.95, na.action=na.omit,
           model=c("ets","arima"),rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","errors")){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    #require(EMD)
    #require("vars")
    
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
    if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")

    #decompose time series
    optim.decomp <- function(timeseries,num_imfs,S_number,num_siftings){
      #performs empirical mode decomposition of the time series
      emdt <- Rlibeemd::emd(timeseries, num_imfs=num_imfs, S_number=S_number, num_siftings=num_siftings)
      return(emdt)
    }

    #optimize Models given a set of initial parameters
    optim.models <- function(timeseries,emdt,modelType,...){
      #model 
      models <- list()
      for(level in 1:ncol(emdt)){
        if(modelType=="ets") {
          models[[level]] <- forecast::ets(ts(emdt[,level]),...)
        }
        else if(modelType=="arima"){
          models[[level]] <- forecast::auto.arima(emdt[,level],...)
        }
      }
      
      return(models)
    }
    
    #computes predictions for each decomposed time series
    decomp.pred <- function(models,maxlevel,n.ahead,level){
      prediction <- list()
      for(level in 1:maxlevel){
        fc <- forecast::forecast(models[[level]], h=n.ahead, level=level)
        prediction[[level]] <- list(pred=fc$mean,lower=fc$lower,upper=fc$upper)
      }
      
      return(prediction)
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(prediction,n.ahead,level,ts.test,ts){
      #computes predictions using the candidate VARmodel
      pred <- lapply(prediction,function(c) c$pred)
      lower <- lapply(prediction,function(c) c$lower)
      upper <- lapply(prediction,function(c) c$upper)
      
      #inverse EMD transform: generates the prediction for the original time series
      pred <- emd.rev(pred)
      lower <- emd.rev(lower)
      upper <- emd.rev(upper)
      
      pred.mean <- pred
      
      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)
        
        return(list(pred=list(mean=pred,lower=lower,upper=upper),errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }
      
      return(list(pred=list(mean=pred,lower=lower,upper=upper)))
    }
    
    #ranks candidate models
    ranking.models <- function(rank,rank.by){
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
      
      return(rank)
    }
    
    #finds the best set of parameters: meaningful IMFs
    rank <- NULL
    
    # creates the Validation series for parameter optimization
    ts.val <- tail(ts,n.ahead)
    ts.tmp <- head(ts,nobs-n.ahead)
    
    #decompose time series
    emdt <- optim.decomp(ts.tmp,num_imfs,S_number,num_siftings)
    nimf <- ncol(emdt)
    
    models <- optim.models(ts.tmp,emdt,modelType)
    
    #prediction of residue series
    if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)) predictions <- decomp.pred(models,nimf,n.ahead,level)
    
    #initial options of meaningfulImfs: i:nimf for i=1,...,(nimf-1)
    firstMeaningfulImf.opt <- c(1:(nimf-1))
    
    #produces candidate models and measures in order to select "best" parameters
    models <- list()
    for(firstMeaningfulImf in firstMeaningfulImf.opt){
      #creates candidate model id and saves it in the list models
      str.meaningfulImfs <- paste(firstMeaningfulImf,nimf,sep="-")
      ModelId <- paste("Imfs:",str.meaningfulImfs)
      
      preds <- predictions[firstMeaningfulImf:nimf]
      
      if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
        #computes predictions and prediction error measures
        rank.measures <- pred.criteria(preds,n.ahead,level,ts.val,ts.tmp)$errors
      }
      
      #combine results of the candidate models in the dataframe rank
      rank <- rbind(rank, data.frame(ModelId=ModelId,firstMeaningfulImf=firstMeaningfulImf,rank.measures))
    }
    
    #ranking the candidate models based on all measures referenced by rank.by
    #also candidate models (objects) are ranked and included as attribute of rank dataframe 
    rank <- ranking.models(rank,rank.by)
    
    #if rank.by is fitness
    firstMeaningfulImf.optim <- rank[1,]$firstMeaningfulImf
    
    
    
    #decompose time series
    emdt <- optim.decomp(ts,num_imfs,S_number,num_siftings)
    nimf <- ncol(emdt)
    
    models <- optim.models(ts,emdt,modelType)
    
    predictions <- decomp.pred(models,nimf,n.ahead,level)

    #generates and optimizes Model based on optim parameter values
    meaningfulImfs <- c(firstMeaningfulImf.optim:nimf)
    str.meaningfulImfs <- paste(firstMeaningfulImf.optim,nimf,sep="-")
    
    preds <- predictions[meaningfulImfs]
    
    pred.measures <- pred.criteria(preds,n.ahead,level,ts.test,ts)

    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(emd=emdt), meaningfulImfs=str.meaningfulImfs, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }