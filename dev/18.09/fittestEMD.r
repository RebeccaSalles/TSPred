#Finds and returns the fittest EMD #DO!
#References:
#[1] A Hilbert-Huang transform approach for predicting cyber-attacks, Kim et al.
fittestEMD <- 
  function(timeseries, timeseries.test=NULL, h=NULL, max.imf=10, boundary=c("none","wave","symmetric","periodic","evenodd"), level=0.95, na.action=na.omit,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    if(!is.null(boundary))
      if(!all(boundary %in% c("none","wave","symmetric","periodic","evenodd"))) stop("the boundary argument must be NULL or a subset of c('none','wave','symmetric','periodic','evenodd')")
    
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

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #set the boundary options to be tested if boundary is null
    if(is.null(boundary)) boundary <- c("none","wave","symmetric","periodic","evenodd")
    
    #decompose time series
    optim.decomp <- function(timeseries,max.imf,boundary){
      #performs empirical mode decomposition of the time series
      emdt <- EMD::emd(timeseries, max.imf=max.imf, boundary=boundary)
      return(emdt)
    }
    
    #prediction of residue series
    residue.pred <- function(emdt,n.ahead,level,rank.by){
      fpoly <- fittestPolyR(emdt$residue, h=n.ahead, level=level, rank.by=rank.by)
      return(list(pred=attr(fpoly$pred,"pred.lm"), model=fpoly$model))
    }
    
    #optimize VAR Model given a set of imfs
    optim.model <- function(emdt, imfs){
      varModel <- vars::VAR(emdt$imf[,imfs])
      return(varModel)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(varModel){
      #computes quality measures acoording to rank.by
      AIC <- stats::AIC(varModel)
      BIC <- stats::BIC(varModel)
      ll <- stats::logLik(varModel)
      df <- attr(ll,"df")
      AICc <- AIC + 2*df*(df+1)/(nobs-df-1)
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(varModel,n.ahead,level,predResidue,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate VARmodel
      predImfs <- predict(varModel,n.ahead=n.ahead, ci=level)
      
      #inverse EMD transform: generates the prediction for the original time series
      pred <- EMD::emd.pred(predImfs,predResidue, figure=FALSE)
      rownames(pred$fcst) <- rownames(pred$lower) <- rownames(pred$upper) <-NULL
      pred <- list(mean=ts(pred$fcst[,"fit"],start=i.n.ahead),
                   lower=ts(pred$lower[,"fit"],start=i.n.ahead),
                   upper=ts(pred$upper[,"fit"],start=i.n.ahead))
      #pred <- list(mean=pred$fcst[,"fit"],lower=pred$lower[,"fit"],upper=pred$upper[,"fit"])
      
      pred.mean <- pred$mean
      
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
    
    #finds the best set of parameters: meaningful IMFs and boundary condition
    rank <- NULL
    for(bndr in boundary){
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #decompose time series
      emdt <- optim.decomp(ts.tmp,max.imf,bndr)
      nimf <- emdt$nimf
      
      #prediction of residue series
      if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)) predResidue <- residue.pred(emdt,n.ahead,level,rank.by)$pred
      
      #initial options of meaningfulImfs: i:nimf for i=1,...,(nimf-1)
      firstMeaningfulImf.opt <- c(1:(nimf-1))
      
      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(firstMeaningfulImf in firstMeaningfulImf.opt){
        #generates and optimizes candidate Model based on initial parameter values
        varModel <- optim.model(emdt, c(firstMeaningfulImf:nimf))
        
        #creates candidate model id and saves it in the list models
        str.meaningfulImfs <- paste(firstMeaningfulImf,nimf,sep="-")
        ModelId <- paste(paste("Boundary:",bndr),paste("Imfs:",str.meaningfulImfs),sep="_")
        models[[ModelId]] <- list(var=varModel)
        
        if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
          #computes fitness measures and returns a dataframe with them
          rank.measures <- fitness.criteria(varModel)
        }
        else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
          #computes predictions and prediction error measures
          rank.measures <- pred.criteria(varModel,n.ahead,level,predResidue,length(ts.tmp)+1,ts.val,ts.tmp)$errors
        }
        
        #combine results of the candidate models in the dataframe rank
        rank <- rbind(rank, data.frame(ModelId=ModelId,boundary=bndr,firstMeaningfulImf=firstMeaningfulImf,rank.measures))
      }
    }
    
    #ranking the candidate models based on all measures referenced by rank.by
    #also candidate models (objects) are ranked and included as attribute of rank dataframe 
    rank <- ranking.models(rank,rank.by,models)
    
    #if rank.by is fitness
    boundary.optim <- as.character(rank[1,]$boundary)
    firstMeaningfulImf.optim <- rank[1,]$firstMeaningfulImf
    
    
    
    #decompose time series
    emdt <- optim.decomp(ts,max.imf,boundary.optim)
    nimf <- emdt$nimf
    
    #prediction of residue series
    predResidue <- residue.pred(emdt,n.ahead,level,rank.by)
    
    #generates and optimizes Model based on optim parameter values
    meaningfulImfs <- c(firstMeaningfulImf.optim:nimf)
    varModel <- optim.model(emdt, meaningfulImfs)
    str.meaningfulImfs <- paste(firstMeaningfulImf.optim,nimf,sep="-")
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(varModel)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(varModel,n.ahead,level,predResidue$pred,nobs+1,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(emd=emdt), meaningfulImfs=str.meaningfulImfs, boundary=boundary.optim, list(varImfs=varModel), 
                  list(polyRresidue=predResidue$model), fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }