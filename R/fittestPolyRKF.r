fittestPolyRKF <- 
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, level=0.9, order=NULL, minorder=0, maxorder=5, initQ=NULL, filtered = TRUE,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    #require(KFAS)
    
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
    
    # set the order of the models to be evaluated (if present)
    if(!is.null(order)) minorder = maxorder = order

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    
    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries, order, initQ){
      model <- TSPred::SSMpolynomial(timeseries,order)
      model <- KFAS::fitSSM(model, inits=rep(initQ,(order+1)))$model
      return(model)
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
    pred.criteria <- function(model,n.ahead,level,filtered,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- predict(model,n.ahead=n.ahead,interval="prediction",level=level, filtered = filtered)
      pred <- list(mean=pred[,1],lower=pred[,2],upper=pred[,3])
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
    if(is.null(initQ) | is.null(order)){
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #initial options of Q
      initQ.opt <- c(log(var(ts.tmp)),0)
      
      #initial options of order
      order.opt <- c(minorder:maxorder)
      
      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(ord in order.opt){
        for(initQ in initQ.opt){
          #generates and optimizes candidate Model based on initial parameter values
          model <- optim.model(ts.tmp, ord, initQ)
          
          #creates candidate model id and saves it in the list models
          ModelId <- paste(paste("Order:",ord),paste("initQ:",round(initQ,digits=1)),sep="_")
          models[[ModelId]] <- model
          
          if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
            #computes fitness measures and returns a dataframe with them
            rank.measures <- fitness.criteria(model,(2*(ord+1)),length(ts.tmp))
          }
          else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
            #computes predictions and prediction error measures
            rank.measures <- pred.criteria(model,n.ahead,level,filtered,length(ts.tmp)+1,ts.val,ts.tmp)$errors
          }
          
          #combine results of the candidate models in the dataframe rank
          rank <- rbind(rank, data.frame(ModelId=ModelId,order=ord,initQ=initQ,rank.measures))
        }
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,models)
      
      initQ.optim <- rank[1,]$initQ
      order.optim <- rank[1,]$order
    }
    else{
      initQ.optim <- initQ
      order.optim <- order
    }
    
    #generates and optimizes Model based on optim parameter values
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
      model <- models[[1]]
    }
    else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
      model <- optim.model(ts, order.optim, initQ.optim)
    }
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model,(2*(order.optim+1)),nobs)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,n.ahead,level,filtered,i.n.ahead,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), order=order.optim, initQ=initQ.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }