fittestPolyR <- 
  function(timeseries, timeseries.test=NULL, h=NULL, order=NULL, minorder=0, maxorder=5, raw = FALSE, na.action=na.omit, level=0.95,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    #require(MuMIn)
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
    
    # set the order of the models to be evaluated (if present)
    if(!is.null(order)) minorder = maxorder = order
    
    #preparing the dataset with independent variables (t,t^2,t^3,...)
    t <- seq(1,nobs,along.with=ts)
    tnew <- seq((nobs+1),(nobs+n.ahead),length.out=n.ahead)
    #independent variables generated with poly(...,raw=FALSE) are orthogonal polynomials and avoid correlation between variables
    pt <- poly(t,maxorder,raw=raw)
    data <- data.frame(y=ts,pt)
    ptnew = predict(pt,tnew) #calls predict.poly
    data.test <- data.frame(ptnew)
    
    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #return optimized Model given its call
    optim.model <- function(modelCall,modelData){
      fit <- eval(parse(text=modelCall))
      return(fit)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model){
      ll <- logLik(model, marginal = TRUE)
      AIC <- AIC(model)
      BIC <- BIC(model)
      AICc <- MuMIn::AICc(model)
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,data.ahead,level,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- predict(model, data.ahead, interval="prediction", se.fit=FALSE, level=level)
      
      rownames(pred) <- NULL
      pred <- list(mean=ts(pred[,"fit"],start=i.n.ahead),
                   lower=ts(pred[,"lwr"],start=i.n.ahead),
                   upper=ts(pred[,"upr"],start=i.n.ahead))
      attr(pred,"pred.lm") <- predict(model, data.ahead, interval="prediction", se.fit=TRUE, level=level)
      
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
      rank <- cbind(rank,rank.position.sum=TSPredC,modelCall=models)
      rank <- rank[with(rank,order(rank.position.sum)),]
      
      #candidate models are ranked and included as attribute of rank dataframe 
      model.calls <- list()
      for(i in 1:nrow(rank)){
        model.calls[[i]] <- parse(text=toString(rank$modelCall[i]))
      }
      
      exc <- names(rank) %in% c("modelCall", "df", "delta", "weight") 
      rank <- rank[!exc]
      attr(rank,"model.calls") <- model.calls
      
      return(rank)
    }
    
    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    
    # creates the Validation series for parameter optimization
    data.val <- tail(data,n.ahead)
    data.tmp <- head(data,nobs-n.ahead)
    
    extra <- NULL
    #if rank.by considers fitness measures, parameter optimization uses only and all the training series
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
      data.tmp <- data
      extra <- rank.by
    }
    
    #produces all possible combinations (dredge) of candidate models and fitness measures in order to select "best" parameters
    #and combine results of the candidate models in the dataframe rank
    modelData <- data.tmp
    fit.max <- lm(y~. ,data=modelData, na.action = "na.fail")
    #set the limits c(lower, upper) for number of terms in a single model (excluding the intercept)
    m.lim = c(0,maxorder)
    rank <- suppressMessages(MuMIn::dredge(fit.max,  m.lim = m.lim, rank = "AICc", extra = extra))
    # subsets models with orders in the range [minorder,maxorder]
    rank <- rank[!apply(data.frame(rank[,(minorder+1):(maxorder+1)]),1,function(row) all(is.na(row)))]
    
    calls <- attr(rank,"model.calls")
    models <- error.measures <- NULL
    for(i in 1:length(calls)){
      #generates and optimizes candidate Model based on call
      model <- optim.model(calls[i],data.tmp)
      
      #saves candidate model calls
      models <- rbind(models,model=toString(calls[i]))
      
      if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
        #computes predictions and prediction error measures
        errors <- pred.criteria(model,data.val[-1],level,nrow(data.tmp)+1,data.val[["y"]],data.tmp[["y"]])$errors
        error.measures <- rbind(error.measures, errors)
      }
    }
    rownames(models) <- NULL
    
    if(!is.null(error.measures)) rank <- cbind(rank,error.measures)
    
    #ranking the candidate models based on all measures referenced by rank.by
    #also candidate models (objects) are ranked and included as attribute of rank dataframe 
    rank <- ranking.models(rank,rank.by,models)
    
    #generates and optimizes Model based on optim parameter values
    model <- optim.model(attr(rank,"model.calls")[[1]],data)
    
    #extract the order of the model
    order.optim <- as.numeric(tail(strsplit(tail(names(model$coefficients),1),"")[[1]],1)[1])
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,data.test,level,nobs+1,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), order=order.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }