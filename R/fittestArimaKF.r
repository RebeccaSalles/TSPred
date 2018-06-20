fittestArimaKF <- 
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, level=0.9, filtered = TRUE, initQ=NULL,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"), ...){
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    #require(KFAS)
    
    #preparing the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    i.n.ahead <- nobs+1
    
    #preparing the test time series (if present) and setting the prediction horizon
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
    
    # function to return the log-likelihood of an ARIMA State Space Model given a set of parameters
    # used by the optim function to optimize the choice of model parameters
    # may also return the model if estimate is FALSE
    likfn <- function(pars, model, p, q, d, estimate=TRUE){
      tmp <- try(SSMarima(ar=artransform(pars[1:p]),
                          ma=artransform(pars[(p+1):(p+q)]),d=d,Q = exp(pars[(p+q+1)])),silent=TRUE)    
      if(!inherits(tmp,"try-error")){
        model["T","arima"] <- tmp$T 
        model["R","arima"] <- tmp$R    
        model["P1","arima"] <- tmp$P1
        model["Q","arima"] <- tmp$Q
        if(estimate){
          -logLik(model)
        } else model
      } else {
        if(estimate){
          1e100
        } else model
      }
    }
    
    #function for choosing initial arima parameters based on the result of auto.arima
    arima.par <- function(timeseries,...){
      #Best fit ARIMA
      fitARIMA <- forecast::auto.arima(timeseries,...)
      
      #Sets arima parameters from fitARIMA
      ar.coef <- fitARIMA$coef[ grep("ar",attr(fitARIMA$coef, "names")) ]
      ma.coef <- fitARIMA$coef[ grep("ma",attr(fitARIMA$coef, "names")) ]
      p <- length(ar.coef)
      q <- length(ma.coef)
      npar <- (p+q+1)
      
      d <- fitARIMA$arma[6]
      if(d==0) npar <- npar+1
      
      return(list(ar.coef=ar.coef,ma.coef=ma.coef,p=p,q=q,d=d,npar=npar))
    }
    
    #optimize ARIMA State Space Model given a set of initial parameters
    optim.model <- function(timeseries, initPar, initQ, likfn){
      #generates initial ARIMA State Space Model
      model <- KFAS::SSModel(timeseries ~ SSMarima(ar=initPar$ar.coef,ma=initPar$ma.coef,d=initPar$d), H=0)
      
      #optimizes the model parameters based on initial values 
      inits <- c(initPar$ar.coef,initPar$ma.coef,initQ)
      fit <- optim(par=inits, fn=likfn, model=model, p=initPar$p, q=initPar$q, d=initPar$d, method='BFGS')
      model_arima <- likfn(pars=fit$par,model=model, p=initPar$p, q=initPar$q, d=initPar$d, estimate=FALSE)
      
      return(model_arima)
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
    
    #if initQ parameter is not provided, the function finds the best option among {log(var(ts)),0}
    rank <- NULL
    if(is.null(initQ)){
      # creates the Validation series for parameter optimization
      ts.val <- tail(ts,n.ahead)
      ts.tmp <- head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #initial options of Q
      initQ.opt <- c(log(var(ts.tmp)),0)
      
      #initial arima parameters
      initPar <- arima.par(ts.tmp,...)
      
      #produces candidate models and measures in order to select "best" initQ parameter 
      models <- list()
      for(initQ in initQ.opt){
        #generates and optimizes candidate ARIMA State Space Model based on initial parameter values
        model_arima <- optim.model(ts.tmp, initPar, initQ, likfn)
        
        #creates candidate model id and saves it in the list models
        ModelId <- paste("ARIMAKF",paste("initQ:",round(initQ,digits=1)),sep="_")
        models[[ModelId]] <- model_arima
        
        if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
          #computes fitness measures and returns a dataframe with them
          rank.measures <- fitness.criteria(model_arima,initPar$npar,length(ts.tmp))
        }
        else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
          #computes predictions and prediction error measures
          rank.measures <- pred.criteria(model_arima,n.ahead,level,filtered,length(ts.tmp)+1,ts.val,ts.tmp)$errors
        }
        
        #combine results of the candidate models in the dataframe rank
        rank <- rbind(rank, data.frame(ModelId=ModelId,initQ=initQ,rank.measures))
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,models)
     
      #if rank.by is fitness
      initQ.optim <- rank[1,]$initQ
    }
    else{
      initQ.optim <- initQ
    }
      
    #initial arima parameters
    initPar <- arima.par(ts,...)
    
    #gets previously optimized Model based on optim parameter values
    #if a ranking based on fitness measures was performed, the whole time series was used for training and the model generated can be reused
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by) & !is.null(rank)){
      model_arima <- attr(rank,"ranked.models")[[1]]
    }
    #generates and optimizes Model based on optim parameter values
    else{
      model_arima <- optim.model(ts, initPar, initQ.optim, likfn)
    }
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model_arima,initPar$npar,nobs)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model_arima,n.ahead,level,filtered,i.n.ahead,ts.test,ts)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model_arima), initQ=initQ.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }