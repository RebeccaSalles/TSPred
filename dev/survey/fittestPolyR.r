fittestPolyR <- 
  function(timeseries, timeseries.valid, minorder=0, maxorder=5, level=0.95, na.action=na.omit, se.fit=FALSE){
    if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
    
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
    n.ahead <- length(ts.valid)
    
    t<-seq(1,nobs,along.with=ts)
    tnew<-seq((nobs+1),(nobs+n.ahead),along.with=ts.valid)
    
    data <- data.frame(ts)
    data.valid <- data.frame(ts.valid)
    names <- "y"
    
    for(i in 1:maxorder){
      names <- c(names,paste("t",i,sep = '^'))
      data <- cbind(data,t^i)
      data.valid <- cbind(data.valid,tnew^i)
    }
    colnames(data) <- names
    colnames(data.valid) <- names
    
    fit.max <- lm(y~. ,data=data, na.action = "na.fail")
    
    rank <- suppressMessages(dredge(fit.max,  m.lim = c(minorder,maxorder), rank = "AICc", extra = c(AIC, BIC)))
    
    calls <- attr(rank,"model.calls")
    models <- MSE <- NMSE <- MAPE <- sMAPE <- MaxError <- NULL
    for(i in 1:length(calls)){
      models <- rbind(models,model=toString(calls[i]))
      
      fit <- eval(parse(text=calls[i]))
      pred <- ts(predict(fit, data.valid, level=level, se.fit = FALSE),start=(nobs+1))
      
      MSE <- rbind(MSE,TSPred::MSE(ts.valid, pred))
      NMSE <- rbind(NMSE,TSPred::NMSE(ts.valid, pred, ts))
      MAPE <- rbind(MAPE,TSPred::MAPE(ts.valid, pred))
      sMAPE <- rbind(sMAPE,TSPred::sMAPE(ts.valid, pred))
      MaxError <- rbind(MaxError,TSPred::MAXError(ts.valid, pred))
    }
    rownames(models) <- NULL
    
    rank <- cbind(rank,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)
    
    criteria<-with(rank,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
    TSPredC <- 0
    for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
    
    rank <- cbind(rank,TSPredC=TSPredC,modelCall=models)
    rank <- rank[with(rank,order(TSPredC)),]
    
    modelCall <- toString(rank$modelCall[1])
    fitPR <- eval(parse(text=modelCall))
    
    model.calls <- list()
    for(i in 1:nrow(rank)){
      model.calls[[i]] <- parse(text=toString(rank$modelCall[i]))
    }
    
    exc <- names(rank) %in% c("modelCall", "df", "delta", "weight") 
    rank <- rank[!exc]
    attr(rank,"model.calls") <- model.calls
    
    statsData <- rank[1,]
    
    #Stats
    AIC <- statsData$AIC
    BIC <- statsData$BIC
    AICc <- statsData$AICc
    ll <- statsData$logLik
    
    #Prediction errors
    prediction <- predict(fitPR, data.valid, se.fit = se.fit, level=level)
    
    MSE <- statsData$MSE
    NMSE <- statsData$NMSE
    MAPE <- statsData$MAPE
    sMAPE <- statsData$sMAPE
    MaxError <- statsData$MaxError
    
    return(list(model=fitPR,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
                pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
  }