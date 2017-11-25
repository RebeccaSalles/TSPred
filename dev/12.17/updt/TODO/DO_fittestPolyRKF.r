fittestPolyRKF <- 
function(timeseries, timeseries.valid, maxorder=5, level=0.9, na.action=na.omit, se.fit=FALSE, filtered = TRUE){
  if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  i.n.ahead <- nobs+1
  ts.valid <- ts(na.action(timeseries.valid),start=i.n.ahead)
  n.ahead <- length(ts.valid)
  
  models <- list()
  rank <- NULL
  for(ord in 0:maxorder){
    for(initQ in c(log(var(ts)),0)){
      model <- TSPred::SSMpolynomial(ts,ord)
      model <- fitSSM(model, inits=rep(initQ,(ord+1)))$model
      
      ModelId <- paste(paste("Ord:",ord),paste("initQ:",round(initQ,digits=1)),sep="_")
      models[[ModelId]] <- model
      
      npar <- (2*(ord+1))
      
      ll <- logLik(model,marginal = TRUE)
      AIC <- -2*ll+2*npar
      BIC <- -2*ll+log(nobs)*npar
      AICc <- AIC + 2*npar*(npar+1)/(nobs-npar-1)
      
      pred <- predict(model,n.ahead=n.ahead,interval="prediction",level=level, filtered = filtered)
      pred <- ts(pred[,1],start=i.n.ahead)
      
      MSE <- TSPred::MSE(ts.valid, pred)
      NMSE <- TSPred::NMSE(ts.valid, pred, ts)
      MAPE <- TSPred::MAPE(ts.valid, pred)
      sMAPE <- TSPred::sMAPE(ts.valid, pred)
      MaxError <- TSPred::MAXError(ts.valid, pred)
      
      rank <- rbind(rank,
                    data.frame(ModelId=ModelId,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
                    MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
    }
  }
  rownames(rank) <- NULL

  criteria<-with(rank,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
  TSPredC <- 0
  for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
  
  rank <- cbind(rank,TSPredC=TSPredC)
  rank <- rank[with(rank,order(TSPredC)),]
  
  models <- models[rank$ModelId]
  attr(rank,"ranked.models") <- models

  fitPR <- models[[1]]
  
  statsData <- rank[1,]

  #Stats
  AIC <- statsData$AIC
  BIC <- statsData$BIC
  AICc <- statsData$AICc
  ll <- statsData$logLik
  
  #Prediction errors
  prediction <- predict(fitPR,n.ahead=n.ahead,interval="prediction",level=level, filtered=filtered, se.fit=se.fit)
  
  MSE <- statsData$MSE
  NMSE <- statsData$NMSE
  MAPE <- statsData$MAPE
  sMAPE <- statsData$sMAPE
  MaxError <- statsData$MaxError
  
  return(list(model=fitPR,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
}