fittestArimaKF <- 
function(timeseries, timeseries.valid, na.action=na.omit, level=0.9, se.fit=FALSE, filtered = TRUE){
  if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  i.n.ahead <- nobs+1
  ts.valid <- ts(na.action(timeseries.valid),start=i.n.ahead)
  n.ahead <- length(ts.valid)

  #Best fit ARIMA
  fitARIMA <- auto.arima(ts)
  
  ar.coef <- fitARIMA$coef[grep("ar",attr(fitARIMA$coef, "names")) ]
  ma.coef <- fitARIMA$coef[grep("ma",attr(fitARIMA$coef, "names")) ]
  p <- length(ar.coef)
  q <- length(ma.coef)
  npar <- (p+q+1)
  
  d <- fitARIMA$arma[6]
  if(d==0) npar <- npar+1

  likfn <- function(pars, model, estimate=TRUE){
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
  
  models <- list()
  rank <- NULL
  for(initQ in c(log(var(ts)),0)){
    model <- SSModel(ts ~ SSMarima(ar=ar.coef,ma=ma.coef,d=d), H=0)
    
    inits <- c(ar.coef,ma.coef,initQ)
    fit <- optim(inits, likfn, model=model, method='BFGS')
    model_arima <- likfn(fit$par,model,FALSE)
    
    ModelId <- paste("ARIMAKF",paste("initQ:",round(initQ,digits=1)),sep="_")
    models[[ModelId]] <- model_arima
    
    ll <- logLik(model_arima, marginal = TRUE)
    AIC <- -2*ll+2*npar
    BIC <- -2*ll+log(nobs)*npar
    AICc <- AIC + 2*npar*(npar+1)/(nobs-npar-1)
    
    pred <- predict(model_arima,n.ahead=n.ahead,interval="prediction",level=level, filtered = filtered)
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
  rownames(rank) <- NULL
  
  criteria<-with(rank,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
  TSPredC <- 0
  for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
  
  rank <- cbind(rank,TSPredC=TSPredC)
  rank <- rank[with(rank,order(TSPredC)),]
  
  models <- models[rank$ModelId]
  attr(rank,"ranked.models") <- models
  
  fitARIMA <- models[[1]]
  
  statsData <- rank[1,]
  
  #Stats
  AIC <- statsData$AIC
  BIC <- statsData$BIC
  AICc <- statsData$AICc
  ll <- statsData$logLik
  
  #Prediction errors
  prediction <- predict(fitARIMA,n.ahead=n.ahead,interval="prediction",level=level, filtered=filtered, se.fit=se.fit)
  
  MSE <- statsData$MSE
  NMSE <- statsData$NMSE
  MAPE <- statsData$MAPE
  sMAPE <- statsData$sMAPE
  MaxError <- statsData$MaxError
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
}