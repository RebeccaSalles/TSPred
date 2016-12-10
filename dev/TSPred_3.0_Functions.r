#Required Libraries
library("TSPred")
library("KFAS")
library("MuMIn")

#calculates the MSE error between the n actual and forecasted values. Return type: Numeric.
#Parameters:
#actual-> a continuation for a time series with the actual values. Required.
#forecast-> a continuation  for a time series with the forecasted values. Required.
MSE <- function(actual, prediction) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- mean((actual-prediction)^2)

  res
}

#calculates the NMSE error between the n actual and forecasted values. Return type: Numeric.
#Parameters:
#actual-> a continuation for a time series with the actual values. Required.
#forecast-> a continuation  for a time series with the forecasted values. Required.
NMSE <- function(actual, prediction, train.actual) {
  if (length(actual) != length(prediction)) stop("actual and prediction have different lengths")
  
  n <- length(actual)
  
  res <- sum( (actual-prediction)^2 ) / sum( (actual-mean(train.actual))^2 )
  
  res
}


optimArima <- function(timeseries, timeseries.test, na.action=na.omit, se.fit=FALSE){
  
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
  n.ahead <- length(ts.test)

  #Best fit ARIMA
  fitARIMA <- auto.arima(ts)
  
  #Stats
  AIC <- fitARIMA$aic
  BIC <- fitARIMA$bic
  AICc <- fitARIMA$aicc
  ll <- fitARIMA$loglik
  
  #Prediction errors
  prediction <- predict(fitARIMA, n.ahead=n.ahead,se.fit=se.fit)

  if(se.fit) pred <- ts(prediction$pred,start=(nobs+1))
  else pred <- ts(prediction,start=(nobs+1))
  
  MSE <- MSE(ts.test, pred)
  NMSE <- NMSE(ts.test, pred, ts)
  MAPE <- TSPred::MAPE(ts.test, pred)
  sMAPE <- TSPred::sMAPE(ts.test, pred)
  MaxError <- TSPred::MAXError(ts.test, pred)
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
}

#If filtered is TRUE use filtered observations for prediction, otherwise, use smoothed observations (sometimes/always equal to the real observations)
optimArimaKF <- function(timeseries, timeseries.test, na.action=na.omit, se.fit=FALSE, filtered = TRUE){
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  i.n.ahead <- nobs+1
  ts.test <- ts(na.action(timeseries.test),start=i.n.ahead)
  n.ahead <- length(ts.test)

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
    
    pred <- predict(model_arima,n.ahead=n.ahead,interval="prediction",level=0.9, filtered = filtered)
    pred <- ts(pred[,1],start=i.n.ahead)
    
    MSE <- MSE(ts.test, pred)
    NMSE <- NMSE(ts.test, pred, ts)
    MAPE <- TSPred::MAPE(ts.test, pred)
    sMAPE <- TSPred::sMAPE(ts.test, pred)
    MaxError <- TSPred::MAXError(ts.test, pred)
    
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
  prediction <- predict(fitARIMA,n.ahead=n.ahead,interval="prediction",level=0.9, filtered=filtered, se.fit=se.fit)
  
  MSE <- statsData$MSE
  NMSE <- statsData$NMSE
  MAPE <- statsData$MAPE
  sMAPE <- statsData$sMAPE
  MaxError <- statsData$MaxError
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
}


optimPolyR <- function(timeseries, timeseries.test, maxorder=5, na.action=na.omit, se.fit=FALSE){
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.test <- ts(na.action(timeseries.test),start=(nobs+1))
  n.ahead <- length(ts.test)
  
  t<-seq(1,nobs,along.with=ts)
  tnew<-seq((nobs+1),(nobs+n.ahead),along.with=ts.test)

  data <- data.frame(ts)
  data.test <- data.frame(ts.test)
  names <- "y"
  
  for(i in 1:maxorder){
    names <- c(names,paste("t",i,sep = '^'))
    data <- cbind(data,t^i)
    data.test <- cbind(data.test,tnew^i)
  }
  colnames(data) <- names
  colnames(data.test) <- names
  
  fit.max <- lm(y~. ,data=data, na.action = "na.fail")

  rank <- suppressMessages(dredge(fit.max,  m.lim = c(0,maxorder), rank = "AIC", extra = alist(AICc, BIC)))
  
  calls <- attr(rank,"model.calls")
  models <- MSE <- NMSE <- MAPE <- sMAPE <- MaxError <- NULL
  for(i in 1:length(calls)){
    models <- rbind(models,model=toString(calls[i]))
    
    fit <- eval(parse(text=calls[i]))
    pred <- ts(predict(fit, data.test, se.fit = FALSE),start=(nobs+1))
    
    MSE <- rbind(MSE,MSE(ts.test, pred))
    NMSE <- rbind(NMSE,NMSE(ts.test, pred, ts))
    MAPE <- rbind(MAPE,TSPred::MAPE(ts.test, pred))
    sMAPE <- rbind(sMAPE,TSPred::sMAPE(ts.test, pred))
    MaxError <- rbind(MaxError,TSPred::MAXError(ts.test, pred))
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
  
  rank <- subset(rank, select=-c(modelCall,df,delta,weight))
  attr(rank,"model.calls") <- model.calls
  
  statsData <- rank[1,]
  
  #Stats
  AIC <- statsData$AIC
  BIC <- statsData$BIC
  AICc <- statsData$AICc
  ll <- statsData$logLik
  
  #Prediction errors
  prediction <- predict(fitPR, data.test, se.fit = se.fit)
  
  MSE <- statsData$MSE
  NMSE <- statsData$NMSE
  MAPE <- statsData$MAPE
  sMAPE <- statsData$sMAPE
  MaxError <- statsData$MaxError
  
  return(list(model=fitPR,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
}


SSMpolynomial <- function(y, ord, H=0, Q=NA){ #Função feita com base no código do dlmodeler
  if( ord<0 ) stop("Order must be >= 0")
  m <- ord+1
  if( length(Q)!=1 & length(Q)!=m ) stop("SigmaQ has wrong dimension: should be of size ",m)
  d <- 1
  
  a0 <- matrix(0,m,1)
  P0 <- diag(0,m,m)
  P0inf <- diag(m)
  
  Tt <- diag(1,m,m)
  if( m>1 ) for( i in 1:(m-1) ) Tt[i,i+1] <- 1
  Rt <- diag(1,m,m)
  Qt <- diag(Q,m) #diag(sigmaQ^2,m)
  
  Zt <- matrix(c(1,rep(0,m-1)),d,m)
  Ht <- H #matrix(sigmaH^2,d,d)
  
  SSM <- SSModel(y ~ -1 + SSMcustom(
      Z=Zt, # observation
      T=Tt, # transition
      R=Rt, # state disturbance selection
      Q=Qt, # state disturbance covariance
      a1=a0, # initial state
      P1=P0, # initial state covariance
      P1inf=P0inf, # diffuse part of P1
      n=1
    ),
    H=Ht # observation disturbance
  )
  
  return(SSM)
}


optimPolyRKF <- function(timeseries, timeseries.test, maxorder=5, na.action=na.omit, se.fit=FALSE, filtered = TRUE){
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  i.n.ahead <- nobs+1
  ts.test <- ts(na.action(timeseries.test),start=i.n.ahead)
  n.ahead <- length(ts.test)
  
  models <- list()
  rank <- NULL
  for(ord in 0:maxorder){
    for(initQ in c(log(var(ts)),0)){
      model <- SSMpolynomial(ts,ord)
      model <- fitSSM(model, inits=rep(initQ,(ord+1)))$model
      
      ModelId <- paste(paste("Ord:",ord),paste("initQ:",round(initQ,digits=1)),sep="_")
      models[[ModelId]] <- model
      
      npar <- (2*(ord+1))
      
      ll <- logLik(model,marginal = TRUE)
      AIC <- -2*ll+2*npar
      BIC <- -2*ll+log(nobs)*npar
      AICc <- AIC + 2*npar*(npar+1)/(nobs-npar-1)
      
      pred <- predict(model,n.ahead=n.ahead,interval="prediction",level=0.9, filtered = filtered)
      pred <- ts(pred[,1],start=i.n.ahead)
      
      MSE <- MSE(ts.test, pred)
      NMSE <- NMSE(ts.test, pred, ts)
      MAPE <- TSPred::MAPE(ts.test, pred)
      sMAPE <- TSPred::sMAPE(ts.test, pred)
      MaxError <- TSPred::MAXError(ts.test, pred)
      
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
  prediction <- predict(fitPR,n.ahead=n.ahead,interval="prediction",level=0.9, filtered=filtered, se.fit=se.fit)
  
  MSE <- statsData$MSE
  NMSE <- statsData$NMSE
  MAPE <- statsData$MAPE
  sMAPE <- statsData$sMAPE
  MaxError <- statsData$MaxError
  
  return(list(model=fitPR,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
}


fittestLM <- function(timeseries, timeseries.test, maxorder=5, na.action=na.omit, se.fit=FALSE, filtered=TRUE){
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  oa <- optimArima(timeseries, timeseries.test, na.action=na.action, se.fit=se.fit)
  oaKF <- optimArimaKF(timeseries, timeseries.test, na.action=na.action, se.fit=se.fit, filtered = filtered)
  opr <- optimPolyR(timeseries, timeseries.test, maxorder=maxorder, na.action=na.action, se.fit=se.fit)
  oprKF <- optimPolyRKF(timeseries, timeseries.test, maxorder=maxorder, na.action=na.action, se.fit=se.fit, filtered = filtered)
  
  oa.t <- data.frame(AICc=oa$AICc,AIC=oa$AIC,BIC=oa$BIC,logLik=oa$logLik,
                     MSE=oa$MSE,NMSE=oa$NMSE,MAPE=oa$MAPE,sMAPE=oa$sMAPE,MaxError=oa$MaxError)
  
  oaKF.t <- data.frame(AICc=oaKF$AICc,AIC=oaKF$AIC,BIC=oaKF$BIC,logLik=oaKF$logLik,
                       MSE=oaKF$MSE,NMSE=oaKF$NMSE,MAPE=oaKF$MAPE,sMAPE=oaKF$sMAPE,MaxError=oaKF$MaxError)
  
  opr.t <- data.frame(AICc=opr$AICc,AIC=opr$AIC,BIC=opr$BIC,logLik=opr$logLik,
                     MSE=opr$MSE,NMSE=opr$NMSE,MAPE=opr$MAPE,sMAPE=opr$sMAPE,MaxError=opr$MaxError)
  
  oprKF.t <- data.frame(AICc=oprKF$AICc,AIC=oprKF$AIC,BIC=oprKF$BIC,logLik=oprKF$logLik,
                       MSE=oprKF$MSE,NMSE=oprKF$NMSE,MAPE=oprKF$MAPE,sMAPE=oprKF$sMAPE,MaxError=oprKF$MaxError)
  
  results <- rbind(oa.t,oaKF.t,opr.t,oprKF.t,deparse.level=0)
  rownames(results) <- c("ARIMA","ARIMAKF","PR","PRKF")
  
  criteria <- with(results,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
  TSPredC <- 0
  for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
  
  rank <- cbind(results,TSPredC=TSPredC)
  rank <- rank[with(rank,order(TSPredC)),]
  
  ranked.results <- list()
  for(i in 1:nrow(rank)){
    switch(as.character(rownames(rank)[i]),
                        ARIMA= ranked.results[["ARIMA"]] <- oa,
                        ARIMAKF= ranked.results[["ARIMAKF"]] <- oaKF,
                        PR= ranked.results[["PR"]] <- opr,
                        PRKF= ranked.results[["PRKF"]] <- oprKF)
  }
  
  fittestLM <- ranked.results[[1]]$model
  
  return(list(model=fittestLM,rank=rank,ranked.results=ranked.results))
}



#Example:
fittest <- fittestLM(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE, filtered=TRUE)
View(fittest$rank)



#Plots:
oa <- fittest$ranked.results$ARIMA
oaKF <- fittest$ranked.results$ARIMAKF
opr <- fittest$ranked.results$PR
oprKF <- fittest$ranked.results$PRKF

plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),xlab="Time",ylab="ARIMA")
lines(ts(oa$pred$pred,start=981),lwd=2,col='blue')
lines(ts(oa$pred$pred+oa$pred$se,start=981),lwd=2,col='light blue')
lines(ts(oa$pred$pred-oa$pred$se,start=981),lwd=2,col='light blue')

fs <- KFS(oaKF$model,filtering=c("state","mean"),smoothing=c("state","mean"))
f <- fitted(fs, filtered = TRUE) 
s <- fitted(fs)
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),xlab="Time",ylab="ARIMAKF")
lines(f,col='red',lty=2,lwd=2)
lines(s,col='green',lty=2,lwd=2)
lines(ts(oaKF$pred[,1],start=981),lwd=2,col='blue')
lines(ts(oaKF$pred[,2],start=981),lwd=2,col='light blue')
lines(ts(oaKF$pred[,3],start=981),lwd=2,col='light blue')

plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),xlab="Time",ylab="PR")
lines(ts(opr$pred$fit,start=981),lwd=2,col='blue')
lines(ts(opr$pred$fit+opr$pred$se.fit,start=981),lwd=2,col='light blue')
lines(ts(opr$pred$fit-opr$pred$se.fit,start=981),lwd=2,col='light blue')

fs <- KFS(oprKF$model,filtering=c("state","mean"),smoothing=c("state","mean"))
f <- fitted(fs, filtered = TRUE) 
s <- fitted(fs)
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),xlab="Time",ylab="PRKF")
lines(f,col='red',lty=2,lwd=2)
lines(s,col='green',lty=2,lwd=2)
lines(ts(oprKF$pred[,1],start=981),lwd=2,col='blue')
lines(ts(oprKF$pred[,2],start=981),lwd=2,col='light blue')
lines(ts(oprKF$pred[,3],start=981),lwd=2,col='light blue')