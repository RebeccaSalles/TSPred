fittestArimaTrans <- 
function(timeseries, timeseries.valid, na.action=na.omit, max.d=2, max.D=1, stationary=FALSE, 
         trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL){
  
  if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
  
  ts <- ts(na.action(timeseries))
  nobs <- length(ts)
  ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
  n.ahead <- length(ts.valid)
  
  original_ts <- ts
  if(!is.null(trans)) ts <- do.call(trans, c(list(ts),transPar))
  
  #Best fit ARIMA
  fitARIMA <- forecast::auto.arima(ts, max.d=max.d, max.D=max.D, stationary=stationary)
  
  #Stats
  AIC <- fitARIMA$aic
  BIC <- fitARIMA$bic
  AICc <- fitARIMA$aicc
  ll <- fitARIMA$loglik
  
  #Prediction errors
  prediction <- forecast::forecast(fitARIMA, h=n.ahead)
  prediction <- list(pred=prediction$mean,lower=prediction$lower,upper=prediction$upper)
  
  if(!is.null(trans) & !is.null(revTrans)){
    for(i in 1:length(prediction)) prediction[[i]] <- do.call(revTrans, c(list(prediction[[i]]),revTransPar))
  }
  
  pred <- ts(prediction$pred,start=(nobs+1))
  
  MSE <- TSPred::MSE(ts.valid, pred)
  NMSE <- TSPred::NMSE(ts.valid, pred, original_ts)
  MAPE <- TSPred::MAPE(ts.valid, pred)
  sMAPE <- TSPred::sMAPE(ts.valid, pred)
  MaxError <- TSPred::MAXError(ts.valid, pred)
  
  return(list(model=fitARIMA,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
              pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError))
}

fittestTrans <- 
function(timeseries, timeseries.valid, na.action=na.omit, max.d=2, max.D=1, stationary=FALSE, 
         test=c("trans","diff","sdiff","alldiffs","arimakf"), trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL){
  if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
  
  test <- match.arg(test)
  #Previsao com dados originais
  pred.xt <- switch(test,
               trans = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=max.d, max.D=max.D, stationary=stationary, 
                                         trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
               diff = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE, 
                                        trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
               sdiff = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE, 
                                         trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
               alldiffs = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE, 
                                            trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
               arimakf = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=max.d, max.D=max.D, stationary=FALSE, 
                                           trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL)
              )
  #Previsao com dados transformados (reverter transformacao)
  pred.xtrans <- switch(test,
                    trans = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=max.d, max.D=max.D, stationary=stationary,
                                              trans=trans, transPar=transPar, revTrans=revTrans, revTransPar=revTransPar),
                    diff = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=max.d, max.D=0, stationary=FALSE, 
                                             trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
                    sdiff = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=max.D, stationary=FALSE, 
                                              trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
                    alldiffs = fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=max.d, max.D=max.D, stationary=FALSE, 
                                                 trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL),
                    arimakf = fittestArimaKF(timeseries,timeseries.valid, na.action=na.action)
                  )

  pred.xt.t <- data.frame(AICc=pred.xt$AICc,AIC=pred.xt$AIC,BIC=pred.xt$BIC,logLik=pred.xt$logLik,
                     MSE=pred.xt$MSE,NMSE=pred.xt$NMSE,MAPE=pred.xt$MAPE,sMAPE=pred.xt$sMAPE,MaxError=pred.xt$MaxError)
  
  pred.xtrans.t <- data.frame(AICc=pred.xtrans$AICc,AIC=pred.xtrans$AIC,BIC=pred.xtrans$BIC,logLik=pred.xtrans$logLik,
                       MSE=pred.xtrans$MSE,NMSE=pred.xtrans$NMSE,MAPE=pred.xtrans$MAPE,sMAPE=pred.xtrans$sMAPE,MaxError=pred.xtrans$MaxError)

  results <- rbind(pred.xt.t,pred.xtrans.t,deparse.level=0)
  rownames(results) <- c("original","transformed")
  
  criteria <- with(results,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
  TSPredC <- 0
  for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
  
  rank <- cbind(results,TSPredC=TSPredC)
  rank <- rank[with(rank,order(TSPredC)),]
  
  ranked.results <- list()
  for(i in 1:nrow(rank)){
    switch(as.character(rownames(rank)[i]),
           original= ranked.results[["original"]] <- pred.xt,
           transformed= ranked.results[["transformed"]] <- pred.xtrans)
  }
  
  fittestTrans <- ranked.results[[1]]$model
  
  return(list(model=fittestTrans,rank=rank,ranked.results=ranked.results))
}


#maxorder from the sma function of smooth package
fittestMAS <- 
  function(timeseries, timeseries.valid, minorder=1, maxorder=min(36,length(ts(na.action(timeseries)))/2), 
           model=c("ets","arima"), max.d=2, max.D=1, stationary=FALSE, level=0.95, na.action=na.omit){
    
    if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
    modelType <- match.arg(model)
    
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
    n.ahead <- length(ts.valid)
    
    MAS <- function(x,order){
      n <- length(x)
      xt <- NULL
      for(t in 1:(n-order+1)){
        xt <- c(xt, sum(x[t:(t+order-1)])/order)
      }
      ts(xt)
    }
    revMAS <- function(xm,xinit,order,addinit=TRUE){
      n <- length(xm)
      x <- xinit
      if(order>1){ 
        for(t in order:(n+order-1)){
          x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
        }
      }
      else x <- xm
      if(addinit) ts(x)
      else ts( tail(x,n) )
    }
    
    models <- list()
    rank <- NULL
    for(ord in minorder:maxorder){
      #moving average smoothing transformation
      sma <- MAS(ts,ord)
      
      if(modelType=="ets") {
        fc <- forecast::forecast(sma, h=n.ahead, level=level)
      }
      else if(modelType=="arima"){
        #Best fit ARIMA
        fc <- forecast::forecast(forecast::auto.arima(sma, max.d=max.d, max.D=max.D, stationary=stationary), h=n.ahead, level=level)
      }
      
      ModelId <- paste(paste("MAOrd:",ord),modelType,sep="_")
      model <- fc$model
      models[[ModelId]] <- model
      
      #Stats
      AIC <- model$aic
      BIC <- model$bic
      AICc <- model$aicc
      ll <- model$loglik
      
      #Prediction errors
      prediction <- list(pred=fc$mean,lower=fc$lower,upper=fc$upper)
      
      xinit <- tail(as.numeric(ts),ord-1)
      prediction$pred <- revMAS(prediction$pred,xinit,ord,addinit=FALSE)
      attributes(prediction$pred) <- attributes(fc$mean)
      prediction$lower <- revMAS(prediction$lower,xinit,ord,addinit=FALSE)
      attributes(prediction$lower) <- attributes(fc$lower)
      prediction$upper <- revMAS(prediction$upper,xinit,ord,addinit=FALSE)
      attributes(prediction$upper) <- attributes(fc$upper)
      
      pred <- ts(prediction$pred,start=(nobs+1))
      
      MSE <- TSPred::MSE(ts.valid, pred)
      NMSE <- TSPred::NMSE(ts.valid, pred, ts)
      MAPE <- TSPred::MAPE(ts.valid, pred)
      sMAPE <- TSPred::sMAPE(ts.valid, pred)
      MaxError <- TSPred::MAXError(ts.valid, pred)
      
      rank <- rbind(rank,
                    data.frame(ModelId=ModelId,Order=ord,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
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
    
    fit <- models[[1]]
    
    statsData <- rank[1,]
    
    order <- statsData$Order
    ma <- MAS(ts,order)
    
    #Stats
    AIC <- statsData$AIC
    BIC <- statsData$BIC
    AICc <- statsData$AICc
    ll <- statsData$logLik
    
    #Prediction errors
    fc <- forecast::forecast(fit, h=n.ahead)
    prediction <- list(pred=fc$mean,lower=fc$lower,upper=fc$upper)
    
    MSE <- statsData$MSE
    NMSE <- statsData$NMSE
    MAPE <- statsData$MAPE
    sMAPE <- statsData$sMAPE
    MaxError <- statsData$MaxError
    
    return(list(order=order,ma=ma,model=fit,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
                pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
  }

#References:
#[1] Day-Ahead Electricity Price Forecasting Using the Wavelet Transform and ARIMA Models, Conejo et al.
#[2] Time series forecasting based on wavelet filtering, Joo and Kim
#[3] A Wavelet Based Prediction Method for Time Series, Stolojescu et al.
fittestWavelet <- 
  function(timeseries, timeseries.valid, filters=c("haar", "d4", "la8", "bl14", "c6"), maxlevel=NULL, 
           model=c("ets","arima"), max.d=2, max.D=1, stationary=FALSE, level=0.95, na.action=na.omit){
    
    if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
    modelType <- match.arg(model)
    
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
    n.ahead <- length(ts.valid)
    
    if(is.null(filters)){
      filters <- c("haar", "d4", "d6", "d8", "d10", "d12", "d14", "d16", "d18", "d20",
                   "la8", "la10", "la12", "la14", "la16", "la18", "la20", 
                   "bl14", "bl18", "bl20", 
                   "c6", "c12", "c18", "c24", "c30") #Filters (mother wavelets) characteristics can be seen in [3]
    }
    
    models <- prediction <- list()
    rank <- NULL
    AIC <- BIC <- AICc <- ll <- list()
    for(f in filters){
      filter <- wavelets::wt.filter(f, modwt=TRUE)
      L <- filter@L
      
      # determine the maximal level of decomposition
      if(missing(maxlevel) | is.null(maxlevel)){
        maxlevel <- as.integer(floor(log(((nobs-1)/(L-1))+1)/log(2)))
      }
      
      wdecomp <- wavelets::modwt(ts, filter=filter, n.levels=maxlevel)
      for(i in 1:maxlevel){
        if(modelType=="ets") {
          Wimodel <- forecast::ets(wdecomp@W[[i]])
          Vimodel <- forecast::ets(wdecomp@V[[i]])
        }
        else if(modelType=="arima"){
          Wimodel <- forecast::auto.arima(wdecomp@W[[i]], max.d=max.d, max.D=max.D, stationary=stationary)
          Vimodel <- forecast::auto.arima(wdecomp@V[[i]], max.d=max.d, max.D=max.D, stationary=stationary)
        }
        
        fcW <- forecast::forecast(Wimodel, h=n.ahead, level=level)
        fcV <- forecast::forecast(Vimodel, h=n.ahead, level=level)
        prediction[[i]] <- list(W=list(pred=fcW$mean,lower=fcW$lower,upper=fcW$upper),
                                V=list(pred=fcV$mean,lower=fcV$lower,upper=fcV$upper))
        #Stats
        AIC[[i]] <- Vimodel$aic #Vi is the main component of the WT of level i acoording to [1]
        BIC[[i]] <- Vimodel$bic
        AICc[[i]] <- Vimodel$aicc
        ll[[i]] <- Vimodel$loglik
      }
      
      for(level in 1:maxlevel){ #Methodology for choosing the optimal decomposition level acoording to [2]
        wdecomp <- wavelets::modwt(ts, filter=filter, n.levels=level)
        wt <- wdecomp
        
        newseries <- c(wdecomp@series,rep(NA,n.ahead))
        wdecomp@series <- as.matrix(newseries)
        wdecomp@attr.X <- attributes(ts(newseries))
        wdecompPred <- wdecomp
        
        for(i in 1:level){
          wdecomp@W[[i]] <- as.matrix(c(wdecomp@W[[i]],prediction[[i]]$W$lower))
          wdecomp@V[[i]] <- as.matrix(c(wdecomp@V[[i]],prediction[[i]]$V$lower))
        }
        iwdecomp <- wavelets::imodwt(wdecomp)
        lower <- ts(tail(iwdecomp,n.ahead),start=(nobs+1))

        wdecomp <- wdecompPred
        for(i in 1:level){
          wdecomp@W[[i]] <- as.matrix(c(wdecomp@W[[i]],prediction[[i]]$W$upper))
          wdecomp@V[[i]] <- as.matrix(c(wdecomp@V[[i]],prediction[[i]]$V$upper))
        }
        iwdecomp <- wavelets::imodwt(wdecomp)
        upper <- ts(tail(iwdecomp,n.ahead),start=(nobs+1))
        
        wdecomp <- wdecompPred
        for(i in 1:level){
          wdecomp@W[[i]] <- as.matrix(c(wdecomp@W[[i]],prediction[[i]]$W$pred))
          wdecomp@V[[i]] <- as.matrix(c(wdecomp@V[[i]],prediction[[i]]$V$pred))
        }
        iwdecomp <- wavelets::imodwt(wdecomp)
        pred <- ts(tail(iwdecomp,n.ahead),start=(nobs+1))
        
        #Prediction errors
        MSE <- TSPred::MSE(ts.valid, pred)
        NMSE <- TSPred::NMSE(ts.valid, pred, ts)
        MAPE <- TSPred::MAPE(ts.valid, pred)
        sMAPE <- TSPred::sMAPE(ts.valid, pred)
        MaxError <- TSPred::MAXError(ts.valid, pred)
        
        ModelId <- paste(paste("Filter:",f),paste("Level:",level),sep="_")
        models[[ModelId]] <- list(wt=wt,pred=pred,lower=lower,upper=upper)
        
        rank <- rbind(rank,
                      data.frame(ModelId=ModelId,AICc=AICc[[level]],AIC=AIC[[level]],BIC=BIC[[level]],logLik=ll[[level]],
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
    
    fit <- models[[1]]$wt
    
    statsData <- rank[1,]
    
    #Stats
    AIC <- statsData$AIC
    BIC <- statsData$BIC
    AICc <- statsData$AICc
    ll <- statsData$logLik
    
    #Prediction errors
    prediction <- list(pred=models[[1]]$pred,lower=models[[1]]$lower,upper=models[[1]]$upper)
    
    MSE <- statsData$MSE
    NMSE <- statsData$NMSE
    MAPE <- statsData$MAPE
    sMAPE <- statsData$sMAPE
    MaxError <- statsData$MaxError
    
    return(list(WT=fit,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
                pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
  }


#References:
#[1] A Hilbert-Huang transform approach for predicting cyber-attacks, Kim et al.
fittestEMD <- 
  function(timeseries, timeseries.valid, max.imf=10, level=0.95, na.action=na.omit){
    
    if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
    
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    ts.valid <- ts(na.action(timeseries.valid),start=(nobs+1))
    n.ahead <- length(ts.valid)
    
    #transform
    emdt <- EMD::emd(ts, max.imf=max.imf)
    
    models <- prediction <- list()
    rank <- NULL
    AIC <- BIC <- AICc <- ll <- 0
    
    #prediction of residue
    fpoly <- fittestPolyR(emdt$residue, tail(emdt$residue,n.ahead), level=level, se.fit=TRUE) #check use of validation data!
    prediction[["residue"]] <- fpoly$pred
    
    #inverse transforms
    for(min.imf in (emdt$nimf-1):1){
      varModel <- vars::VAR(emdt$imf[,c(min.imf:emdt$nimf)])
      prediction[["imfs"]] <- predict(varModel,n.ahead=n.ahead, ci=level)
      
      iemdt <- EMD::emd.pred(prediction[["imfs"]],prediction[["residue"]], figure=FALSE)
      
      pred <- iemdt$fcst
      lower <- iemdt$lower
      upper <- iemdt$upper
      
      #Prediction errors
      MSE <- TSPred::MSE(ts.valid, pred)
      NMSE <- TSPred::NMSE(ts.valid, pred, ts)
      MAPE <- TSPred::MAPE(ts.valid, pred)
      sMAPE <- TSPred::sMAPE(ts.valid, pred)
      MaxError <- TSPred::MAXError(ts.valid, pred)
      
      #Stats
      AIC <- AIC(varModel)
      BIC <- BIC(varModel)
      ll <- logLik(varModel)
      df <- attr(ll,"df")
      AICc <- AIC + 2*df*(df+1)/(nobs-df-1)
      
      ModelId <- paste(paste("Level:",max.imf),paste("Imfs:",paste(min.imf,emdt$nimf,sep="-")),sep="_")
      models[[ModelId]] <- list(var=varModel,meaningfulImfs=min.imf:emdt$nimf,pred=pred,lower=lower,upper=upper)
      
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
    
    fit <- emdt
    varImfs <- models[[1]]$var
    meaningfulImfs <- models[[1]]$meaningfulImfs
    polyRresidue <- fpoly$model
    
    statsData <- rank[1,]
    
    #Stats
    AIC <- statsData$AIC
    BIC <- statsData$BIC
    AICc <- statsData$AICc
    ll <- statsData$logLik
    
    #Prediction errors
    prediction <- list(pred=models[[1]]$pred,lower=models[[1]]$lower,upper=models[[1]]$upper)
    
    MSE <- statsData$MSE
    NMSE <- statsData$NMSE
    MAPE <- statsData$MAPE
    sMAPE <- statsData$sMAPE
    MaxError <- statsData$MaxError
    
    return(list(emd=fit,meaningfulImfs=meaningfulImfs,varImfs=varImfs,polyRresidue=polyRresidue,AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll,
                pred=prediction,MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError,rank=rank))
  }



fittestTransExp <- 
  function(timeseries, timeseries.valid, na.action=na.omit, 
           trans=c("original","LT","LT10","BCT","PCT","MAS","DT","DIF","SDIF","DIFs","SM_DIF","ETS_DT","HW_DT","TF_DT","WT","EMD")){
    if(is.null(timeseries) || is.null(timeseries.valid) ) stop("timeseries and timeseries.valid are required and must have positive length")
    
    Tr <- list()
    
    #Original data with ARMA
    if("original" %in% trans){
      Tr[["original"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE, 
                                            trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL)
    }
    
    #Logarithmic transform (LT)
    if("LT" %in% trans){
      LT <- function(x) log(x)
      revLT <- function(x) exp(x)
      #Parameters
      par <- list(trans=LT, transPar=NULL, revTrans=revLT, revTransPar=NULL)
      Tr[["LT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                      trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    if("LT10" %in% trans){
      LT10 <- function(x) log10(x)
      revLT10 <- function(x) 10^(x)
      #Parameters
      par <- list(trans=LT10, transPar=NULL, revTrans=revLT10, revTransPar=NULL)
      Tr[["LT10"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                        trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    #Box-Cox transform (BCT)
    if("BCT" %in% trans){
      BCT <- function(x,lambda){
        if (lambda == 0) log( x )
        else ( (x) ^ lambda - 1 ) / lambda
      }
      revBCT <- function(x,lambda){
        if (lambda == 0) exp(x)
        else (x*lambda +1)^(1/lambda)
      }
      #Maximum likelihood-like estimate for the power parameter of the Box-Cox transform
      require("car")
      lambda <- powerTransform(timeseries)$roundlam
      #Parameters
      par <- list(trans=BCT, transPar=list(lambda=lambda), revTrans=revBCT, revTransPar=list(lambda=lambda))
      Tr[["BCT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                       trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    
    #Percentage change transform (PCT)
    if("PCT" %in% trans){
      PCT <- function(x){
        lag <- 1
        n <- length(x)
        xt <- x[(1+lag):n]
        xt_1 <- x[1:(n-lag)]
        log(xt)-log(xt_1)
      }
      revPCT <- function(p,x0){
        xt <- x0*(1+p[1])
        for(i in 2:length(p)) xt <- c(xt, (1+p[i])*xt[length(xt)] )
        xt
      }
      #Parameters
      par <- list(trans=PCT, transPar=NULL, revTrans=revPCT, revTransPar=list(x0=timeseries[[length(timeseries)]]))
      Tr[["PCT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                       trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    
    #Moving average smoother (MAS)
    if("MAS" %in% trans){
      MAS <- function(x,order){
        n <- length(x)
        xt <- NULL
        for(t in 1:(n-order+1)){
          xt <- c(xt, sum(x[t:(t+order-1)])/order)
        }
        ts(xt)
      }
      revMAS <- function(xm,xinit,order,addinit=TRUE){
        n <- length(xm)
        x <- xinit
        if(order>1){ 
          for(t in order:(n+order-1)){
            x <- c(x, xm[t-order+1]*order-sum(x[(t-1):(t-order+1)]))
          }
        }
        else x <- xm
        if(addinit) ts(x)
        else ts( tail(x,n) )
      }
      #Fittest order parameter of the moving average smoothing based on log likelihood and prediction accuracy
      fMAS <- fittestMAS(timeseries, timeseries.valid, model="arima",max.d=0, max.D=0, stationary=TRUE)
      order <- fMAS$order
      #Parameters
      par <- list(trans=MAS, transPar=list(order=order), revTrans=revMAS, revTransPar=list(xinit=tail(timeseries,order-1),order=order,addinit=FALSE))
      Tr[["MAS"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                       trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    
    #Detrending
    if("DT" %in% trans){
      detrend <- function(x,trend){
        x-trend
      }
      revDetrend <- function(residuals,trend){
        residuals+trend
      }
      #Fittest polynomial regression
      fpoly <- fittestPolyR(timeseries, timeseries.valid)
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fpoly$model)), revTrans=revDetrend, revTransPar=list(trend=fpoly$pred))
      Tr[["DT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                      trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    
    #Differencing (code from Forecasting: principles and practice, Hyndman and Athanasopoulos, 8.1 Stationarity and differencing, https://www.otexts.org/fpp/8/1)
    if("DIF" %in% trans){
      require(forecast)
      #Analise de fittness e previsao
      Tr[["DIF"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.D=0, 
                                       trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL)
    }
    if("SDIF" %in% trans){
      Tr[["SDIF"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, 
                                        trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL)
    }
    if("DIFs" %in% trans){
      Tr[["DIFs"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, 
                                        trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL)
    }
    
    
    
    #Models allowing detrending and differencing
    #Differencing
    #Structural Model (trend term = ARIMA) estimated by Kalman Filter
    if("SM_DIF" %in% trans){
      Tr[["SM_DIF"]] <- fittestArimaKF(timeseries,timeseries.valid, na.action=na.action)
    }
    
    #Detrending
    #Exponential smoothing (ETS)
    if("ETS_DT" %in% trans){
      fets <- ses(timeseries, h=length(timeseries.valid), initial="optimal")
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fets$model)), revTrans=revDetrend, revTransPar=list(trend=fets$mean))
      Tr[["ETS_DT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                          trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    #Holt-Winter's exponential smoothing (Level, Trend and Seasonality)
    if("HW_DT" %in% trans){
      fhw <- tryCatch( hw(timeseries, h=length(timeseries.valid), initial="optimal") ,
                       error = function(c) holt(timeseries, h=length(timeseries.valid), initial="optimal"))
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fhw$model)), revTrans=revDetrend, revTransPar=list(trend=fhw$mean))
      Tr[["HW_DT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                         trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    #Theta Forecasting
    if("TF_DT" %in% trans){
      ftf <- thetaf(timeseries, h=length(timeseries.valid))
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(ftf)), revTrans=revDetrend, revTransPar=list(trend=ftf$mean))
      Tr[["TF_DT"]] <- fittestArimaTrans(timeseries,timeseries.valid, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                         trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar)
    }
    
    
    
    #Wavelet transform
    if("WT" %in% trans){
      require(wavelets)
      Tr[["WT"]] <- fittestWavelet(timeseries, timeseries.valid, model="arima", max.d=0, max.D=0, stationary=TRUE)
    }
    
    
    
    #EMD
    if("EMD" %in% trans){
      require(EMD)
      Tr[["EMD"]] <- fittestEMD(timeseries, timeseries.valid, max.imf=10, level=0.95)
    }
    
    
    results <- NULL
    for(t in trans){
      pred.t <- data.frame(AICc=Tr[[t]]$AICc,AIC=Tr[[t]]$AIC,BIC=Tr[[t]]$BIC,logLik=Tr[[t]]$logLik,
                           MSE=Tr[[t]]$MSE,NMSE=Tr[[t]]$NMSE,MAPE=Tr[[t]]$MAPE,sMAPE=Tr[[t]]$sMAPE,MaxError=Tr[[t]]$MaxError)
      
      rbind(results,pred.t,deparse.level=0) -> results
    }
    rownames(results) <- trans
    
    criteria <- with(results,cbind(AIC,AICc,BIC,-logLik,MSE,NMSE,MAPE,sMAPE,MaxError))
    TSPredC <- 0
    for(i in 1:ncol(criteria)) TSPredC <- TSPredC + rank(criteria[,i])
    
    rank <- cbind(results,TSPredC=TSPredC)
    rank <- rank[with(rank,order(TSPredC)),]
    
    ranked.results <- Tr[as.character(rownames(rank))]
    
    fittestTrans <- ranked.results[[1]]$model
    
    return(list(model=fittestTrans,rank=rank,ranked.results=ranked.results))
  }