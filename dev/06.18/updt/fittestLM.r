fittestLM <- 
function(timeseries, timeseries.test=NULL, h=NULL, level=0.95, na.action=na.omit, filtered=TRUE,
         order=NULL, minorder=0, maxorder=5, raw = FALSE,initQ=NULL,
         rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){
  #catch parameter errors
  if(is.null(timeseries))    stop("timeseries is required and must have positive length")
  if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
  
  oa <- fittestArima(timeseries, timeseries.test, h, level=level, na.action=na.action, ...)
  oaKF <- fittestArimaKF(timeseries, timeseries.test, h, level=level,initQ=initQ, na.action=na.action, filtered=filtered, rank.by=rank.by)
  opr <- fittestPolyR(timeseries, timeseries.test, h, level=level, na.action=na.action, rank.by=rank.by, order=order, minorder=minorder, maxorder=maxorder, raw=raw)
  oprKF <- fittestPolyRKF(timeseries, timeseries.test, h, level=level, na.action=na.action, filtered=filtered, rank.by=rank.by, order=order, minorder=minorder, maxorder=maxorder, initQ=initQ)

  oa.t <- data.frame(AICc=oa$AICc,AIC=oa$AIC,BIC=oa$BIC,logLik=oa$logLik,
                     MSE=oa$MSE,NMSE=oa$NMSE,MAPE=oa$MAPE,sMAPE=oa$sMAPE,MaxError=oa$MaxError)
  oaKF.t <- data.frame(AICc=oaKF$AICc,AIC=oaKF$AIC,BIC=oaKF$BIC,logLik=oaKF$logLik,
                       MSE=oaKF$MSE,NMSE=oaKF$NMSE,MAPE=oaKF$MAPE,sMAPE=oaKF$sMAPE,MaxError=oaKF$MaxError)
  opr.t <- data.frame(AICc=opr$AICc,AIC=opr$AIC,BIC=opr$BIC,logLik=opr$logLik,
                     MSE=opr$MSE,NMSE=opr$NMSE,MAPE=opr$MAPE,sMAPE=opr$sMAPE,MaxError=opr$MaxError)
  oprKF.t <- data.frame(AICc=oprKF$AICc,AIC=oprKF$AIC,BIC=oprKF$BIC,logLik=oprKF$logLik,
                       MSE=oprKF$MSE,NMSE=oprKF$NMSE,MAPE=oprKF$MAPE,sMAPE=oprKF$sMAPE,MaxError=oprKF$MaxError)
  
  rank <- rbind(oa.t,oaKF.t,opr.t,oprKF.t,deparse.level=0)
  rownames(rank) <- c("ARIMA","ARIMAKF","PR","PRKF")
  
  #create ranking criteria based on all measures referenced by rank.by
  criteria <- rank[ , (names(rank) %in% rank.by), drop = FALSE]
  if("logLik" %in% names(criteria)) criteria["logLik"] <- -criteria["logLik"]
  TSPredC <- 0
  for(c in names(criteria)) TSPredC <- TSPredC + rank(criteria[c])
  names(TSPredC) <- NULL
  
  #ranking the candidate models based on all measures referenced by rank.by
  rank <- cbind(rank,rank.position.sum=TSPredC)
  rank <- rank[with(rank,order(rank.position.sum)),]
  
  
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