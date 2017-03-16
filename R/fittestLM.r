fittestLM <- 
function(timeseries, timeseries.test, maxorder=5, na.action=na.omit, se.fit=FALSE, filtered=TRUE){
  if(is.null(timeseries) || is.null(timeseries.test) ) stop("timeseries and timeseries.test are required and must have positive length")
  
  oa <- fittestArima(timeseries, timeseries.test, na.action=na.action, se.fit=se.fit)
  oaKF <- fittestArimaKF(timeseries, timeseries.test, na.action=na.action, se.fit=se.fit, filtered = filtered)
  opr <- fittestPolyR(timeseries, timeseries.test, maxorder=maxorder, na.action=na.action, se.fit=se.fit)
  oprKF <- fittestPolyRKF(timeseries, timeseries.test, maxorder=maxorder, na.action=na.action, se.fit=se.fit, filtered = filtered)
  
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