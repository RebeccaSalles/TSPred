fittestTransform <-
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit, 
           trans=c("None","LT","LT10","BCT","PCT","MAS","DT","DIF","SDIF","DIFs","SM","ETS","HW","TF","WT","EMD"),
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")){
    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")
    
    #Necessary for correct transform parameter definitions
    timeseries <- na.action(timeseries)
    timeseries.test <- na.action(timeseries.test)
    
    Tr <- list()
    
    #Original data with ARMA
    if("None" %in% trans){
      Tr[["None"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE, 
                                                      trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL) ,
                                    error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    #Logarithmic transform (LT)
    if("LT" %in% trans){
      LT <- function(x) log(x)
      revLT <- function(x) exp(x)
      #Parameters
      par <- list(trans=LT, transPar=NULL, revTrans=revLT, revTransPar=NULL)
      Tr[["LT"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                              error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    if("LT10" %in% trans){
      LT10 <- function(x) log10(x)
      revLT10 <- function(x) 10^(x)
      #Parameters
      par <- list(trans=LT10, transPar=NULL, revTrans=revLT10, revTransPar=NULL)
      Tr[["LT10"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                  trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                                error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
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
      lambda <- tryCatch( car::powerTransform(timeseries)$roundlam , error=function(c) NA)
      #Parameters
      par <- list(trans=BCT, transPar=list(lambda=lambda), revTrans=revBCT, revTransPar=list(lambda=lambda))
      Tr[["BCT"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                 trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                               error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
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
      Tr[["PCT"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                 trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                               error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
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
      fMAS <- fittestMAS(timeseries,timeseries.test,h,model="arima",max.d=0,max.D=0,stationary=TRUE,na.action=na.action,order=NULL,rank.by=rank.by)
      order <- fMAS$order
      #Parameters
      par <- list(trans=MAS, transPar=list(order=order), revTrans=revMAS, revTransPar=list(xinit=tail(timeseries,order-1),order=order,addinit=FALSE))
      Tr[["MAS"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                 trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                               error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    #Detrending
    detrend <- function(x,trend){
      x-trend
    }
    revDetrend <- function(residuals,trend){
      residuals+trend
    }
    if("DT" %in% trans){
      #Fittest polynomial regression
      fpoly <- fittestPolyR(timeseries,timeseries.test,h,order=NULL,na.action=na.action,rank.by=rank.by)
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fpoly$model)), revTrans=revDetrend, revTransPar=list(trend=fpoly$pred))
      Tr[["DT"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                              error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    #Differencing (code from Forecasting: principles and practice, Hyndman and Athanasopoulos, 8.1 Stationarity and differencing, https://www.otexts.org/fpp/8/1)
    if("DIF" %in% trans){
      #Analise de fittness e previsao
      Tr[["DIF"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.D=0, 
                                                 trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL) ,
                               error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    if("SDIF" %in% trans){
      Tr[["SDIF"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, 
                                                  trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL) ,
                                error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    if("DIFs" %in% trans){
      Tr[["DIFs"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, 
                                                  trans=NULL, transPar=NULL, revTrans=NULL, revTransPar=NULL) ,
                                error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    
    #Models allowing detrending and differencing
    #Differencing
    #Structural Model (trend term = ARIMA) estimated by Kalman Filter
    if("SM" %in% trans){
      Tr[["SM"]] <- tryCatch( fittestArimaKF(timeseries,timeseries.test,h,initQ=NULL,filtered=TRUE,na.action=na.action,rank.by=rank.by) ,
                                  error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    #Detrending
    #Exponential smoothing (ETS)
    if("ETS" %in% trans){
      fets <- ses(timeseries, h=length(timeseries.test), initial="optimal")
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fets$model)), revTrans=revDetrend, revTransPar=list(trend=fets$mean))
      Tr[["ETS"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                    trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                                  error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    #Holt-Winter's exponential smoothing (Level, Trend and Seasonality)
    if("HW" %in% trans){
      fhw <- tryCatch( hw(timeseries, h=length(timeseries.test), initial="optimal") ,
                       error = function(c) holt(timeseries, h=length(timeseries.test), initial="optimal"))
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(fhw$model)), revTrans=revDetrend, revTransPar=list(trend=fhw$mean))
      Tr[["HW"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                   trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                                 error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    #Theta Forecasting
    if("TF" %in% trans){
      ftf <- thetaf(timeseries, h=length(timeseries.test))
      #Parameters
      par <- list(trans=detrend, transPar=list(trend=fitted(ftf)), revTrans=revDetrend, revTransPar=list(trend=ftf$mean))
      Tr[["TF"]] <- tryCatch( fittestArimaTrans(timeseries,timeseries.test,h, na.action=na.action, max.d=0, max.D=0, stationary=TRUE,
                                                   trans=par$trans, transPar=par$transPar, revTrans=par$revTrans, revTransPar=par$revTransPar) ,
                                 error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    
    #Wavelet transform
    if("WT" %in% trans){
      Tr[["WT"]] <- tryCatch( fittestWavelet(timeseries, timeseries.test,h,model="arima",max.d=0,max.D=0,stationary=TRUE,
                                             filters=NULL,n.levels=NULL,maxlevel=NULL,na.action=na.action,rank.by=rank.by) ,
                              error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    
    #EMD
    if("EMD" %in% trans){
      Tr[["EMD"]] <- tryCatch( fittestEMD(timeseries, timeseries.test,h,max.imf=10,boundary=NULL,na.action=na.action,rank.by=rank.by) ,
                               error=function(c) list(AICc=NA,AIC=NA,BIC=NA,logLik=NA,MSE=NA,NMSE=NA,MAPE=NA,sMAPE=NA,MaxError=NA,pred=NA)
      )
    }
    
    
    results <- NULL
    for(t in trans){
      pred.t <- data.frame(AICc=Tr[[t]]$AICc,AIC=Tr[[t]]$AIC,BIC=Tr[[t]]$BIC,logLik=Tr[[t]]$logLik,
                           MSE=Tr[[t]]$MSE,NMSE=Tr[[t]]$NMSE,MAPE=Tr[[t]]$MAPE,sMAPE=Tr[[t]]$sMAPE,MaxError=Tr[[t]]$MaxError)
      
      rbind(results,pred.t,deparse.level=0) -> results
    }
    rownames(results) <- trans
    
	# evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
	
    #ranks candidate models
    #create ranking criteria based on all measures referenced by rank.by
    criteria <- results[ , (names(results) %in% rank.by), drop = FALSE]
    if("logLik" %in% names(criteria)) criteria["logLik"] <- -criteria["logLik"]
    TSPredC <- 0
    for(c in names(criteria)) TSPredC <- TSPredC + rank(criteria[c])
    names(TSPredC) <- NULL
    
    #ranking the candidate models based on all measures referenced by rank.by
    rank <- cbind(results,rank.position.sum=TSPredC)
    rank <- rank[with(rank,order(rank.position.sum)),]
    
    #candidate results are ranked
    ranked.results <- Tr[as.character(rownames(rank))]

    return(list(rank=rank,ranked.results=ranked.results))
  }