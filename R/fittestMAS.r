#Finds and returns the fittest MAS
#maxorder from the sma function of smooth package


#' Automatic prediction with moving average smoothing 
#' 
#' The function uses an automatically produced moving average smoother as base
#' for predicting and returning the next n consecutive values of the provided
#' univariate time series using an also automatically fitted model
#' (\code{\link{ets}}/\code{\link{stlf}} or \code{\link{arima}}). It also
#' evaluates the fitness and prediction accuracy of the produced model.
#' 
#' The function produces a moving average smoother of \code{timeseries} with
#' order \code{order} and uses it as base for model fitting and prediction. If
#' \code{model="arima"}, an arima model is used and automatically fitted using
#' the \code{\link[forecast]{auto.arima}} function. If \code{model="ets"}, the
#' function fits an \code{[forecast]\link{ets}} model (if \code{timeseries} is
#' non-seasonal or the seasonal period is 12 or less) or
#' \code{\link[forecast]{stlf}} model (if the seasonal period is 13 or more).
#' 
#' For producing the prediction of the next \code{h} consecutive values of the
#' provided univariate time series, the function \code{\link{mas.rev}} is used.
#' 
#' If \code{order} is \code{NULL}, it is automatically selected. For that, a
#' set with candidate models constructed for moving average smoothers of orders
#' from \code{minorder} to \code{maxorder} is generated. The default value of
#' \code{maxorder} is set based on code from the \code{sma} function of
#' \code{smooth} package. The value option of \code{order} which generate the
#' best ranked candidate model acoording to the criteria in \code{rank.by} is
#' selected.
#' 
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link{AICc}}, \code{\link{BIC}} or
#' \code{\link{logLik}}). In the former case, the candidate models are used for
#' time series prediction and the error measures are calculated by means of a
#' cross-validation process. In the latter case, the candidate models are
#' fitted and fitness criteria are calculated based on all observations in
#' \code{timeseries}.
#' 
#' If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the
#' candidate models are ranked by all the mentioned prediction error measures
#' or fitness criteria, respectively. The wheight of the ranking criteria is
#' equally distributed. In this case, a \code{rank.position.sum} criterion is
#' produced for ranking the candidate models. The \code{rank.position.sum}
#' criterion is calculated as the sum of the rank positions of a model (1 = 1st
#' position = better ranked model, 2 = 2nd position, etc.) on each calculated
#' ranking criteria. 
#' 
#' @param timeseries A vector or univariate time series. 
#' @param timeseries.test A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. It is used as a
#' testing set and base for calculation of prediction error measures. Ignored
#' if \code{NULL}.
#' @param h Number of consecutive values of the time series to be predicted. If
#' \code{h} is \code{NULL}, the number of consecutive values to be predicted is
#' assumed to be equal to the length of \code{timeseries.test}. Required when
#' \code{timeseries.test} is \code{NULL}. 
#' @param order A numeric integer value corresponding to the order of moving
#' average smoother to be produced. If \code{NULL}, the order of the moving
#' average smoother returned by the function is automatically selected within
#' the interval \code{minorder:maxorder}. See 'Details'. 
#' @param minorder A numeric integer value corresponding to the minimum order
#' of candidate moving average smoothers to be produced and evaluated. Ignored
#' if \code{order} is provided. See 'Details'. 
#' @param maxorder A numeric integer value corresponding to the maximal order
#' of candidate moving average smoothers to be produced and evaluated. Ignored
#' if \code{order} is provided. See 'Details'. 
#' @param model Character string. Indicates which model is to be used for
#' fitting and prediction of the moving average smoothed series. 
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}. 
#' @param level Confidence level for prediction intervals. See the
#' \code{\link[forecast]{forecast}} function of the \code{forecast} package. 
#' @param rank.by Character string. Criteria used for ranking candidate models
#' generated. See 'Details'. 
#' @param ... Additional arguments passed to the modeling functions. 
#' @return A list with components: \item{model}{A list containing information
#' about the best evaluated model.} \item{order}{The order of moving average
#' smoother provided or automatically selected.} \item{ma}{The simple moving
#' average smoother of order \code{order} of the provided time series.}
#' \item{AICc}{Numeric value of the computed AICc criterion of the best
#' evaluated model.} \item{AIC}{Numeric value of the computed AIC criterion of
#' the best evaluated model.} \item{BIC}{Numeric value of the computed BIC
#' criterion of the best evaluated model.} \item{logLik}{Numeric value of the
#' computed log-likelihood of the best evaluated model.} \item{pred}{A list
#' with the components \code{mean}, \code{lower} and \code{upper}, containing
#' the predictions of the best evaluated model and the lower and upper limits
#' for prediction intervals, respectively. All components are time series. See
#' the \code{\link[forecast]{forecast}} function in the \code{forecast}
#' package.} \item{MSE}{Numeric value of the resulting MSE error of prediction.
#' Require \code{timeseries.test}.} \item{NMSE}{Numeric value of the resulting
#' NMSE error of prediction. Require \code{timeseries.test}.}
#' \item{MAPE}{Numeric value of the resulting MAPE error of prediction. Require
#' \code{timeseries.test}.} \item{sMAPE}{Numeric value of the resulting sMAPE
#' error of prediction. Require \code{timeseries.test}.}
#' \item{MaxError}{Numeric value of the maximal error of prediction. Require
#' \code{timeseries.test}.} \item{rank.val}{Data.frame with the fitness or
#' prediction accuracy criteria computed for all candidate models ranked by
#' \code{rank.by}. It has the attribute \code{"ranked.models"}, which is a list
#' of objects containing all the candidate models, also ranked by
#' \code{rank.by}.} \item{rank.by}{Ranking criteria used for ranking candidate
#' models and producing \code{rank.val}.}
#' @author Rebecca Pontes Salles 
#' @seealso \code{\link{fittestEMD}}, \code{\link{fittestWavelet}} 
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords moving average smoother automatic fitting adjustment prediction
#' evaluation criterion errors time series
#' @examples
#' 
#' data(CATS)
#' \donttest{
#' fMAS <- fittestMAS(CATS[,1],h=20,model="arima")
#' 
#' #automatically selected order of moving average
#' mas.order <- fMAS$order
#' }
#' 
#' @export fittestMAS
fittestMAS <- 
  function(timeseries, timeseries.test=NULL, h=NULL, order=NULL, minorder=1, maxorder=min(36,length(ts(na.action(timeseries)))/2), 
           model=c("ets","arima"), level=0.95, na.action=stats::na.omit,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){
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
          ts.test <- utils::head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h
    
    # evaluate choice of model for prediction
    modelType <- match.arg(model)
    
    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")
    
    #transformation function (moving average smoothing)
    MAS <- TSPred::mas
    #reverse transformation function (moving average smoothing)
    revMAS <- TSPred::mas.rev
    
    #transforms series, optimize Model given a set of initial parameters and predicts n.ahead observations
    optim.model <- function(timeseries,order,modelType,n.ahead,level,...){
      #moving average smoothing transformation
      sma <- tryCatch( MAS(timeseries,order) ,
                       error=function(c) NULL)
      if(is.null(sma)) return(NULL)
      
      #require(forecast)
      if(modelType=="ets") {
        fc <- forecast::forecast(sma, h=n.ahead, level=level,...)
      }
      else if(modelType=="arima"){
        #Best fit ARIMA
        fc <- forecast::forecast(forecast::auto.arima(sma,...), h=n.ahead, level=level)
      }
      
      return(fc)
    }
    
    #computes quality measures acoording to rank.by
    fitness.criteria <- function(model){
      #computes quality measures acoording to rank.by
      AIC <- model$aic
      BIC <- model$bic
      AICc <- model$aicc
      ll <- model$loglik
      
      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }
    
    #computes predictions (reverse transforms predictions), and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(fc,i.n.ahead,ts.test,ts,ord){
      #computes predictions using the candidate model
      prediction <- list(mean=fc$mean,lower=fc$lower,upper=fc$upper)
      
      xinit <- utils::tail(as.numeric(ts),ord-1)
      prediction$mean <- revMAS(prediction$mean,xinit,ord,addinit=FALSE)
      attributes(prediction$mean) <- attributes(fc$mean)
      prediction$lower <- revMAS(prediction$lower,xinit,ord,addinit=FALSE)
      attributes(prediction$lower) <- attributes(fc$lower)
      prediction$upper <- revMAS(prediction$upper,xinit,ord,addinit=FALSE)
      attributes(prediction$upper) <- attributes(fc$upper)
      
      pred.mean <- ts(prediction$mean,start=i.n.ahead)
      
      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)
        
        return(list(pred=prediction,errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }
      
      return(list(pred=prediction))
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
      rank <- cbind(rank,rank.position.sum=TSPredC)
      rank <- rank[with(rank,order(rank.position.sum)),]
      
      #candidate models are ranked and included as attribute of rank dataframe 
      models <- models[rank$ModelId]
      attr(rank,"ranked.models") <- models
      
      return(rank)
    }
    
    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    if(is.null(order)){
      # creates the Validation series for parameter optimization
      ts.val <- utils::tail(ts,n.ahead)
      ts.tmp <- utils::head(ts,nobs-n.ahead)
      
      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts
      
      #initial options of pars
      pars.opt <- c(minorder:maxorder)
      
      #produces candidate models and measures in order to select "best" parameters
      models <- list()
      for(par in pars.opt){
        #generates and optimizes candidate Model based on initial parameter values
        fc <- optim.model(ts.tmp,order=par,modelType=modelType,n.ahead=n.ahead,level=level,...)
        if(is.null(fc)) next
        model <- fc$model
        
        #creates candidate model id and saves it in the list models
        ModelId <- paste(paste("MAOrd:",par),modelType,sep="_")
        models[[ModelId]] <- model
        
        if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
          #computes fitness measures and returns a dataframe with them
          rank.measures <- fitness.criteria(model)
        }
        else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
          #computes predictions and prediction error measures
          rank.measures <- pred.criteria(fc,length(ts.tmp)+1,ts.val,ts.tmp,par)$errors
        }
        
        #combine results of the candidate models in the dataframe rank
        rank <- rbind(rank, data.frame(ModelId=ModelId,order=par,rank.measures))
      }
      
      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe 
      rank <- ranking.models(rank,rank.by,models)
      
      #if rank.by is fitness
      order.optim <- rank[1,]$order
    }
    else{
      order.optim <- order
    }
    
    #gets previously optimized Model based on optim parameter values
    #if a ranking based on fitness measures was performed, the whole time series was used for training and the model generated can be reused
    if(any(c("AIC","AICc","BIC","logLik") %in% rank.by) & !is.null(rank)){
      model <- attr(rank,"ranked.models")[[1]]
      fc <- forecast::forecast(model, h=n.ahead, level=level)
    }
    #generates and optimizes Model based on optim parameter values
    else{
      fc <- optim.model(ts,order=order.optim,modelType=modelType,n.ahead=n.ahead,level=level,...)
      model <- fc$model
    }
    
    #moving average smoothed time series
    ma <- MAS(ts,order.optim)
    
    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list
    
    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(fc,(nobs+1),ts.test,ts,order.optim)
    
    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)
    
    #append results in a list
    results <- c( list(model=model), order=order.optim, list(ma=ma), fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)
    
    return(results)
  }