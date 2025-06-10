#Finds and returns the fittest wavelet  #DO!
#References:
#[1] Day-Ahead Electricity Price Forecasting Using the Wavelet Transform and ARIMA Models, Conejo et al.
#[2] Time series forecasting based on wavelet filtering, Joo and Kim
#[3] A Wavelet Based Prediction Method for Time Series, Stolojescu et al.


#' Automatic prediction with wavelet transform
#'
#' The function automatically applies a maximal overlap discrete wavelet
#' transform to a provided univariate time series. The resulting components of
#' the decomposed series are used as base for predicting and returning the next
#' n consecutive values of the provided univariate time series using also
#' automatically fitted models (\code{\link[forecast]{ets}} or \code{\link{arima}}). It
#' also evaluates fitness and prediction accuracy of the produced models.
#'
#' The function produces a maximal overlap discrete wavelet transform of
#' \code{timeseries}. It performs a time series decomposition of level
#' \code{n.levels} using the wavelet filter \code{filters}. See the
#' \code{\link[wavelets]{modwt}} function. Each component series resulting from
#' the decomposition (\code{n.levels} wavelet coefficients series and
#' \code{n.levels} scaling coefficients series) is separately used as base for
#' model fitting and prediction. If \code{model="arima"}, arima models are used
#' and automatically fitted using the \code{\link[forecast]{auto.arima}}
#' function. If \code{model="ets"}, the function fits
#' \code{[forecast]\link[forecast]{ets}} models. The set of predictions for all component
#' series are then reversed transformed in order to produce the next \code{h}
#' consecutive values of the provided univariate time series in
#' \code{timeseries}. See the \code{\link[wavelets]{imodwt}} function.
#'
#' If \code{length(filters)>1} or \code{filters=NULL}, it is automatically
#' selected. For that, a set of candidate wavelet decompositions with different
#' options of filters is generated and used for model fitting and prediction.
#' Also, if \code{n.levels} is \code{NULL}, it is automatically set as a value
#' within the interval \code{1:maxlevel} (if \code{maxlevel} is not provided,
#' it is calculated according to the wavelet filter based on code from
#' \code{\link[wavelets]{modwt}}). For that, candidate decompositions are
#' specified with different levels. The options of filter and/or level of
#' decomposition which generate the best ranked model fitness/predictions
#' acoording to the criteria in \code{rank.by} are selected.
#'
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link[MuMIn]{AICc}}, \code{\link{BIC}} or
#' \code{\link{logLik}}). In the former case, the candidate wavelet
#' decompositions are used for time series prediction and the error measures
#' are calculated by means of a cross-validation process. In the latter case,
#' the component series of the candidate decompositions are modeled and model
#' fitness criteria are calculated based on all observations in
#' \code{timeseries}. In particular, the fitness criteria calculated for
#' ranking the candidate decomposition correspond to the model produced for the
#' \code{n.levels}th scaling coefficients series as it can be considered the
#' main component of a decomposition of level \code{n.levels} (Conejo,2005).
#'
#' If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the
#' candidate decompositions are ranked by all the mentioned prediction error
#' measures or fitness criteria, respectively. The wheight of the ranking
#' criteria is equally distributed. In this case, a \code{rank.position.sum}
#' criterion is produced for ranking the candidate decompositions. The
#' \code{rank.position.sum} criterion is calculated as the sum of the rank
#' positions of a decomposition (1 = 1st position = better ranked model, 2 =
#' 2nd position, etc.) on each calculated ranking criteria.
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
#' @param filters A vector containing character strings indicating which
#' wavelet filter to use in the decomposition. If \code{length(filters)>1}, the
#' wavelet transform filter used for generating the return of the function is
#' automatically selected. If \code{NULL}, all supported filters are considered
#' for automatic selection. See 'Details'. For more details on all the
#' supported filters and corresponding character strings see
#' \code{\link[wavelets]{wt.filter}}.
#' @param n.levels An integer specifying the level of the decomposition. If
#' \code{NULL}, the level of the wavelet decomposition returned by the function
#' is automatically selected within the interval \code{1:maxlevel}. See
#' 'Details'.
#' @param maxlevel A numeric integer value corresponding to the maximal level
#' of candidate wavelet decompositions to be produced and evaluated. If
#' \code{NULL}, \code{maxlevel} is set as
#' \code{floor(log(((nobs-1)/(L-1))+1)/log(2))}, where
#' \code{nobs=length(timeseries)} and \code{L} is the length of the wavelet and
#' scaling filters. See \code{\link[wavelets]{modwt}} and
#' \code{\link[wavelets]{wt.filter}}. Ignored if \code{n.levels} is provided.
#' See 'Details'.
#' @param boundary Character string. Indicates which boundary method to use.
#' See \code{\link[wavelets]{modwt}}.
#' @param model Character string. Indicates which model is to be used for
#' fitting and prediction of the components of the decomposed series.
#' @param conf.level Confidence level for prediction intervals. See the
#' \code{\link[forecast]{forecast}} function of the \code{forecast} package. %%
#' ~~Describe \code{na.action} here~~
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}.
#' @param rank.by Character string. Criteria used for ranking candidate
#' decompositions/models/predictions generated during parameter selection. See
#' 'Details'.
#' @param ... Additional arguments passed to the modeling functions. %%
#' ~~Describe \code{na.action} here~~
#' @return A list with components: \item{WT}{An object of class
#' \code{\link[wavelets]{modwt}} containing the wavelet transformed/decomposed
#' time series.} \item{level}{The level of wavelet decomposition provided or
#' automatically selected.} \item{filter}{A character string indicating the
#' (provided or automatically selected) wavelet filter used in the
#' decomposition.} \item{AICc}{Numeric value of the computed AICc criterion of
#' the fitted model for the \code{level}th scaling coefficients series.}
#' \item{AIC}{Numeric value of the computed AIC criterion of the fitted model
#' for the \code{level}th scaling coefficients series.} \item{BIC}{Numeric
#' value of the computed BIC criterion of the fitted model for the
#' \code{level}th scaling coefficients series.} \item{logLik}{Numeric value of
#' the computed log-likelihood of the fitted model for the \code{level}th
#' scaling coefficients series.} \item{pred}{A list with the components
#' \code{mean}, \code{lower} and \code{upper}, containing the predictions based
#' on the best evaluated decomposition and the lower and upper limits for
#' prediction intervals, respectively. All components are time series. See the
#' \code{\link[forecast]{forecast}} function in the \code{forecast} package.}
#' \item{MSE}{Numeric value of the resulting MSE error of prediction. Require
#' \code{timeseries.test}.} \item{NMSE}{Numeric value of the resulting NMSE
#' error of prediction. Require \code{timeseries.test}.} \item{MAPE}{Numeric
#' value of the resulting MAPE error of prediction. Require
#' \code{timeseries.test}.} \item{sMAPE}{Numeric value of the resulting sMAPE
#' error of prediction. Require \code{timeseries.test}.}
#' \item{MaxError}{Numeric value of the maximal error of prediction. Require
#' \code{timeseries.test}.} \item{rank.val}{Data.frame with the fitness or
#' prediction accuracy criteria computed based on all candidate decompositions
#' ranked by \code{rank.by}. It has the attribute \code{"ranked.wt"}, which is
#' a list of \code{\link[wavelets]{modwt}} objects containing all the candidate
#' decompositions, also ranked by \code{rank.by}. Only provided if
#' \code{filters} or \code{n.levels} were automatically selected.}
#' \item{rank.by}{Ranking criteria used for ranking candidate decompositions
#' and producing \code{rank.val}.}
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{fittestEMD}}, \code{\link{fittestMAS}} ~
#' @references A. J. Conejo, M. A. Plazas, R. Espinola, A. B. Molina, Day-ahead
#' electricity price forecasting using the wavelet transform and ARIMA models,
#' IEEE Transactions on Power Systems 20 (2005) 1035-1042.
#'
#' T. Joo, S. Kim, Time series forecasting based on wavelet filtering, Expert
#' Systems with Applications 42 (2015) 3868-3874.
#'
#' C. Stolojescu, I. Railean, S. M. P. Lenca, A. Isar, A wavelet based
#' prediction method for time series. In Proceedings of the 2010 International
#' Conference Stochastic Modeling Techniques and Data Analysis, Chania, Greece
#' (pp. 8-11) (2010). %% ~put references to the literature/web site here ~
#' @keywords wavelet decomposition transform automatic fitting adjustment
#' prediction evaluation criterion errors time series
#' @examples
#'
#' data(CATS)
#' \donttest{
#' fW <- fittestWavelet(CATS[,1],h=20,model="arima")
#'
#' #plot wavelet transform/decomposition
#' plot(fW$WT)
#' }
#'
#' @export fittestWavelet
fittestWavelet <-
  function(timeseries, timeseries.test=NULL, h=1, filters=c("haar", "d4", "la8", "bl14", "c6"), n.levels=NULL, maxlevel=NULL,
           boundary="periodic",model=c("ets","arima"), conf.level=0.95, na.action=stats::na.omit,
           rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){

    #require(wavelets)
    #require(forecast)

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

    # if no set of filters is provided, all possibilities are tested
    if(is.null(filters)){
      filters <- c("haar", "d4", "d6", "d8", "d10", "d12", "d14", "d16", "d18", "d20",
                   "la8", "la10", "la12", "la14", "la16", "la18", "la20",
                   "bl14", "bl18", "bl20",
                   "c6", "c12", "c18", "c24", "c30") #Filters (mother wavelets) characteristics can be seen in [3]
    }

    # evaluate choices of rank.by
    rank.by <- match.arg(rank.by)
    if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
    else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")

    #set maximal level of decomposition
    set.maxlevel <- function(filter,maxlevel){
      # get Wavelet Transform Filter
      filter <- wavelets::wt.filter(filter, modwt=TRUE)
      # get the length of the wavelet and scaling filters
      L <- filter@L

      # determine the maximal level of decomposition
      if(missing(maxlevel) | is.null(maxlevel)){
        maxlevel <- as.integer(floor(log(((nobs-1)/(L-1))+1)/log(2)))
      }

      return(maxlevel)
    }

    #decompose the time series by maximal overlap discrete wavelet transform
    optim.wt <- function(timeseries,filter,n.levels){
      wdecomp <- wavelets::modwt(timeseries, filter=filter, n.levels=n.levels,boundary=boundary)

      return(wdecomp)
    }

    #transform time series and optimize Models given a set of initial parameters
    optim.models <- function(timeseries,filter,maxlevel,modelType,...){
      #decompose the time series by maximal overlap discrete wavelet transform
      wdecomp <- optim.wt(timeseries,filter,maxlevel)

      #model
      Wmodels <- Vmodels <- list()
      for(level in 1:maxlevel){
        if(modelType=="ets") {
          Wmodels[[level]] <- forecast::ets(ts(wdecomp@W[[level]]),...)
          Vmodels[[level]] <- forecast::ets(ts(wdecomp@V[[level]]),...)
        }
        else if(modelType=="arima"){
          Wmodels[[level]] <- forecast::auto.arima(wdecomp@W[[level]],...)
          Vmodels[[level]] <- forecast::auto.arima(wdecomp@V[[level]],...)
        }
      }

      return(list(Wmodels=Wmodels,Vmodels=Vmodels))
    }

    #computes quality measures acoording to rank.by
    fitness.criteria <- function(models,level){
      #Vi is the main component of the WT of level i acoording to [1]
      Vimodel <- models$Vmodels[[level]]
      #computes quality measures acoording to rank.by
      AIC <- Vimodel$aic
      BIC <- Vimodel$bic
      AICc <- Vimodel$aicc
      ll <- Vimodel$loglik

      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }

    #computes predictions for each decomposed time series
    decomp.pred <- function(models,maxlevel,n.ahead,confLevel){
      prediction <- list()
      for(level in 1:maxlevel){
        fcW <- forecast::forecast(models$Wmodels[[level]], h=n.ahead, level=confLevel)
        fcV <- forecast::forecast(models$Vmodels[[level]], h=n.ahead, level=confLevel)
        prediction[[level]] <- list(W=list(pred=fcW$mean,lower=fcW$lower,upper=fcW$upper),
                                    V=list(pred=fcV$mean,lower=fcV$lower,upper=fcV$upper))
      }

      return(prediction)
    }

    #inverse transform the time series, computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(wdecomp,decomp.level,prediction,n.ahead,i.n.ahead,ts.test,ts){
      #edits modwt object: adds n.ahead NAs to the end of original time series values
      newseries <- c(wdecomp@series,rep(NA,n.ahead))
      wdecomp@series <- as.matrix(newseries)
      wdecomp@attr.X <- attributes(ts(newseries))
      #saves state of modwt object until this point
      wdecompPred <- wdecomp

      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$pred))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$pred))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets prediction time series
      pred <- ts(utils::tail(iwdecomp,n.ahead),start=i.n.ahead)

      #Obtain lower and upper predictions analogously
      #retrieves previous state of modwt object
      wdecomp <- wdecompPred
      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$lower))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$lower))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets lower prediction time series
      lower <- ts(utils::tail(iwdecomp,n.ahead),start=i.n.ahead)

      #retrieves previous state of modwt object
      wdecomp <- wdecompPred
      #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
      for(level in 1:decomp.level){
        wdecomp@W[[level]] <- as.matrix(c(wdecomp@W[[level]],prediction[[level]]$W$upper))
        wdecomp@V[[level]] <- as.matrix(c(wdecomp@V[[level]],prediction[[level]]$V$upper))
      }
      #inverse maximal overlap discrete wavelet transform
      iwdecomp <- wavelets::imodwt(wdecomp)
      #gets upper prediction time series
      upper <- ts(utils::tail(iwdecomp,n.ahead),start=i.n.ahead)


      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred)
        NMSE <- TSPred::NMSE(ts.test, pred, ts)
        MAPE <- TSPred::MAPE(ts.test, pred)
        sMAPE <- TSPred::sMAPE(ts.test, pred)
        MaxError <- TSPred::MAXError(ts.test, pred)

        return(list(pred=list(mean=pred,lower=lower,upper=upper),errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }

      return(list(pred=list(mean=pred,lower=lower,upper=upper)))
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
      attr(rank,"ranked.wt") <- models

      return(rank)
    }

    #if parameter is not provided, the function finds the best option among {...}
    rank <- NULL
    if(length(filters)==1 & !is.null(n.levels)){
      level.optim <- n.levels
      filter.optim <- filters[1]
    }
    else{
      # creates the Validation series for parameter optimization
      ts.val <- utils::tail(ts,n.ahead)
      ts.tmp <- utils::head(ts,nobs-n.ahead)

      #if rank.by considers fitness measures, parameter optimization uses only and all the training series
      if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)) ts.tmp <- ts

      #level options to be tested is equal to c(n.levels) if it is not null
      if(!is.null(n.levels)){
        #set maximal level of decomposition to be tested
        maxlevel <- n.levels
        #initial options of levels
        levels.opt <- n.levels
      }

      for(filter in filters){
        #set level options to be tested
        if(is.null(n.levels)){
          #set maximal level of decomposition to be tested
          maxlevel <- set.maxlevel(filter,maxlevel)
          #initial options of levels
          levels.opt <- c(1:maxlevel)
        }

        #decompose time series up to maxlevel and optimize Models of each decomposed series given a set of initial parameters
        models <- tryCatch( optim.models(ts.tmp,filter,maxlevel,modelType,...) , error = function(e) e )
        if(inherits(models, "error")) next

        #computes predictions for each decomposed time series if rank.by includes error measures
        if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)) prediction <- decomp.pred(models,maxlevel,n.ahead,conf.level)

        #produces candidate models and measures in order to select "best" parameters
        wts <- list()
        for(level in levels.opt){
          #generates and optimizes candidate Model based on initial parameter values
          wt <- optim.wt(ts.tmp,filter,level)

          #creates candidate model id and saves it in the list models
          ModelId <- paste(paste("Filter:",filter),paste("Level:",level),sep="_")
          wts[[ModelId]] <- wt

          if(any(c("AIC","AICc","BIC","logLik") %in% rank.by)){
            #computes fitness measures and returns a dataframe with them
            rank.measures <- fitness.criteria(models,level)
          }
          else if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
            #computes predictions and prediction error measures
            rank.measures <- pred.criteria(wt,level,prediction,n.ahead,length(ts.tmp)+1,ts.val,ts.tmp)$errors
          }

          #combine results of the candidate models in the dataframe rank
          rank <- rbind(rank, data.frame(ModelId=ModelId,filter=filter,level=level,rank.measures))
        }
      }

      #ranking the candidate models based on all measures referenced by rank.by
      #also candidate models (objects) are ranked and included as attribute of rank dataframe
      rank <- ranking.models(rank,rank.by,wts)

      #if rank.by is fitness
      level.optim <- rank[1,]$level
      filter.optim <- as.character(rank[1,]$filter)
    }




    #decompose time series up to maxlevel and optimize Models of each decomposed series given a set of initial parameters
    models <- optim.models(ts,filter.optim,level.optim,modelType,...)

    #computes predictions for each decomposed time series if rank.by includes error measures
    prediction <- decomp.pred(models,level.optim,n.ahead,conf.level)

    #generates and optimizes candidate Model based on initial parameter values
    wt <- optim.wt(ts,filter.optim,level.optim)

    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(models,level.optim)
    fit.measures <- lapply(fit.measures,identity) #transforms to list

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(wt,level.optim,prediction,n.ahead,nobs+1,ts.test,ts)

    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)

    #append results in a list
    results <- c( list(WT=wt), level=level.optim, filter=filter.optim, fit.measures, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)

    return(results)
  }
