#Finds and returns the fittest EMD #DO!
#References:
#[1] A Hilbert-Huang transform approach for predicting cyber-attacks, Kim et al.

#' Automatic prediction with empirical mode decomposition
#'
#' The function automatically applies an empirical mode decomposition to a
#' provided univariate time series. The resulting components of the decomposed
#' series are used as base for predicting and returning the next n consecutive
#' values of the provided univariate time series using also automatically
#' fitted models. It also evaluates fitness and
#' prediction accuracy of the produced models.
#'
#' The function produces an empirical mode decomposition of \code{timeseries}.
#' See the \code{\link[Rlibeemd]{emd}} function. The Intrinsic Mode Functions (IMFs) and residue series
#' resulting from the decomposition are separately used as base for model
#' fitting and prediction.
#' The set of predictions for all IMFs and residue series are then reversed
#' transformed in order to produce the next \code{h} consecutive values of the
#' provided univariate time series in \code{timeseries}. See the
#' \code{\link{emd.rev}} function.
#'
#' The function automatically selects the meaningful IMFs of
#' a decomposition. For that, the function produces
#' models for different selections of meaningful IMFs according to the
#' possible intervals \code{i:num_imfs} for \code{i=1,...,(num_imfs-1)}, where
#' \code{num_imfs} is the number of IMFs in a decomposition. The options of
#' meaningful IMFs of a decomposition which generate
#' the best ranked model fitness/predictions acoording to the criteria in
#' \code{rank.by} are selected.
#'
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link[MuMIn]{AICc}}, \code{\link{BIC}} or
#' \code{\link{logLik}}). In the former case, the candidate empirical mode
#' decompositions are used for time series prediction and the error measures
#' are calculated by means of a cross-validation process. In the latter case,
#' the component series of the candidate decompositions are modeled and model
#' fitness criteria are calculated based on all observations in
#' \code{timeseries}. In particular, the fitness criteria calculated for
#' ranking the candidate decompositions correspond to the
#' models produced for the IMFs.
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
#' @param num_imfs Number of Intrinsic Mode Functions (IMFs) to compute. See \code{\link[Rlibeemd]{emd}}.
#' @param S_number,num_siftings See \code{\link[Rlibeemd]{emd}}.
#' @param level Confidence level for prediction intervals. See
#' \code{\link[stats]{predict.lm}} and \code{\link[vars]{predict}}.
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}.
#' @param model Character string. Indicates which model is to be used for
#' fitting and prediction of the components of the decomposed series.
#' @param rank.by Character string. Criteria used for ranking candidate
#' decompositions/models/predictions generated during parameter selection. See
#' 'Details'.
#'
#' @return A list with components: \item{emd}{Same as \code{\link[Rlibeemd]{emd}}.
#' Contains the empirical mode decomposition of \code{timeseries}.}
#' \item{meaningfulImfs}{Character string indicating the automatically selected
#' meaningful IMFs of the decomposition.}
#' \item{pred}{A list with the
#' components \code{mean}, \code{lower} and \code{upper}, containing the
#' predictions based on the best evaluated decomposition and the lower and
#' upper limits for prediction intervals, respectively. All components are time
#' series.} \item{MSE}{Numeric value of the resulting MSE error of prediction.
#' Require \code{timeseries.test}.} \item{NMSE}{Numeric value of the resulting
#' NMSE error of prediction. Require \code{timeseries.test}.}
#' \item{MAPE}{Numeric value of the resulting MAPE error of prediction. Require
#' \code{timeseries.test}.} \item{sMAPE}{Numeric value of the resulting sMAPE
#' error of prediction. Require \code{timeseries.test}.}
#' \item{MaxError}{Numeric value of the maximal error of prediction. Require
#' \code{timeseries.test}.} \item{rank.val}{Data.frame with the fitness or
#' prediction accuracy criteria computed based on all candidate decompositions
#' ranked by \code{rank.by}.} \item{rank.by}{Ranking
#' criteria used for ranking candidate decompositions and producing
#' \code{rank.val}.}
#'
#' @author Rebecca Pontes Salles
#'
#' @seealso \code{\link{fittestWavelet}}, \code{\link{fittestMAS}}
#'
#' @references Kim, D., Paek, S. H., & Oh, H. S. (2008). A Hilbert-Huang
#' transform approach for predicting cyber-attacks. Journal of the Korean
#' Statistical Society, 37(3), 277-283.
#'
#' @keywords emd decomposition transform automatic fitting adjustment
#' prediction evaluation criterion errors time series
#'
#' @examples
#'
#' data(CATS)
#' \donttest{
#' femd <- fittestEMD(CATS[,1],h=20)
#' }
#'
#' @export fittestEMD
fittestEMD <-
  function(timeseries, timeseries.test=NULL, h=NULL, num_imfs=0, S_number=4L, num_siftings=50L, level=0.95, na.action=stats::na.omit,
           model=c("ets","arima"),rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","errors")){
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
    if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")

    #decompose time series
    optim.decomp <- function(timeseries,num_imfs,S_number,num_siftings){
      #performs empirical mode decomposition of the time series
      emdt <- Rlibeemd::emd(timeseries, num_imfs=num_imfs, S_number=S_number, num_siftings=num_siftings)
      return(emdt)
    }

    #optimize Models given a set of initial parameters
    optim.models <- function(timeseries,emdt,modelType,...){
      #model
      models <- list()
      for(level in 1:ncol(emdt)){
        if(modelType=="ets") {
          models[[level]] <- forecast::ets(ts(emdt[,level]),...)
        }
        else if(modelType=="arima"){
          models[[level]] <- forecast::auto.arima(emdt[,level],...)
        }
      }

      return(models)
    }

    #computes predictions for each decomposed time series
    decomp.pred <- function(models,maxlevel,n.ahead,level){
      prediction <- list()
      for(level in 1:maxlevel){
        fc <- forecast::forecast(models[[level]], h=n.ahead, level=level)
        prediction[[level]] <- list(pred=fc$mean,lower=fc$lower,upper=fc$upper)
      }

      return(prediction)
    }

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(prediction,n.ahead,level,ts.test,ts){
      #computes predictions using the candidate VARmodel
      pred <- lapply(prediction,function(c) c$pred)
      lower <- lapply(prediction,function(c) c$lower)
      upper <- lapply(prediction,function(c) c$upper)

      #inverse EMD transform: generates the prediction for the original time series
      pred <- emd.rev(pred)
      lower <- emd.rev(lower)
      upper <- emd.rev(upper)

      pred.mean <- pred

      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)

        return(list(pred=list(mean=pred,lower=lower,upper=upper),errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }

      return(list(pred=list(mean=pred,lower=lower,upper=upper)))
    }

    #ranks candidate models
    ranking.models <- function(rank,rank.by){
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

      return(rank)
    }

    #finds the best set of parameters: meaningful IMFs
    rank <- NULL

    # creates the Validation series for parameter optimization
    ts.val <- utils::tail(ts,n.ahead)
    ts.tmp <- utils::head(ts,nobs-n.ahead)

    #decompose time series
    emdt <- optim.decomp(ts.tmp,num_imfs,S_number,num_siftings)
    nimf <- ncol(emdt)

    models <- optim.models(ts.tmp,emdt,modelType)

    #prediction of residue series
    if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)) predictions <- decomp.pred(models,nimf,n.ahead,level)

    #initial options of meaningfulImfs: i:nimf for i=1,...,(nimf-1)
    firstMeaningfulImf.opt <- c(1:(nimf-1))

    #produces candidate models and measures in order to select "best" parameters
    models <- list()
    for(firstMeaningfulImf in firstMeaningfulImf.opt){
      #creates candidate model id and saves it in the list models
      str.meaningfulImfs <- paste(firstMeaningfulImf,nimf,sep="-")
      ModelId <- paste("Imfs:",str.meaningfulImfs)

      preds <- predictions[firstMeaningfulImf:nimf]

      if(any(c("MSE","NMSE","MAPE","sMAPE","MaxError") %in% rank.by)){
        #computes predictions and prediction error measures
        rank.measures <- pred.criteria(preds,n.ahead,level,ts.val,ts.tmp)$errors
      }

      #combine results of the candidate models in the dataframe rank
      rank <- rbind(rank, data.frame(ModelId=ModelId,firstMeaningfulImf=firstMeaningfulImf,rank.measures))
    }

    #ranking the candidate models based on all measures referenced by rank.by
    #also candidate models (objects) are ranked and included as attribute of rank dataframe
    rank <- ranking.models(rank,rank.by)

    #if rank.by is fitness
    firstMeaningfulImf.optim <- rank[1,]$firstMeaningfulImf



    #decompose time series
    emdt <- optim.decomp(ts,num_imfs,S_number,num_siftings)
    nimf <- ncol(emdt)

    models <- optim.models(ts,emdt,modelType)

    predictions <- decomp.pred(models,nimf,n.ahead,level)

    #generates and optimizes Model based on optim parameter values
    meaningfulImfs <- c(firstMeaningfulImf.optim:nimf)
    str.meaningfulImfs <- paste(firstMeaningfulImf.optim,nimf,sep="-")

    preds <- predictions[meaningfulImfs]

    pred.measures <- pred.criteria(preds,n.ahead,level,ts.test,ts)

    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)

    #append results in a list
    results <- c( list(emd=emdt), meaningfulImfs=str.meaningfulImfs, list(pred=prediction), errors.measures )
    if(!is.null(rank) ) results <- c(results, list(rank.val=rank), rank.by=rank.by)

    return(results)
  }
