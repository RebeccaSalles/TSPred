#' Automatically finding fittest linear model for prediction
#'
#' The function automatically evaluates and returns the fittest linear model
#' among ARIMA and polynomial regression, with and without Kalman filtering,
#' for prediction of a given univariate time series. Wrapper for the
#' \code{\link{fittestArima}}, \code{\link{fittestArimaKF}},
#' \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}} functions for
#' automatic time series prediction, whose results are also returned.
#'
#' The results of the best evaluated models returned by
#' \code{\link{fittestArima}}, \code{\link{fittestArimaKF}},
#' \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}} are ranked and
#' the fittest linear model for prediction of the given univariate time series
#' is selected based on the criteria in \code{rank.by}.
#'
#' The ranking criteria in \code{rank.by} may be set as a prediction error
#' measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}},
#' \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria
#' (such as \code{\link{AIC}}, \code{\link[MuMIn]{AICc}}, \code{\link{BIC}} or
#' \code{\link{logLik}}). See \code{\link{fittestArima}},
#' \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} or
#' \code{\link{fittestPolyRKF}}.
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
#' @param timeseries A vector or univariate time series which contains the
#' values used for fitting the models.
#' @param timeseries.test A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. It is used as a
#' testing set and base for calculation of prediction error measures. Ignored
#' if \code{NULL}.
#' @param h Number of consecutive values of the time series to be predicted. If
#' \code{h} is \code{NULL}, the number of consecutive values to be predicted is
#' assumed to be equal to the length of \code{timeseries.test}. Required when
#' \code{timeseries.test} is \code{NULL}.
#' @param level Confidence level for prediction intervals.
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.test}.
#' @param filtered See \code{\link{fittestArimaKF}} and
#' \code{\link{fittestPolyRKF}}.
#' @param initQ See \code{\link{fittestArimaKF}} and
#' \code{\link{fittestPolyRKF}}.
#' @param order See \code{\link{fittestPolyR}} and
#' \code{\link{fittestPolyRKF}}.
#' @param minorder See \code{\link{fittestPolyR}} and
#' \code{\link{fittestPolyRKF}}.
#' @param maxorder See \code{\link{fittestPolyR}} and
#' \code{\link{fittestPolyRKF}}.
#' @param raw See \code{\link{fittestPolyR}}.
#' @param ... See \code{\link{fittestArima}} and \code{\link{fittestArimaKF}}.
#' @param rank.by Character string. Criteria used for ranking candidate models.
#' See 'Details'.
#' @return A list with components: \item{model}{An object containing the
#' fittest evaluated linear model. The class of the model object is dependent
#' on the results of the evaluation (ranking). See \code{\link{fittestArima}},
#' \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and
#' \code{\link{fittestPolyRKF}}.} \item{rank}{Data.frame with the fitness
#' and/or prediction accuracy criteria computed for all models considered,
#' ranked by \code{rank.by}.} \item{ranked.results}{A list of lists containing
#' the ranked results of the functions \code{\link{fittestArima}},
#' \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and
#' \code{\link{fittestPolyRKF}}. Also ranked by \code{rank.by}.}
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{fittestArima}}, \code{\link{fittestArimaKF}},
#' \code{\link{fittestPolyR}}, \code{\link{fittestPolyRKF}}
#' @keywords fittest linear model ARIMA polynomial regression automatic fitting
#' Kalman filter adjustment prediction evaluation criterion errors
#' @examples
#'
#' \donttest{
#' data(CATS,CATS.cont)
#' fittest <- fittestLM(CATS[,1],CATS.cont[,1])
#'
#' #fittest model information
#' fittest$rank[1,]
#'
#' #predictions of the fittest model
#' fittest$ranked.results[[1]]$pred
#' }
#'
#' @export fittestLM
fittestLM <-
function(timeseries, timeseries.test=NULL, h=NULL, level=0.95, na.action=stats::na.omit, filtered=TRUE,
         order=NULL, minorder=0, maxorder=5, raw = FALSE,initQ=NULL,
         rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...){
  #catch parameter errors
  if(is.null(timeseries))    stop("timeseries is required and must have positive length")
  if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")

  oa <- fittestArima(timeseries, timeseries.test, h, level=level, na.action=na.action, ...)
  oaKF <- fittestArimaKF(timeseries, timeseries.test, h, level=level,initQ=initQ, na.action=na.action, filtered=filtered, rank.by=rank.by,...)
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

  # evaluate choices of rank.by
  rank.by <- match.arg(rank.by)
  if(rank.by == "fitness") rank.by <- c("AIC","AICc","BIC","logLik")
  else if(rank.by == "errors") rank.by <- c("MSE","NMSE","MAPE","sMAPE","MaxError")

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
