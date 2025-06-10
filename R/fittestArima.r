#' Automatic ARIMA fitting, prediction and accuracy evaluation
#'
#' The function predicts and returns the next n consecutive values of a
#' univariate time series using an automatically best fitted ARIMA model. It
#' also evaluates the fitness of the produced model, using AICc, AIC, BIC and
#' logLik criteria, and its prediction accuracy, using the MSE, NMSE, MAPE,
#' sMAPE and maximal error accuracy measures.
#'
#' The ARIMA model is automatically fitted by the
#' \code{\link[forecast]{auto.arima}} function and it is used for prediction by
#' the \code{\link[forecast]{forecast}} function both in the \code{forecast}
#' package.
#'
#' The fitness criteria AICc, AIC (\code{\link{AIC}}), BIC (\code{\link{BIC}})
#' and log-likelihood (\code{\link{logLik}}) are extracted from the fitted
#' ARIMA model. Also, the prediction accuracy of the model is computed by means
#' of MSE (\code{\link{MSE}}), NMSE (\code{\link{NMSE}}), MAPE
#' (\code{\link{MAPE}}), sMAPE (\code{\link{sMAPE}}) and maximal error
#' (\code{\link{MAXError}}) measures.
#'
#' @param timeseries A vector or univariate time series which contains the
#' values used for fitting an ARIMA model.
#' @param timeseries.test A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. It is used as a
#' testing set and base for calculation of prediction error measures. Ignored
#' if \code{NULL}.
#' @param h Number of consecutive values of the time series to be predicted. If
#' \code{h} is \code{NULL}, the number of consecutive values to be predicted is
#' assumed to be equal to the length of \code{timeseries.test}. Required when
#' \code{timeseries.test} is \code{NULL}.
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.test}. The default function is
#' \code{\link[stats]{na.omit}}, which omits any missing values found in
#' \code{timeseries} or \code{timeseries.test}.
#' @param level Confidence level for prediction intervals.
#' @param ... Additional arguments passed to the
#' \code{\link[forecast]{auto.arima}} modelling function.
#' @return A list with components: \item{model}{A list of class "ARIMA"
#' containing the best fitted ARIMA model. See the \code{\link[forecast]{auto.arima}}
#' function in the \code{forecast} package.} \item{parameters}{A list
#' containing the parameters of the best fitted ARIMA model. See the
#' \code{\link{arimaparameters}} function.} \item{AICc}{Numeric value of the
#' computed AICc criterion of the fitted model.} \item{AIC}{Numeric value of
#' the computed AIC criterion of the fitted model.} \item{BIC}{Numeric value of
#' the computed BIC criterion of the fitted model.} \item{logLik}{Numeric value
#' of the computed log-likelihood of the fitted model.} \item{pred}{A list with
#' the components \code{mean}, \code{lower} and \code{upper}, containing the
#' predictions and the lower and upper limits for prediction intervals,
#' respectively. All components are time series. See the \code{\link[forecast]{forecast}}
#' function in the \code{forecast} package.} \item{MSE}{Numeric value of the
#' resulting MSE error of prediction.} \item{NMSE}{Numeric value of the
#' resulting NMSE error of prediction.} \item{MAPE}{Numeric value of the
#' resulting MAPE error of prediction.} \item{sMAPE}{Numeric value of the
#' resulting sMAPE error of prediction.} \item{MaxError}{Numeric value of the
#' maximal error of prediction.}
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{fittestArimaKF}}, \code{\link{fittestLM}},
#' \code{\link{marimapred}}
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#'
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords ARIMA automatic fitting adjustment prediction evaluation criterion
#' errors
#' @examples
#'
#' \donttest{
#' data(CATS,CATS.cont)
#' fArima <- fittestArima(CATS[,1],CATS.cont[,1])
#' #predicted values
#' pred <- fArima$pred$mean
#' #model information
#' cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC,
#'  logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE,
#'  MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)
#'
#' #plotting the time series data
#' plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),
#'  xlab="Time",ylab="ARIMA")
#' #plotting the predicted values
#' lines(ts(pred,start=981),lwd=2,col='blue')
#' #plotting prediction intervals
#' lines(ts(fArima$pred$upper[,2],start=981),lwd=2,col='light blue')
#' lines(ts(fArima$pred$lower[,2],start=981),lwd=2,col='light blue')
#' }
#'
#' @export fittestArima
fittestArima <-
  function(timeseries, timeseries.test=NULL, h=NULL, na.action=stats::na.omit, level=c(80,95), ...){

    #catch parameter errors
    if(is.null(timeseries))    stop("timeseries is required and must have positive length")
    if(is.null(timeseries.test) & is.null(h)) stop("the number of values to be predicted is unknown, provide either timeseries.test or h")

    #require("forecast")

    #prepare the training time series
    ts <- ts(na.action(timeseries))
    nobs <- length(ts)
    i.n.ahead <- nobs+1
    #prepare the test time series (if present) and set the prediction horizon
    n.ahead <- ts.test <- NULL
    if(!is.null(timeseries.test)) {
      ts.test <- ts(na.action(timeseries.test),start=i.n.ahead)
      n.ahead <- length(ts.test)
      if(!is.null(h)){
        if(h < n.ahead){
          ts.test <- utils::head(ts.test,h)
          n.ahead <- h
        }
      }
    }
    else n.ahead <- h

    #optimize Model given a set of initial parameters
    optim.model <- function(timeseries,...){
      model <- forecast::auto.arima(timeseries,...)
      return(model)
    }

    #computes quality measures
    fitness.criteria <- function(model){
      AIC <- model$aic
      BIC <- model$bic
      AICc <- model$aicc
      ll <- model$loglik

      return(data.frame(AICc=AICc,AIC=AIC,BIC=BIC,logLik=ll))
    }

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.criteria <- function(model,n.ahead,level,i.n.ahead,ts.test,ts){
      #computes predictions using the candidate model
      pred <- forecast::forecast(model, h=n.ahead, level=level)
      pred <- list(mean=pred$mean,lower=pred$lower,upper=pred$upper)
      pred.mean <- ts(pred$mean,start=i.n.ahead)

      #computes prediction error measures if ts.test is provided
      if(!is.null(ts.test)) {
        MSE <- TSPred::MSE(ts.test, pred.mean)
        NMSE <- TSPred::NMSE(ts.test, pred.mean, ts)
        MAPE <- TSPred::MAPE(ts.test, pred.mean)
        sMAPE <- TSPred::sMAPE(ts.test, pred.mean)
        MaxError <- TSPred::MAXError(ts.test, pred.mean)

        return(list(pred=pred,errors=data.frame(MSE=MSE,NMSE=NMSE,MAPE=MAPE,sMAPE=sMAPE,MaxError=MaxError)))
      }

      return(list(pred=pred))
    }

    #generates and optimizes Model based on optim parameter values
    model <- optim.model(ts, ...)

    modelpar <- TSPred::arimaparameters(model)

    #computes fitness measures and returns a dataframe with them
    fit.measures <- fitness.criteria(model)
    fit.measures <- lapply(fit.measures,identity) #transforms to list

    #computes predictions, and prediction error measures (if timeseries.test is provided)
    pred.measures <- pred.criteria(model,n.ahead,level,i.n.ahead,ts.test,ts)

    #predictions
    prediction <- pred.measures$pred
    #error measures into list
    errors.measures <- switch(is.null(pred.measures$errors)+1,lapply(pred.measures$errors,identity),NULL)

    #append results in a list
    results <- c( list(model=model), list(parameters=modelpar), fit.measures, list(pred=prediction), errors.measures )

    return(results)
  }
