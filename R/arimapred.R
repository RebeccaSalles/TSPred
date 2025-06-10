#' Automatic ARIMA fitting and prediction
#'
#' The function predicts and returns the next n consecutive values of a time
#' series using an automatically fitted ARIMA model. It may also plot the
#' predicted values against the actual ones using the function
#' \code{\link{plotarimapred}}.
#'
#' The ARIMA model used for time series prediction is automatically fitted by
#' the \code{\link[forecast]{auto.arima}} function in the \code{forecast} package. In
#' order to avoid drift errors, the function introduces an auxiliary regressor
#' whose values are a sequence of consecutive integer numbers starting from 1.
#' The fitted ARIMA model is used for prediction by the
#' \code{\link{predict.Arima}} function in the \code{stats} package. For more
#' details, see the \code{\link[forecast]{auto.arima}} function in the \code{forecast}
#' package and the \code{\link{predict.Arima}} function in the stats package.
#'
#' @param timeseries A vector or univariate time series which contains the
#' values used for fitting an ARIMA model.
#' @param timeseries.cont A vector or univariate time series containing a
#' continuation for \code{timeseries} with actual values. Ignored if
#' \code{NULL}.
#' @param n.ahead Number of consecutive values of the time series, which are to
#' be predicted. If \code{n.ahead} is \code{NULL}, the number of consecutive
#' values to be predicted is assumed to be equal to the length of
#' \code{timeseries.cont}. Required when \code{timeseries.cont} is \code{NULL}.
#' @param na.action A function for treating missing values in \code{timeseries}
#' and \code{timeseries.cont}. The default function is \code{\link{na.omit}},
#' which omits any missing values found in \code{timeseries} or
#' \code{timeseries.cont}.
#' @param xreg A vector, matrix, data frame or times series of external
#' regressors used for fitting the ARIMA model.  It must have the same number
#' of rows as \code{timeseries}. Ignored if \code{NULL}.
#' @param newxreg A vector, matrix, data frame or times series with new values
#' of \code{xreg} to be used for prediction. Must have at least \code{n.ahead}
#' rows or the number of rows in \code{timeseries.cont}. Ignored if
#' \code{NULL}.
#' @param se.fit If \code{se.fit} is \code{TRUE}, the standard errors of the
#' predictions are returned.
#' @param plot If \code{plot} is \code{TRUE}, the function will generate a
#' graphic of the predicted values against the actual ones in
#' \code{timeseries.cont}.
#' @param range.p A percentage which defines how much the range of the
#' graphic's y-axis will be increased from the minimum limits imposed by data.
#' @param ylab A title for the graphic's y-axis. Ignored if \code{NULL}.
#' @param xlab A title for the graphic's x-axis. Ignored if \code{NULL}.
#' @param main An overall title for the graphic. Ignored if \code{NULL}.
#' @return A time series of predictions, or if \code{se.fit} is \code{TRUE}, a
#' list with the components \code{pred}, the predictions, and \code{se}, the
#' estimated standard errors. Both components are time series. See the
#' \code{\link{predict.Arima}} function in the stats package.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link[forecast]{auto.arima}}, \code{\link{predict.Arima}},
#' \code{\link{plotarimapred}}, \code{\link{marimapred}}
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#'
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords ARIMA automatic fitting adjustment prediction
#' @examples
#'
#' data(SantaFe.A,SantaFe.A.cont)
#' arimapred(SantaFe.A[,1],SantaFe.A.cont[,1])
#' arimapred(SantaFe.A[,1],n.ahead=100)
#'
#' @export arimapred
arimapred <-
function(timeseries,timeseries.cont=NULL, n.ahead=NULL, na.action=stats::na.omit, xreg=NULL, newxreg=NULL, se.fit=FALSE, plot=FALSE, range.p=0.2,ylab=NULL,xlab=NULL,main=NULL){
  if(is.null(timeseries))    stop("timeseries is required")
  if(is.null(timeseries.cont) & is.null(n.ahead)) stop("the number of values to be predicted is unknown")

  ts <- ts(na.action(timeseries))

  N <- NULL
  if(!is.null(timeseries.cont)) N <- length(na.action(timeseries.cont))
  if(!is.null(n.ahead)) N <- n.ahead

  nobs <- length(ts)
  i <- nobs + 1
  f <- nobs +  N

  reg <- cbind(1:nobs,xreg)

  fit <- forecast::auto.arima(ts,xreg=ts(reg,start=1))

  newreg <- cbind(i:f,newxreg)

  pred <- stats::predict(fit,n.ahead=N, newxreg=ts(newreg,start=i),se.fit=se.fit)

  if(!is.null(timeseries.cont) & plot)
  {
    ts.cont <- ts(na.action(timeseries.cont),start=i)
    plotarimapred(ts.cont, fit, xlim=c(i,f), range.p, xreg=newreg, ylab=ylab, xlab=xlab, main=main)
  }

  return (pred)
}
