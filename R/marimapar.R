#' Get parameters of multiple ARIMA models.
#' 
#' The function returns the parameters of a set of automatically fitted ARIMA
#' models, including non-seasonal and seasonal orders and drift. Based on
#' multiple application of the \code{arimapar} function.
#' 
#' See the \code{arimapar} function.
#' 
#' @param timeseries A vector, matrix, or data frame which contains a set of
#' time series used for fitting ARIMA models. Each column corresponds to one
#' time series.
#' @param na.action A function for treating missing values in
#' \code{timeseries}. The default function is \code{\link[stats]{na.omit}}, which
#' omits any missing values found in \code{timeseries}.
#' @param xreg A vector, matrix, data frame or times series of external
#' regressors used for fitting all the ARIMA models.  It must have the same
#' number of rows as \code{TimeSeries}. Ignored if \code{NULL}.
#' @return A list of numeric vectors, each one giving the number of AR, MA,
#' seasonal AR and seasonal MA coefficients, plus the period and the number of
#' non-seasonal and seasonal differences of the automatically fitted ARIMA
#' models. It is also presented the value of the fitted drift constants.
#' @seealso \code{arimapar}, \code{\link{arimapred}}, \code{\link{marimapred}}
#' @references See the \code{arimapar} function.
#' @keywords ARIMA automatic fitting adjustment parameters
#' 
#' @export marimapar
marimapar <-
function(timeseries, na.action=stats::na.omit, xreg=NULL){
	.Deprecated("arimaparameters","TSPred")
  #return (lapply(timeseries,arimapar,na.action=na.action, xreg=xreg))
}
