#' Multiple time series automatic ARIMA fitting and prediction 
#' 
#' The function predicts and returns the next n consecutive values of a set of
#' time series using automatically fitted ARIMA models. Based on multiple
#' application of the \code{\link{arimapred}} function. 
#' 
#' See the \code{\link{arimapred}} function. 
#' 
#' @param TimeSeries A vector, matrix, or data frame which contains a set of
#' time series used for fitting ARIMA models. Each column corresponds to one
#' time series. 
#' @param TimeSeriesCont A vector, matrix, or data frame containing
#' continuation points for \code{TimeSeries} with actual values. Each column
#' corresponds to one time series. Ignored if \code{NULL}. 
#' @param n.ahead A numeric vector (or a single numeric value) with the number
#' of consecutive values which are to be predicted of each respective time
#' series in \code{TimeSeries}. If \code{n.ahead} is \code{NULL}, the number of
#' values to be predicted of each time series in \code{TimeSeries} is assumed
#' to be equal to the number of rows in each respective time series in
#' \code{TimeSeriesCont}. Required when \code{TimeSeriesCont} is \code{NULL}.
#' 
#' @param na.action A function for treating missing values in \code{TimeSeries}
#' and \code{TimeSeriesCont}. The default function is \code{\link[stats]{na.omit}},
#' which omits any missing values found in \code{TimeSeries} or
#' \code{TimeSeriesCont}. 
#' @param xreg A list of vectors, matrices, data frames or times series of
#' external regressors used for fitting the ARIMA models. The first component
#' of the list contains external regressors for the first time series in
#' \code{TimeSeries} and therefore must have the same number of rows as this
#' respective time series. This is also valid for the second component, and so
#' forth. Ignored if \code{NULL}. 
#' @param newxreg A list of vectors, matrices, data frames or times series with
#' new values of \code{xreg} to be used for prediction. The first component of
#' the list must have at least the same number of rows as the respective first
#' value in \code{n.ahead} or, if \code{n.ahead} is \code{NULL}, the number of
#' continuation points in the respective first time series in
#' \code{TimeSeriesCont}. This is also valid for the second component, and so
#' forth. Ignored if \code{NULL}. 
#' @param se.fit If \code{se.fit} is \code{TRUE}, the standard errors of the
#' predictions are returned. 
#' @param plot A Boolean parameter which defines whether the function
#' \code{\link{arimapred}} will generate a graphic. If \code{plot} is
#' \code{TRUE}, graphics will be generated for each time series in
#' \code{TimeSeries}. 
#' @param range.p A percentage which defines how much the range of the
#' graphics' y-axis will be increased from the minimum limits imposed by data.
#' 
#' @param ylab A title for the graphics' y-axis. Ignored if \code{NULL}. %%
#' ~~Describe \code{ylab} here~~
#' @param xlab A title for the graphics' x-axis. Ignored if \code{NULL}. %%
#' ~~Describe \code{xlab} here~~
#' @param main An overall title for the graphics. Ignored if \code{NULL}. %%
#' ~~Describe \code{main} here~~
#' @return A vector of time series of predictions, if the number of consecutive
#' values predicted of each time series in \code{TimeSeries} is the same,
#' otherwise a list of time series of predictions.
#' 
#' If \code{se.fit} is \code{TRUE}, a vector of lists, each one with the
#' components \code{pred}, the predictions, and \code{se}, the estimated
#' standard errors. Both components are time series. See the
#' \code{\link{predict.Arima}} function in the stats package and the function
#' \code{\link{arimapred}}.
#'
#' @author Rebecca Pontes Salles 
#' @seealso \code{\link{arimapred}} ~
#' @references See the \code{\link{arimapred}} function. %% ~put references to
#' the literature/web site here ~
#' @keywords ARIMA automatic fitting adjustment prediction
#' @examples
#' 
#' data(SantaFe.A,SantaFe.A.cont)
#' marimapred(SantaFe.A,SantaFe.A.cont)
#' 
#' @export marimapred
marimapred <-
function(TimeSeries,TimeSeriesCont=NULL,n.ahead=NULL,na.action=stats::na.omit,xreg=NULL,newxreg=NULL,se.fit=FALSE,plot=FALSE,range.p=0.2,ylab=NULL,xlab=NULL,main=NULL){
    if(!is.null(TimeSeriesCont)) {
		if(!is.null(n.ahead)) {
			if(!is.null(xreg) & !is.null(newxreg)) {
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, MoreArgs = list(se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, ylab=colnames(TimeSeries), n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, MoreArgs = list(se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
			else{
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, n.ahead=n.ahead, MoreArgs = list(xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, ylab=colnames(TimeSeries), n.ahead=n.ahead, MoreArgs = list(xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
		}
		else{
			if(!is.null(xreg) & !is.null(newxreg)) {
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, xreg=xreg, newxreg=newxreg, MoreArgs = list(n.ahead=n.ahead, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, ylab=colnames(TimeSeries), xreg=xreg, newxreg=newxreg, MoreArgs = list(n.ahead=n.ahead, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
			else{
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, MoreArgs = list(n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, ylab=colnames(TimeSeries), MoreArgs = list(n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
		}
	}
    else{
        if(!is.null(n.ahead)) {
			if(!is.null(xreg) & !is.null(newxreg)) {
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, MoreArgs = list(timeseries.cont=TimeSeriesCont, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, ylab=colnames(TimeSeries), n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, MoreArgs = list(timeseries.cont=TimeSeriesCont, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
			else{
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, n.ahead=n.ahead, MoreArgs = list(timeseries.cont=TimeSeriesCont, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, ylab=colnames(TimeSeries), n.ahead=n.ahead, MoreArgs = list(timeseries.cont=TimeSeriesCont, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
		}
		else{
			if(!is.null(xreg) & !is.null(newxreg)) {
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, xreg=xreg, newxreg=newxreg, MoreArgs = list(timeseries.cont=TimeSeriesCont, n.ahead=n.ahead, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, ylab=colnames(TimeSeries), xreg=xreg, newxreg=newxreg, MoreArgs = list(timeseries.cont=TimeSeriesCont, n.ahead=n.ahead, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
			else{
				if(!is.null(ylab)) {
					Predictions <- mapply(arimapred, TimeSeries, MoreArgs = list(timeseries.cont=TimeSeriesCont, n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
				else {
					Predictions <- mapply(arimapred, TimeSeries, ylab=colnames(TimeSeries), MoreArgs = list(timeseries.cont=TimeSeriesCont, n.ahead=n.ahead, xreg=xreg, newxreg=newxreg, se.fit=se.fit, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
				}
			}
		}
    }
    return (Predictions)
}
