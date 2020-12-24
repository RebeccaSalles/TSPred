#' Plot ARIMA predictions against actual values 
#' 
#' The function plots ARIMA predictions against its actual values with
#' prediction intervals. 
#' 
#' The model in \code{fit.arima} is used for prediction by the
#' \code{\link[forecast]{forecast.Arima}} function in the \code{forecast} package. The
#' resulting \code{forecast} object is then used for plotting the predictions
#' and their intervals by the \code{\link[forecast]{plot.forecast}} function also in the
#' \code{forecast} package. For more details, see the
#' \code{\link[forecast]{forecast.Arima}} and the \code{\link[forecast]{plot.forecast}} functions
#' in the \code{forecast} package. 
#' 
#' @param ts.cont A vector or univariate time series containing actual values
#' for a time series that are to be plotted against its respective predictions.
#' The number of consecutive values to be predicted is assumed to be equal to
#' the number of rows in \code{ts.cont}. If \code{xreg} is used, the number of
#' values to be predicted is set to the number of rows of \code{xreg}. %%
#' ~~Describe \code{ts.cont} here~~
#' @param fit.arima A fitted ARIMA model for the time series that is to be
#' predicted. An object of class "\code{Arima}", "\code{ar}" or
#' "\code{fracdiff}". See the \code{object} argument of the
#' \code{\link[forecast]{forecast.Arima}} function in the forecast package. 
#' @param xlim Numeric vector containing the initial and final limits of the
#' x-axis to be plotted, respectively. 
#' @param range.percent A percentage which defines how much the range of the
#' graphic's y-axis will be increased from the minimum limits imposed by data.
#' 
#' @param xreg A vector, matrix, data frame or times series with new values of
#' external regressors to be used for prediction (for class Arima objects
#' only). See the \code{xreg} argument of the \code{\link[forecast]{forecast.Arima}}
#' function in the forecast package. 
#' @param ylab A title for the graphic's y-axis. Ignored if \code{NULL}. %%
#' ~~Describe \code{ylab} here~~
#' @param xlab A title for the graphic's x-axis. Ignored if \code{NULL}. %%
#' ~~Describe \code{xlab} here~~
#' @param main An overall title for the graphic. Ignored if \code{NULL}. %%
#' ~~Describe \code{main} here~~
#' @return None.
#' @author Rebecca Pontes Salles 
#' @seealso \code{\link[forecast]{forecast.Arima}}, \code{\link[forecast]{plot.forecast}},
#' \code{\link{arimapred}} ~
#' @references See the \code{\link[forecast]{forecast.Arima}} and the
#' \code{\link[forecast]{plot.forecast}} functions in the forecast package. %% ~put
#' references to the literature/web site here ~
#' @keywords ARIMA prediction plot
#' @examples
#' 
#' data(SantaFe.A,SantaFe.A.cont)
#' fit <- forecast::auto.arima(SantaFe.A)  
#' ts.cont <- ts(SantaFe.A.cont,start=1001)
#' plotarimapred(ts.cont, fit, xlim=c(1001,1100))
#' 
#' @export plotarimapred
plotarimapred <-
function(ts.cont, fit.arima, xlim, range.percent=0.2, xreg=NULL, ylab=NULL, xlab=NULL, main=NULL){
  if(!is.null(xreg)) ts.pred <- forecast::forecast(fit.arima,length(ts.cont),xreg=xreg)
  else ts.pred <- forecast::forecast(fit.arima,length(ts.cont))
  
  mn <- min(ts.pred$lower,ts.cont)
  mx <- max(ts.pred$upper,ts.cont)
  
  yrange.min <- mn-range.percent*abs(mn)
  yrange.max <- mx+range.percent*abs(mx)
  yrange <- range(yrange.min,yrange.max)
  
  plot(ts.pred,xlim=xlim,ylim=yrange, ylab=ylab, xlab=xlab, main=main)
  graphics::lines(ts.cont,xlim=xlim, lty=5, lwd=2, col="black")
}
