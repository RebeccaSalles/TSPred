#fit: return of auto.arima


#' Get ARIMA model parameters
#' 
#' The function returns the parameters of a fitted ARIMA model, including
#' non-seasonal and seasonal orders and drift.
#' 
#' The \code{fit} object could possibly be the result of
#' \code{\link[forecast]{auto.arima}} or \code{\link[forecast]{Arima}} of the
#' \code{forecast} package, or \code{\link[stats]{arima}} of the \code{stats}
#' package.
#' 
#' @param fit An object of class "Arima" containing a fitted ARIMA model.
#' @return A list giving the number of AR, MA, seasonal AR and seasonal MA
#' coefficients, plus the period and the number of non-seasonal and seasonal
#' differences of the provided ARIMA model. The value of the fitted drift
#' constant is also presented.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{fittestArima}},\code{\link{arimapred}}
#' @references R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
#' principles and practice. OTexts.
#' 
#' R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
#' Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
#' @keywords ARIMA parameters
#' @examples
#' 
#' data(SantaFe.A)
#' arimaparameters(forecast::auto.arima(SantaFe.A[,1]))
#' 
#' @export arimaparameters
arimaparameters <- function(fit){
  #fit$arma -> A compact form of the specification, as a vector giving the number of AR, MA, seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences.
	if(utils::tail(colnames(fit$var.coef),1)=="drift"){
		ARIMAModelInfo <- list(AR=fit$arma[1],Diff=fit$arma[6],MA=fit$arma[2],SeasonalAR=fit$arma[3],SeasonalDiff=fit$arma[7],SeasonalMA=fit$arma[4],Period=fit$arma[5],Drift=utils::tail(fit$coef,1))
	}
	else {
		ARIMAModelInfo <- list(AR=fit$arma[1],Diff=fit$arma[6],MA=fit$arma[2],SeasonalAR=fit$arma[3],SeasonalDiff=fit$arma[7],SeasonalMA=fit$arma[4],Period=fit$arma[5],Drift=NA)
	}
	return(ARIMAModelInfo)
}
