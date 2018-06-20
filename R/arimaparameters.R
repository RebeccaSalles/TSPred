#fit: return of auto.arima
arimaparameters <- function(fit){
  #fit$arma -> A compact form of the specification, as a vector giving the number of AR, MA, seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences.
	if(tail(colnames(fit$var.coef),1)=="drift"){
		ARIMAModelInfo <- list(AR=fit$arma[1],Diff=fit$arma[6],MA=fit$arma[2],SeasonalAR=fit$arma[3],SeasonalDiff=fit$arma[7],SeasonalMA=fit$arma[4],Period=fit$arma[5],Drift=tail(fit$coef,1))
	}
	else {
		ARIMAModelInfo <- list(AR=fit$arma[1],Diff=fit$arma[6],MA=fit$arma[2],SeasonalAR=fit$arma[3],SeasonalDiff=fit$arma[7],SeasonalMA=fit$arma[4],Period=fit$arma[5],Drift=NA)
	}
	return(ARIMAModelInfo)
}