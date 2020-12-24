#' Prediction/modeling quality metrics
#'
#' Constructors for the \code{evaluating} class representing a time series prediction
#' or modeling fitness quality evaluation based on particular metrics.
#'
#' @section Error metrics:
#' 	Mean Squared Error.
#' 
#' @aliases evaluating fitness error
#'
#' @return An object of class \code{evaluating}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords quality evaluation metric
#' 
#' @rdname MSE
#' @export MSE
MSE <- function(){
  error(eval_func=TSPred::MSE, eval_par=NULL, method="Mean Squared Error", subclass="MSE")
}

#Subclass NMSE
#' @rdname MSE
#' @section Error metrics:
#' 	Normalised Mean Squared Error.
#' @param eval_par List of named parameters required by \code{\link{NMSE}} such as \code{train.actual}.
#' @export
NMSE <- function(eval_par=list(train.actual=NULL)){
  error(eval_func=TSPred::NMSE, eval_par=eval_par, method="Normalised Mean Squared Error", subclass="NMSE")
}

#Subclass RMSE
#' @rdname MSE
#' @section Error metrics:
#' 	Root Mean Squared Error.
#' @export
RMSE <- function(){
  error(eval_func=ModelMetrics::rmse, eval_par=eval_par, method="Root Mean Squared Error", subclass="RMSE")
}

#Subclass MAPE
#' @rdname MSE
#' @section Error metrics:
#' 	Mean Absolute Percentage Error.
#' @export
MAPE <- function(){
  error(eval_func=TSPred::MAPE, eval_par=NULL, method="Mean Absolute Percentage Error", subclass="MAPE")
}

#Subclass sMAPE
#' @rdname MSE
#' @section Error metrics:
#' 	Symmetric Mean Absolute Percentage Error.
#' @export
sMAPE <- function(){
  error(eval_func=TSPred::sMAPE, eval_par=NULL, method="Symmetric Mean Absolute Percentage Error", subclass="sMAPE")
}

#Subclass MAXError
#' @rdname MSE
#' @section Error metrics:
#' 	Maximal Error.
#' @export
MAXError <- function(){
  error(eval_func=TSPred::MAXError, eval_par=NULL, method="Maximal Error", subclass="MAXError")
}

#Subclass AIC
#' @rdname MSE
#' @section Fitness criteria:
#' 	Akaike's Information Criterion.
#' @export
AIC <- function(){
  fitness(eval_func=stats::AIC, eval_par=NULL, method="Akaike's Information Criterion", subclass="AIC")
}

#Subclass BIC
#' @rdname MSE
#' @section Fitness criteria:
#' 	Schwarz's Bayesian Information Criterion.
#' @export
BIC <- function(){
  fitness(eval_func=stats::BIC, eval_par=NULL, method="Schwarz's Bayesian Information Criterion", subclass="BIC")
}

#Subclass AICc
#' @rdname MSE
#' @section Fitness criteria:
#' 	Second-order Akaike's Information Criterion.
#' @export
AICc <- function(){
  fitness(eval_func=MuMIn::AICc, eval_par=NULL, method="Second-order Akaike's Information Criterion", subclass="AICc")
}

#Subclass logLik
#' @rdname MSE
#' @section Fitness criteria:
#' 	Log-Likelihood.
#' @export
logLik <- function(){
  fitness(eval_func=stats::logLik, eval_par=NULL, method="Log-Likelihood", subclass="logLik")
}