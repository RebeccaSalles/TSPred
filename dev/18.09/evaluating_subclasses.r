#Subclass MSE
MSE <- function(){
  error(eval_func=TSPred::MSE, eval_par=NULL, method="Mean Squared Error", subclass="MSE")
}

#Subclass NMSE
NMSE <- function(eval_par=list(train.actual=NULL)){
  error(eval_func=TSPred::NMSE, eval_par=eval_par, method="Normalised Mean Squared Error", subclass="NMSE")
}

#Subclass RMSE
RMSE <- function(){
  error(eval_func=ModelMetrics::rmse, eval_par=eval_par, method="Root Mean Squared Error", subclass="RMSE")
}

#Subclass MAPE
MAPE <- function(){
  error(eval_func=TSPred::MAPE, eval_par=NULL, method="Mean Absolute Percentage Error", subclass="MAPE")
}

#Subclass sMAPE
sMAPE <- function(){
  error(eval_func=TSPred::sMAPE, eval_par=NULL, method="Symmetric Mean Absolute Percentage Error", subclass="sMAPE")
}

#Subclass MAXError
MAXError <- function(){
  error(eval_func=TSPred::MAXError, eval_par=NULL, method="Maximal Error", subclass="MAXError")
}

#Subclass AIC
AIC <- function(){
  fitness(eval_func=stats::AIC, eval_par=NULL, method="Akaike's Information Criterion", subclass="AIC")
}

#Subclass BIC
BIC <- function(){
  fitness(eval_func=stats::BIC, eval_par=NULL, method="Schwarz's Bayesian Information Criterion", subclass="BIC")
}

#Subclass AICc
AICc <- function(){
  fitness(eval_func=MuMIn::AICc, eval_par=NULL, method="Second-order Akaike's Information Criterion", subclass="AICc")
}

#Subclass logLik
logLik <- function(){
  fitness(eval_func=stats::logLik, eval_par=NULL, method="Log-Likelihood", subclass="logLik")
}