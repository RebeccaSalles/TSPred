#Subclass MSE
MSE <- function(){
  evaluating(eval_func=TSPred::MSE, eval_par=NULL, method="Mean Squared Error", subclass="MSE")
}










#============== DO ==============

#Subclass NMSE #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass MAPE #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass sMAPE #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass RMSE #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass MAXERROR #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass AIC #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass BIC #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass AICc #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}

#Subclass LOGLIK #DO
ARIMA <- function(){
  evaluating(eval_func=forecast::auto.arima, eval_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  if(!is.null(obj$par)){
    cat("Parameters:\n")
    print(obj$par)
  }
}