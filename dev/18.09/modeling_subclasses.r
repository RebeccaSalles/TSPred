#Subclass ARIMA
ARIMA <- function(train_par=NULL, pred_par=list(level=c(80,95))){
  modeling(train_func = forecast::auto.arima, train_par=c(list(train_par)),
           pred_func = forecast::forecast, pred_par=c(list(pred_par)),
           method="ARIMA model", subclass="ARIMA")
}
summary.ARIMA <- function(obj,...){
  NextMethod()
  if(!is.null(obj$train$par) || !is.null(obj$pred$par))  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}













#============== DO ==============

#Subclass POLYR #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass ARIMAKF #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass POLYRKF #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass ETS #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass HW #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass TF #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass NNET #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass RREGF #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}

#Subclass SVM #DO
ARIMA <- function(){
  modeling(train_func=forecast::auto.arima, train_par=NULL, pred_func=forecast::forecast, pred_par=NULL, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){ #DO
  NextMethod()
  cat("Parameters:\n")
  if(!is.null(obj$train$par)){
    cat("\tTraining:\n")
    print(obj$train$par)
  }
  if(!is.null(obj$pred$par)){
    cat("\tPredicting:\n")
    print(obj$pred$par)
  }
}