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
#Subclass ARIMAKF #DO
#Subclass POLYRKF #DO
#Subclass ETS #DO
#Subclass HW #DO
#Subclass TF #DO
#Subclass NNET #DO
#Subclass RREGF #DO
#Subclass SVM #DO