#Subclass ARIMA
ARIMA <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::auto.arima, train_par=c(train_par),
        pred_func = forecast_mean, pred_par=c(pred_par),
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


#Subclass NNET
NNET <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95))){
  MLM(train_func = nnet::nnet, train_par=c(list(size=size),train_par),
      pred_func = predict, pred_par=c(pred_par),
      method="Artificial Neural Network model", subclass="NNET")
}
summary.NNET <- function(obj,...){
  NextMethod()
  if(!is.null(obj$train$par) || !is.null(obj$pred$par))  cat("Parameters:\n")
  cat("\tUnits in the hidden layer: ",obj$train$par$size,"\n")
  if(length(obj$train$par)>1){
    cat("\nOther parameters:\n")
    print(obj$train$par[-1])
  }
  
  if(!is.null(obj$pred$par)){
    cat("Predicting parameters:\n")
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