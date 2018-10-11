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


#Subclass NNET
NNET <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95))){
  
  nnet_io <- function(data,...){
    io <- mlm_io(data)
    do.call(nnet::nnet,c(list(x=io$input),list(y=io$output),list(...)))
  }
  
  predict_io <- function(input,...){
    mdl <- input[[1]]
    newdata <- input[[2]]
    io <- mlm_io(newdata)
    do.call(predict,c(list(object=mdl),list(newdata=io$input),list(...)))
  }
  
  modeling(train_func = nnet_io, train_par=c(list(size=size),train_par),
           pred_func = predict_io, pred_par=c(pred_par),
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