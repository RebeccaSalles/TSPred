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

#Subclass ETS
ETS <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::ets, train_par=c(train_par),
         pred_func = forecast_mean, pred_par=c(pred_par),
         method="Exponential Smoothing State Space model", subclass="ETS")
}
summary.ETS <- function(obj,...){
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

#Subclass HW
HW <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::hw, train_par=c(train_par),
         pred_func = forecast_mean, pred_par=c(pred_par),
         method="Holt-Winter's Exponential Smoothing model", subclass="HW")
}
summary.HW <- function(obj,...){
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

#Subclass TF
TF <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  thetaf_model <- function(...){
    do.call(forecast::thetaf,c(list(...)))$model
  }
  
  linear(train_func = thetaf_model, train_par=c(train_par),
         pred_func = forecast_mean, pred_par=c(pred_par),
         method="Theta Forecasting model", subclass="TF")
}
summary.TF <- function(obj,...){
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
NNET <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = nnet::nnet, train_par=c(list(size=size),train_par),
      pred_func = predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
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

#Subclass RFrst
RFrst <- function(ntree=500,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = randomForest::randomForest, train_par=c(list(ntree=ntree),train_par),
      pred_func = predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Random Forest model", subclass="RFrst")
}
fitted.randomForest <- function(obj,...){
  return(obj$predicted)
}
residuals.randomForest <- function(obj,...){
  return(obj$y-obj$predicted)
}
summary.RFrst <- function(obj,...){
  NextMethod()
  if(!is.null(obj$train$par) || !is.null(obj$pred$par))  cat("Parameters:\n")
  cat("\tNumber of trees: ",obj$train$par$ntree,"\n")
  if(length(obj$train$par)>1){
    cat("\nOther parameters:\n")
    print(obj$train$par[-1])
  }
  
  if(!is.null(obj$pred$par)){
    cat("Predicting parameters:\n")
    print(obj$pred$par)
  }
}

#Subclass RBF
RBF <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = RSNNS::rbf, train_par=c(list(size=size),train_par),
      pred_func = predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Radial Basis Function (RBF) Network model", subclass="RBF")
}
residuals.rbf <- function(obj,...){
  return(attr(obj,"y")-fitted(obj))
}
summary.RBF <- function(obj,...){
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

#Subclass SVM
SVM <- function(train_par=list(), pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = e1071::svm, train_par=c(train_par),
      pred_func = predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Support Vector Machine model", subclass="SVM")
}
summary.SVM <- function(obj,...){
  NextMethod()
  if(!is.null(obj$train$par) || !is.null(obj$pred$par))  cat("Parameters:\n")
  if(length(obj$train$par)>0){
    print(obj$train$par)
  }
  
  if(!is.null(obj$pred$par)){
    cat("Predicting parameters:\n")
    print(obj$pred$par)
  }
}

#Subclass MLP
MLP <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = RSNNS::mlp, train_par=c(list(size=size),train_par),
      pred_func = predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Multi-Layer Perceptron (MLP) Network model", subclass="MLP")
}
summary.MLP <- function(obj,...){
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

#Subclass ELM
ELM <- function(train_par=list(), pred_par=list(), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = elmNNRcpp::elm_train, train_par=c(train_par),
      pred_func = elmNNRcpp::elm_predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Extreme Learning Machine (ELM) model", subclass="ELM")
}
summary.ELM <- function(obj,...){
  NextMethod()
  if(!is.null(obj$train$par) || !is.null(obj$pred$par))  cat("Parameters:\n")
  if(length(obj$train$par)>0){
    print(obj$train$par)
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