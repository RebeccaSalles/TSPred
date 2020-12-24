#' Time series prediction models
#'
#' Constructors for the \code{modeling} class representing a time series modeling
#' and prediction method based on a particular model.
#'
#' @section Linear models:
#' 	ARIMA model. \code{train_func} set as \code{\link[forecast]{auto.arima}}
#'  and \code{pred_func} set as \code{\link[forecast]{forecast}}.
#'
#' @param train_par List of named parameters required by \code{train_func}.
#' @param pred_par List of named parameters required by \code{pred_func}.
#' @param sw A \code{\link{SW}} object regarding sliding windows processing.
#' @param proc A list of \code{\link{processing}} objects regarding any pre(post)processing
#' needed during training or prediction.
#'
#' @return An object of class \code{modeling}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords modeling prediction model method
#' 
#' @rdname ARIMA
#' @export ARIMA
#Subclass ARIMA
ARIMA <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::auto.arima, train_par=c(train_par),
        pred_func = forecast_mean, pred_par=c(pred_par),
        method="ARIMA model", subclass="ARIMA")
}
#' @export
summary.ARIMA <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Linear models:
#' 	Exponential Smoothing State Space model. \code{train_func} set as \code{\link[forecast]{ets}}
#'  and \code{pred_func} set as \code{\link[forecast]{forecast}}.
#' @export
ETS <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::ets, train_par=c(train_par),
         pred_func = forecast_mean, pred_par=c(pred_par),
         method="Exponential Smoothing State Space model", subclass="ETS")
}
#' @export
summary.ETS <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Linear models:
#' 	Holt-Winter's Exponential Smoothing model. \code{train_func} set as \code{\link[forecast]{hw}}
#'  and \code{pred_func} set as \code{\link[forecast]{forecast}}.
#' @export
HW <- function(train_par=list(), pred_par=list(level=c(80,95))){
  
  forecast_mean <- function(...){
    do.call(forecast::forecast,c(list(...)))$mean
  }
  
  linear(train_func = forecast::hw, train_par=c(train_par),
         pred_func = forecast_mean, pred_par=c(pred_par),
         method="Holt-Winter's Exponential Smoothing model", subclass="HW")
}
#' @export
summary.HW <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Linear models:
#' 	Theta Forecasting model. \code{train_func} set as \code{\link[forecast]{thetaf}}
#'  and \code{pred_func} set as \code{\link[forecast]{forecast}}.
#' @export
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
#' @export
summary.TF <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Artificial Neural Network model. \code{train_func} set as \code{\link[nnet]{nnet}}
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @param size See \code{\link[nnet]{nnet}}
#' @export
NNET <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = nnet::nnet, train_par=c(list(size=size),train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Artificial Neural Network model", subclass="NNET")
}
#' @export
summary.NNET <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Random Forest model. \code{train_func} set as \code{\link[randomForest]{randomForest}}
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @param ntree See \code{\link[randomForest]{randomForest}}
#' @export
RFrst <- function(ntree=500,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = randomForest::randomForest, train_par=c(list(ntree=ntree),train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Random Forest model", subclass="RFrst")
}
#' @export
fitted.randomForest <- function(object,...){
  obj <- object
  return(obj$predicted)
}
#' @export
residuals.randomForest <- function(object,...){
  obj <- object
  return(obj$y-obj$predicted)
}
#' @export
summary.RFrst <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Radial Basis Function (RBF) Network model. \code{train_func} set as \code{\link[RSNNS]{rbf}}
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @param size See \code{\link[RSNNS]{rbf}}
#' @export
RBF <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = RSNNS::rbf, train_par=c(list(size=size),train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Radial Basis Function (RBF) Network model", subclass="RBF")
}
#' @export
residuals.rbf <- function(object,...){
  obj <- object
  return(attr(obj,"y")-stats::fitted(obj))
}
#' @export
summary.RBF <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Support Vector Machine model. \code{train_func} set as \code{\link[e1071]{svm}} 
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @export
SVM <- function(train_par=list(), pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = e1071::svm, train_par=c(train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Support Vector Machine model", subclass="SVM")
}
#' @export
summary.SVM <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Multi-Layer Perceptron (MLP) Network model. \code{train_func} set as \code{\link[RSNNS]{mlp}} 
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @param size See \code{\link[RSNNS]{mlp}}
#' @export
MLP <- function(size=5,train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = size+1), proc=list(MM=MinMax())){
  MLM(train_func = RSNNS::mlp, train_par=c(list(size=size),train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Multi-Layer Perceptron (MLP) Network model", subclass="MLP")
}
#' @export
summary.MLP <- function(object,...){
  obj <- object
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
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Extreme Learning Machine (ELM) model. \code{train_func} set as \code{\link[elmNNRcpp]{elm_train}} 
#'  and \code{pred_func} set as \code{\link[elmNNRcpp]{elm_predict}}.
#' @export
ELM <- function(train_par=list(), pred_par=list(), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  MLM(train_func = elmNNRcpp::elm_train, train_par=c(train_par),
      pred_func = elmNNRcpp::elm_predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Extreme Learning Machine (ELM) model", subclass="ELM")
}
#' @export
summary.ELM <- function(object,...){
  obj <- object
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