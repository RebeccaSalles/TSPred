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
#' 	Support Vector Machine model. \code{train_func} set as \code{\link[e1071]{tune.svm}} 
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @export
SVM <- function(train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  
  tuned_svm <- function(...){
    do.call(e1071::tune.svm,c(ranges=c(epsilon=seq(0,1,0.1), cost=1:100),list(...)))$best.model
  }  

  MLM(train_func = tuned_svm, train_par=c(train_par),
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

#Subclass Tensor_CNN
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Convolutional Neural Network - TensorFlow.
#'  \code{train_func} based on functions from \code{\link[tensorflow]} and \code{\link[keras]},
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @export
Tensor_CNN <- function(train_par=NULL, pred_par=list(level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  
  ts_tensor_cnn <- function(X, Y) {
	  cnn_epochs <- 2000
	  build_model <- function(train_df) {
		set.seed(1)
		
		spec <- tfdatasets::feature_spec(train_df, t0 ~ . ) %>% 
		  tfdatasets::step_numeric_column(tfdatasets::all_numeric(), normalizer_fn = tfdatasets::scaler_standard()) %>% 
		  tfdatasets::fit()
		
		input <- tfdatasets::layer_input_from_dataset(train_df %>% dplyr::select(-t0))
		
		output <- input %>% 
		  keras::layer_dense_features(tfdatasets::dense_features(spec)) %>% 
		  keras::layer_dense(units = 64, activation = "relu") %>%
		  keras::layer_dense(units = 64, activation = "relu") %>%
		  keras::layer_dense(units = 1) 
		
		model <- keras::keras_model(input, output)
		
		model %>% 
		  keras::compile(
			loss = "mse",
			optimizer = keras::optimizer_rmsprop(),
			metrics = list("mean_absolute_error")
		  )
		
		return(model)
	  }
	  
	  XY <- data.frame(X)
	  XY$t0 <- Y
	  
	  model <- build_model(XY)
	  
	  print_dot_callback <- keras::callback_lambda(
		on_epoch_end = function(epoch, logs) {
		  if (epoch %% 800 == 0) cat("\n")
		  if (epoch %% 10 == 0) cat(".")
		}
	  )    
	  
	  history <- model %>% keras::fit(
		x = XY %>% dplyr::select(-t0),
		y = XY$t0,
		epochs = cnn_epochs,
		validation_split = 0.2,
		verbose = 0,
		callbacks = list(print_dot_callback)
	  )  
	  cat("\n")
	  
	  return(model)
	}  

  MLM(train_func = ts_tensor_cnn, train_par=c(train_par),
      pred_func = stats::predict, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Convolutional Neural Network - TensorFlow", subclass="Tensor_CNN")
}
#' @export
summary.Tensor_CNN <- function(object,...){
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

#Subclass Tensor_LSTM
#' @rdname ARIMA
#' @section Machine learning models:
#' 	Long Short Term Memory Neural Networks - TensorFlow.
#'  \code{train_func} based on functions from \code{\link[tensorflow]} and \code{\link[keras]},
#'  and \code{pred_func} set as \code{\link[stats]{predict}}.
#' @export
Tensor_LSTM <- function(train_par=NULL, pred_par=list(batch_size=1,level=c(80,95)), sw=SW(window_len = 6), proc=list(MM=MinMax())){
  
  ts_tensor_lstm <- function(X, Y) {
	  lstm_epochs <- 2000
  
	  print_dot_callback <- keras::callback_lambda(
		on_epoch_end = function(epoch, logs) {
		  if (epoch %% 800 == 0) cat("\n")
		  if (epoch %% 10 == 0) cat(".")
		}
	  )    
	  
	  set.seed(1)
	  batch.size <- 1
	  size <- ncol(X)
	  
	  X <- array(as.vector(X), dim=(c(dim(X),1)))
	  
	  model <- keras::keras_model_sequential()
	  model %>%
		keras::layer_lstm(units = 100,
				   input_shape = c(size, 1),
				   batch_size = batch.size,
				   return_sequences = TRUE,
				   stateful = TRUE) %>%
		keras::layer_dropout(rate = 0.5) %>%
		keras::layer_lstm(units = 50,
				   return_sequences = FALSE,
				   stateful = TRUE) %>%
		keras::layer_dropout(rate = 0.5) %>%
		keras::layer_dense(units = 1)
	  model %>%
		keras::compile(loss = 'mae', optimizer = 'adam')
	  
	  
	  model %>% keras::fit(x = X,
					y = Y,
					batch_size = batch.size,
					epochs = lstm_epochs,
					verbose = 0,
					shuffle = FALSE,
					callbacks = list(print_dot_callback)
	  )
	  model %>% keras::reset_states()
	  cat("\n")
	  
	  return(model)
  }
  
  predict_mean <- function(...){
    do.call(stats::predict,c(list(...)))[,1]
  }

  MLM(train_func = ts_tensor_lstm, train_par=c(train_par),
      pred_func = predict_mean, pred_par=c(pred_par),
      sw=sw, proc=proc,
      method="Long Short Term Memory Neural Networks - TensorFlow", subclass="Tensor_LSTM")
}
#' @export
summary.Tensor_LSTM <- function(object,...){
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