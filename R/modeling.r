#Class train
new_trn <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"trn")
  )
}

validate_trn <- function(trn_obj){
  values <- unclass(trn_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing training function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(trn_obj)
}

trn <- function(func, par=NULL, ..., subclass=NULL){
  validate_trn(new_trn(func, par, ..., subclass=subclass))
}

is.trn <- function(trn_obj){
  methods::is(trn_obj,"trn")
}

train.trn <- function(obj,...){
  do.call(obj$func,c(list(...),obj$par))
}


#Class pred
new_pred <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"pred")
  )
}

validate_pred <- function(pred_obj){
  values <- unclass(pred_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing prediction function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(pred_obj)
}

pred <- function(func, par=NULL, ..., subclass=NULL){
  validate_pred(new_pred(func, par, ..., subclass=subclass))
}

is.pred <- function(pred_obj){
  methods::is(pred_obj,"pred")
}

predict.pred <- function(obj,...){
  do.call(obj$func,c(list(...),obj$par))
}


#Class modeling
new_modeling <- function(train, pred, ..., subclass=NULL){
  stopifnot(is.trn(train))
  if(!is.null(pred)) stopifnot(is.pred(pred))
  
  structure(
    list(
      train = train,
      pred = pred,
      ...
    ),
    class = c(subclass,"modeling")
  )
}

validate_modeling <- function(modeling_obj){
  values <- unclass(modeling_obj)
  
  if(is.null(values$train) || !is.trn(values$train)) {
    stop("argument 'train' must be a non-missing training ('trn') object",call. = FALSE)
  }
  if(!is.null(values$pred) && !is.pred(values$pred)) {
    stop("argument 'pred' must be NULL or a prediction ('pred') object",call. = FALSE)
  }
  
  return(modeling_obj)
}


#' Time series modeling and prediction
#'
#' Constructor for the \code{modeling} class representing a time series modeling
#' and prediction method based on a particular model.
#' The \code{modeling} class has two specialized subclasses \code{linear} and 
#' \code{MLM} reagarding linear models and machine learning based models, respectively.
#'
#' @aliases modeling linear MLM
#' @param train_func A function for training a particular model.
#' @param train_par List of named parameters required by \code{train_func}.
#' @param pred_func A function for prediction based on the model trained by \code{train_func}.
#' @param pred_par List of named parameters required by \code{pred_func}.
#' @param sw A \code{\link{SW}} object regarding sliding windows processing. Optional.
#' @param proc A list of \code{\link{processing}} objects regarding any pre(post)processing
#' needed during training or prediction. Optional.
#' @param ... Other parameters to be encapsulated in the class object.
#' @param subclass Name of new specialized subclass object created in case it is provided. 
#'
#' @return An object of class \code{modeling}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords modeling prediction model method
#' @examples
#' 
#' forecast_mean <- function(...){
#'    do.call(forecast::forecast,c(list(...)))$mean
#' }
#'   
#' l <- linear(train_func = forecast::auto.arima, pred_func = forecast_mean,
#'             method="ARIMA model", subclass="ARIMA")
#' summary(l)
#'
#' m <- MLM(train_func = nnet::nnet, train_par=list(size=5),
#'       pred_func = predict, sw=SW(window_len = 6), proc=list(MM=MinMax()),
#'       method="Artificial Neural Network model", subclass="NNET")
#' summary(m)
#' 
#' @export modeling
modeling <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  train_obj <- do.call(trn,list(func=train_func, par=train_par, subclass=NULL))
  pred_obj <- NULL
  if(!is.null(pred_func))
    pred_obj <- do.call(pred,list(func=pred_func, par=pred_par, subclass=NULL))
  
  validate_modeling(new_modeling(train_obj, pred_obj, ..., subclass=subclass))
}

#Subclass MLM
#' @rdname modeling
#' @export
MLM <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, sw=NULL, proc=NULL, ..., subclass=NULL){
  mlm_obj <- modeling( train_func=train_func, 
                       train_par=train_par,
                       pred_func=pred_func,
                       pred_par=pred_par,
                       sw=sw,
                       proc=proc,
                       ...,
                       subclass=c(subclass,"MLM"))
  validate_MLM(mlm_obj)
}
validate_MLM <- function(mlm_obj){
  mlm_obj <- validate_modeling(mlm_obj)
  values <- unclass(mlm_obj)
  
  if(!is.null(values$sw) && !is.SW(values$sw)) {
    stop("argument 'sw' must be NULL or a sliding windows processing ('SW') object",call. = FALSE)
  }
  if(!is.null(values$proc) && length(values$proc)>0) {
    for(p in values$proc)
      if(!is.processing(p))
        stop("argument 'proc' must be NULL or a list of processing ('processing') objects",call. = FALSE)
  }
  
  return(mlm_obj)
}

#Subclass linear
#' @rdname modeling
#' @export
linear <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  modeling(train_func=train_func, 
           train_par=train_par,
           pred_func=pred_func,
           pred_par=pred_par,
           ...,
           subclass=c(subclass,"linear"))
}

is.modeling <- function(modeling_obj){
  methods::is(modeling_obj,"modeling")
}

is.MLM <- function(MLM_obj){
  methods::is(MLM_obj,"MLM")
}

is.linear <- function(linear_obj){
  methods::is(linear_obj,"linear")
}

#Summary method
#' @export
summary.modeling <- function(object,...){
  obj <- object
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}

#' @export
summary.MLM <- function(object,...){
  obj <- object
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  cat("Type: Machine learning model\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}

#' @export
summary.linear <- function(object,...){
  obj <- object
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  cat("Type: Linear model\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}


#' Training a time series model
#'
#' \code{train} is a generic function for training a time series model
#' based on a particular training function defined in a \code{\link{modeling}} object.
#' The function invokes particular \emph{methods} which
#' depend on the class of the first argument.
#'
#' @aliases train
#' @param obj An object of class \code{\link{modeling}} defining a particular model.
#' @param data A list of time series to be modelled.
#' @param ... Other parameters passed to \code{train_func} of \code{obj}.
#'
#' For \code{train.MLM}, \code{sw} of \code{obj} may be used to transform the time series 
#' in \code{data} into sliding windows used during training. Also, 
#' \code{proc} of \code{obj} may be used to preprocess the time series before training.
#'
#' @return A list containing \code{obj} and the trained models.
#' @author Rebecca Pontes Salles
#' @family train
#'
#' @keywords model training
#' @examples
#' data(CATS,CATS.cont)
#'
#' a <- ARIMA()
#' model <- train(a,list(CATS[,1]))
#'
#' n <- NNET(size=5, sw=SW(window_len = 5+1), proc=list(MM=MinMax()))
#' model <- train(n,list(CATS[,1]))
#' 
#' @export train
train <- function(obj,...){
  UseMethod("train")
}

#' @rdname train
#' @export
train.MLM <- function(obj,data,...){
  res <- list()
  #browser()
  for(i in c(1:length(data))){
    data_i <- data[i]
    obj_i <- obj
    
    if(!is.null(obj$sw)){
      attr(data_i,"subset") <- "train"
      sw_res <- preprocess(obj$sw,data_i)
      obj_i$sw <- objs(sw_res)[[1]]
      attr(obj_i$sw,"train_data") <- utils::tail(data_i[[1]],obj_i$sw$prep$par$k-1)
      data_i <- res(sw_res)
    }
    if(!is.null(obj$proc)){
      for(p in c(1:length(obj$proc))){
        attr(data_i,"subset") <- "train"
        proc_res <- preprocess(obj$proc[[p]],data_i)
        obj_i$proc[[p]] <- objs(proc_res)[[1]]
        data_i <- res(proc_res)
      }
    }
    
    data_i <- stats::as.ts(data_i[[1]])
    io <- mlm_io(data_i)
    
    proc_res <- train(obj$train, as.matrix(io$input), as.matrix(io$output), ...)
    attr(proc_res,"name") <- names(data[i])
    attr(proc_res,"y") <- io$output
    
    if("list" %in% class(proc_res)) {
      aux_res <- list()
      aux_res[[attr(proc_res,"name")]] <- proc_res
      proc_res <- aux_res
    }
    
    res[[i]] <- result(obj_i,proc_res)
  }
  
  return(results(res))
}

#' @rdname train
#' @export
train.linear <- function(obj,data,...){
  res <- list()
  
  for(i in c(1:length(data))){
    data_i <- stats::as.ts(data[[i]])
    
    proc_res <- train(obj$train, data_i, ...)
    attr(proc_res,"name") <- names(data[i])
    res[[i]] <- result(obj,proc_res)
  }
  
  return(results(res))
}


#' Predict method for \code{\link{modeling}} objects
#'
#' Obtains time series predictions based on a trained model and a particular prediction function
#' defined in a \code{\link{modeling}} object.
#'
#' @aliases predict
#' @param object An object of class \code{\link{modeling}} defining a particular model.
#' @param mdl A time series model object used for prediction.
#' @param data A list of time series data input for prediction.
#' @param n.ahead Integer defining the number of observations to be predicted.
#' @param ... Other parameters passed to \code{pred_func} of \code{object}.
#' @param onestep Should the function produce one-step ahead predictions?
#' If \code{FALSE}, a multi-step ahead prediction approach is adopted.
#'
#' For \code{predict.MLM}, \code{sw} of \code{object} may be used to transform the time series 
#' input in \code{data} into sliding windows used during prediction. Also, 
#' \code{proc} of \code{object} may be used to preprocess/postprocess the input during prediction.
#'
#' @return A list containing \code{object} and the produced predictions.
#' @author Rebecca Pontes Salles
#' @family predict
#'
#' @keywords time series prediction
#' @examples
#' data(CATS,CATS.cont)
#'
#' a <- ARIMA()
#' model <- train(a,list(CATS[,1]))$results[[1]]$res
#' pred_data <- predict(a,model,data=NULL,n.ahead=20,onestep=FALSE)
#'
#' n <- NNET(size=5, sw=SW(window_len = 5+1), proc=list(MM=MinMax()))
#' model <- train(n,list(CATS[,1]))$results[[1]]$res
#' pred_data <- predict(n,model,data=list(CATS.cont[,1]),n.ahead=20)
#' 
#' @name predict
NULL
#predict is already a generic method
#predict <- function(obj,...){
#  UseMethod("predict")
#}

#' @rdname predict
#' @export
predict.MLM <- function(object,mdl,data,n.ahead,...,onestep=TRUE){
  obj <- object
  ts_name <- names(data)
  data <- as.list(data)
  
  if(is.list(mdl) && length(mdl)==1 && names(mdl)==ts_name) mdl <- mdl[[1]]
  
  if(!is.null(obj$sw)){
    data[[1]] <- c( attr(obj$sw,"train_data"), data[[1]] )
    attr(data,"subset") <- "test"
    sw_res <- preprocess(obj$sw,data)
    data <- res(sw_res)
  }
  
  data <- stats::as.ts(data[[1]])
  io <- mlm_io(data)
  
  if(onestep){
    input <- io$input
    #browser()
    obj_test <- NULL
    if(!is.null(obj$proc)){
      obj_test <- list()
      for(p in c(1:length(obj$proc))){
        attr(input,"subset") <- "test"
        proc_res <- preprocess(obj$proc[[p]],list(input))
        attr(proc_res$results[[1]]$res,"name") <- ts_name
        obj_test[[p]] <- objs(proc_res)[[1]]
        input <- res(proc_res)[[1]]
      }
    }
    
    proc_res <- stats::predict(obj$pred, mdl, as.matrix(input),...)
    
    if(!is.null(obj_test)){
      for(p in c(length(obj_test):1)){
        proc_res <- list(proc_res)
        names(proc_res) <- ts_name
        proc_res <- postprocess(obj_test[[p]],proc_res)
        proc_res <- res(proc_res)[[1]]
      }
    }
    
    attr(proc_res,"name") <- ts_name
    res <- list(result(obj,proc_res))
  }
  else{
    
    predictions <- NULL
    tuple <- io$input[1,]
    len_tuple <- length(tuple)
    
    names_tuple <- names(tuple)
    
    for(i in c(1:n.ahead)){
      
      obj_test <- NULL
      if(!is.null(obj$proc)){
        obj_test <- list()
        for(p in c(1:length(obj$proc))){
          attr(tuple,"subset") <- "test"
          proc_res <- preprocess(obj$proc[[p]],list(t(tuple)))
          attr(proc_res$results[[1]]$res,"name") <- ts_name
          obj_test[[p]] <- objs(proc_res)[[1]]
          tuple <- as.matrix(res(proc_res)[[1]])
        }
      }
      
      proc_res <- stats::predict(obj$pred, mdl, as.matrix(tuple),...)
      tuple <- utils::tail(c(tuple,proc_res),len_tuple)
      
      if(!is.null(obj_test)){
        for(p in c(length(obj_test):1)){
          tuple <- list(tuple)
          names(tuple) <- ts_name
          tuple <- postprocess(obj_test[[p]],tuple)
          tuple <- res(tuple)[[1]]
        }
      }
      
      names(tuple) <- names_tuple
      
      predictions <- c(predictions,utils::tail(tuple,1))
      
    }
    
    attr(predictions,"name") <- ts_name
    res <- list(result(obj,predictions))
  }
  
  return(results(res))
}

#' @rdname predict
#' @export
predict.linear <- function(object,mdl,data,n.ahead,...,onestep=TRUE){
  obj <- object
  if(!onestep){
    proc_res <- stats::predict(obj$pred, mdl, n.ahead,...)
    attr(proc_res,"name") <- attr(mdl,"name")
    res <- list(result(obj,proc_res))
  }
  else{
    test <- stats::as.ts(data[[1]])
    
    train_data <- stats::fitted(mdl)+stats::residuals(mdl)
    mdl_res <- mdl
    
    predictions <- NULL
    
    for(p in c(1:n.ahead)){
      proc_res <- stats::predict(obj$pred, mdl_res, 1,...)
      predictions <- c(predictions,proc_res)
      
      train_data <- c(train_data,test[p])
      mdl_res <- train(obj$train, train_data)
    }
    attr(predictions,"name") <- attr(mdl,"name")
    res <- list(result(obj,predictions))
  }
  
  return(results(res))
}