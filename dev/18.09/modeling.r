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
  is(trn_obj,"trn")
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
  is(pred_obj,"pred")
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


modeling <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  train_obj <- do.call(trn,list(func=train_func, par=train_par, subclass=NULL))
  pred_obj <- NULL
  if(!is.null(pred_func))
    pred_obj <- do.call(pred,list(func=pred_func, par=pred_par, subclass=NULL))
  
  validate_modeling(new_modeling(train_obj, pred_obj, ..., subclass=subclass))
}

is.modeling <- function(modeling_obj){
  is(modeling_obj,"modeling")
}

#Summary method
summary.modeling <- function(obj,...){
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}


#Subclass MLM
MLM <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  modeling(train_func=train_func, 
           train_par=train_par,
           pred_func=pred_func,
           pred_par=pred_par,
           ...,
           subclass="MLM")
}

is.MLM <- function(MLM_obj){
  is(MLM_obj,"MLM")
}

train.MLM <- function(obj,data,...){
  res <- list()
  
  for(i in c(1:length(data))){
    data_i <- as.ts(data[[i]])
    io <- mlm_io(data_i)
    
    proc_res <- train(obj$train, io$input, io$output, ...)
    attr(proc_res,"name") <- names(data[i])
    res[[i]] <- result(obj,proc_res)
  }
  
  return(results(res))
}

predict.MLM <- function(obj,mdl,data,n.ahead,...,onestep=TRUE){
  res <- list()
  
  for(i in c(1:length(mdl))){
    mdl_i <- mdl[[i]]
    data_i <- as.ts(data[[i]])
    io <- mlm_io(data_i)
    
    if(onestep){
      proc_res <- predict(obj$pred, mdl_i, io$input,...)
      attr(proc_res,"name") <- names(data[i])
      res[[i]] <- result(obj,proc_res)
    }
    else{
      predictions <- NULL
      tuple <- io$input[1,]
      len_tuple <- length(tuple)
      for(p in c(1:n.ahead)){
        proc_res <- predict(obj$pred, mdl_i, tuple,...)
        predictions <- c(predictions,proc_res)
        tuple <- tail(c(tuple,proc_res),len_tuple)
      }
      attr(predictions,"name") <- names(data[i])
      res[[i]] <- result(obj,predictions)
    }
  }
  
  return(results(res))
}

summary.MLM <- function(obj,...){
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  cat("Type: Machine learning model\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}


#Subclass linear
linear <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  modeling(train_func=train_func, 
           train_par=train_par,
           pred_func=pred_func,
           pred_par=pred_par,
           ...,
           subclass="linear")
}

is.linear <- function(linear_obj){
  is(linear_obj,"linear")
}

train.linear <- function(obj,data,...){
  res <- list()
  
  for(i in c(1:length(data))){
    data_i <- as.ts(data[[i]])
    
    proc_res <- train(obj$train, data_i, ...)
    attr(proc_res,"name") <- names(data[i])
    res[[i]] <- result(obj,proc_res)
  }
  
  return(results(res))
}

predict.linear <- function(obj,mdl,data,n.ahead,...,onestep=TRUE){
  res <- list()
  
  for(i in c(1:length(mdl))){
    mdl_i <- mdl[[i]]
    
    if(!onestep){
      proc_res <- predict(obj$pred, mdl_i, n.ahead,...)
      attr(proc_res,"name") <- names(mdl[i])
      res[[i]] <- result(obj,proc_res)
    }
    else{
      if(length(data)==1) test_i <- as.ts(data[[1]])
      else test_i <- as.ts(data[[i]])
      
      train_data <- fitted(mdl_i)+residuals(mdl_i)
      mdl_res <- mdl_i
      
      predictions <- NULL
      
      for(p in c(1:n.ahead)){
        proc_res <- predict(obj$pred, mdl_res, 1,...)
        predictions <- c(predictions,proc_res)
        
        train_data <- c(train_data,test_i[p])
        mdl_res <- train(obj$train, train_data, ...)
      }
      attr(predictions,"name") <- names(mdl[i])
      res[[i]] <- result(obj,predictions)
    }
  }
  
  return(results(res))
}

summary.linear <- function(obj,...){
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  cat("Type: Linear model\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}