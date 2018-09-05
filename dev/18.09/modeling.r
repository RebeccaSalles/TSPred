#Class train
new_train <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"train")
  )
}

validate_train <- function(train_obj){
  values <- unclass(train_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing training function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(train_obj)
}

train <- function(func, par=NULL, ..., subclass=NULL){
  validate_train(new_train(func, par, ..., subclass=subclass))
}

is.train <- function(train_obj){
  is(train_obj,"train")
}

run.train <- function(obj,...){
  do.call(obj$func,c(obj$par,list(...)))
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

run.pred <- function(obj,...){
  do.call(obj$func,c(obj$par,list(...)))
}


#Class modeling
new_modeling <- function(train, pred, ..., subclass=NULL){
  stopifnot(is.train(train))
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
  
  if(is.null(values$train) || !is.train(values$train)) {
    stop("argument 'train' must be a non-missing training ('train') object",call. = FALSE)
  }
  if(!is.null(values$pred) && !is.pred(values$pred)) {
    stop("argument 'pred' must be NULL or a prediction ('pred') object",call. = FALSE)
  }
  
  return(modeling_obj)
}

modeling <- function(train_func, train_par=NULL, pred_func=NULL, pred_par=NULL, ..., subclass=NULL){
  train_obj <- do.call(train,c(train_func, train_par, subclass=NULL))
  pred_obj <- NULL
  if(!is.null(pred_func))
    pred_obj <- do.call(pred,c(pred_func, pred_par, subclass=NULL))
  
  validate_modeling(new_modeling(train_obj, pred_obj, ..., subclass=subclass))
}

is.modeling <- function(modeling_obj){
  is(modeling_obj,"modeling")
}

run.modeling <- function(obj,...,pred=FALSE){
  if(!pred)
    result <- run(obj$train,...)
  else
    result <- run(obj$pred,...)
  
  return(list(obj=obj,res=result))
}


#Summary method
summary.modeling <- function(obj,...){
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$train$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}