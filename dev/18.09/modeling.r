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

run.trn <- function(obj,...){
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

run.pred <- function(obj,...){
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

run.modeling <- function(obj,input,...,map=TRUE,pred=FALSE){
  
  mdl <- function(obj,input,...,pred=FALSE){
    if(!pred) run(obj$train,input,...)
    else run(obj$pred,input,...)
  }
  
  res <- list()
  if(map){
    for(i in c(1:length(input))){
      input_i <- input[[i]]
      if(!pred) input_i <- as.ts(input[[i]])
      
      proc_res <- mdl(obj,input_i,...,pred=pred)
      attr(proc_res,"name") <- names(input[i])
      res[[i]] <- result(obj,proc_res)
    }
  }
  else {
    proc_res <- mdl(obj,input,...,pred=pred)
    res[[1]] <- result(obj,proc_res)
  }
  
  return(results(res))
}

#Summary method
summary.modeling <- function(obj,...){
  cat("Modeling class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$trn$par) && is.null(obj$pred$par)) cat("Parameters: N/A\n")
}