#Class evaluating
new_evaluating <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"evaluating")
  )
}

validate_evaluating <- function(evaluating_obj){
  values <- unclass(evaluating_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing evaluating function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(evaluating_obj)
}

evaluating <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  validate_evaluating(new_evaluating(eval_func, eval_par, ..., subclass=subclass))
}

is.evaluating <- function(evaluating_obj){
  is(evaluating_obj,"evaluating")
}

evaluate.evaluating <- function(obj, test, pred, ...){
  result <- do.call(obj$func,c(list(test),list(pred),list(...),obj$par))
  attr(result,"name") <- attr(test,"name")
  res <- list(result(obj,result))

  return(results(res))
}

summary.evaluating <- function(obj,...){
  cat("Evaluating class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$par)) cat("Parameters: N/A\n")
}

#Subclass fitness
fitness <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  evaluating(eval_func=eval_func, 
             eval_par=eval_par,
             ...,
             subclass=c(subclass,"fitness"))
}

is.fitness <- function(fitness_obj){
  is(fitness_obj,"fitness")
}

evaluate.fitness <- function(obj, mdl, test, pred, ...){
  result <- do.call(obj$func,c(list(mdl),obj$par))
  attr(result,"name") <- attr(mdl,"name")
  res <- list(result(obj,result))
  
  return(results(res))
}


#Subclass error
error <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  evaluating(eval_func=eval_func, 
             eval_par=eval_par,
             ...,
             subclass=c(subclass,"error"))
}

is.error <- function(error_obj){
  is(error_obj,"error")
}

evaluate.error <- function(obj, mdl, test, pred, ..., fitness=FALSE){
  if(fitness){
    test <- fitted(mdl)+residuals(mdl)
    pred <- fitted(mdl)
    result <- do.call(obj$func,c(list(test),list(pred),list(...),obj$par))
    attr(result,"name") <- attr(mdl,"name")
  }
  else{
    result <- do.call(obj$func,c(list(test),list(pred),list(...),obj$par))
    attr(result,"name") <- attr(test,"name")
  }
  
  res <- list(result(obj,result))
  
  return(results(res))
}