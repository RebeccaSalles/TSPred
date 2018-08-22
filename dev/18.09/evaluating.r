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

run.evaluating <- function(obj,...){
  result <- do.call(obj$func,c(obj$par,list(...)))
  
  return(list(obj=obj,res=result))
}


#Summary method
summary.evaluating <- function(obj,...){
  cat("Evaluating class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$par)) cat("Parameters: N/A\n")
}