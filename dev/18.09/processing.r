#Class prep
new_prep <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"prep")
  )
}

validate_prep <- function(prep_obj){
  values <- unclass(prep_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing preprocessing function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(prep_obj)
}

prep <- function(func, par=NULL, ..., subclass=NULL){
  validate_prep(new_prep(func, par, ..., subclass=subclass))
}

is.prep <- function(prep_obj){
  is(prep_obj,"prep")
}

preprocess.prep <- function(obj,...){
  do.call(obj$func,c(list(...),obj$par))
}


#Class postp
new_postp <- function(func, par, ..., subclass=NULL){
  stopifnot(is.function(func))
  if(!is.null(par)) stopifnot(is.list(par))
  
  structure(
    list(
      func = func,
      par = par,
      ...
    ),
    class = c(subclass,"postp")
  )
}

validate_postp <- function(postp_obj){
  values <- unclass(postp_obj)
  
  if(is.null(values$func) || !is.function(values$func)) {
    stop("argument 'func' must be a non-missing postprocessing function",call. = FALSE)
  }
  if(!is.null(values$par) && !is.list(values$par)) {
    stop("argument 'par' must be NULL or a list of named parameters",call. = FALSE)
  }
  
  return(postp_obj)
}

postp <- function(func, par=NULL, ..., subclass=NULL){
  validate_postp(new_postp(func, par, ..., subclass=subclass))
}

is.postp <- function(postp_obj){
  is(postp_obj,"postp")
}

postprocess.postp <- function(obj,...){
  do.call(obj$func,c(list(...),obj$par))
}


#Class processing
new_processing <- function(prep, postp, ..., subclass=NULL){
  stopifnot(is.prep(prep))
  if(!is.null(postp)) stopifnot(is.postp(postp))
  
  structure(
    list(
      prep = prep,
      postp = postp,
      ...
    ),
    class = c(subclass,"processing")
  )
}

validate_processing <- function(processing_obj){
  values <- unclass(processing_obj)
  
  if(is.null(values$prep) || !is.prep(values$prep)) {
    stop("argument 'prep' must be a non-missing preprocessing ('prep') object",call. = FALSE)
  }
  if(!is.null(values$postp) && !is.postp(values$postp)) {
    stop("argument 'postp' must be NULL or a postprocessing ('postp') object",call. = FALSE)
  }
  
  return(processing_obj)
}

processing <- function(prep_func, prep_par=NULL, postp_func=NULL, postp_par=NULL, ..., subclass=NULL){
  prep_obj <- do.call(prep,list(func=prep_func, par=prep_par, subclass=NULL))
  postp_obj <- NULL
  if(!is.null(postp_func))
    postp_obj <- do.call(postp,list(func=postp_func, par=postp_par, subclass=NULL))
  
  validate_processing(new_processing(prep_obj, postp_obj, ..., subclass=subclass))
}

is.processing <- function(processing_obj){
  is(processing_obj,"processing")
}

preprocess.processing <- function(obj,data,...,map=TRUE){

  res <- list()
  if(map){
    for(d in c(1:length(data))){
      data_d <- as.ts(data[[d]])
      
      proc_res <- preprocess(obj$prep,data_d,...)
      attr(proc_res,"name") <- names(data[d])
      res[[d]] <- result(obj,proc_res)
    }
  }
  else {
    proc_res <- preprocess(obj$prep,data,...)
    res[[1]] <- result(obj,proc_res)
  }
  
  return(results(res))
}

postprocess.processing <- function(obj,data,...,map=TRUE){
  
  res <- list()
  if(map){
    for(d in c(1:length(data))){
      data_d <- as.ts(data[[d]])
      
      proc_res <- postprocess(obj$postp,data_d,...)
      attr(proc_res,"name") <- names(data[d])
      res[[d]] <- result(obj,proc_res)
    }
  }
  else {
    proc_res <- postprocess(obj$postp,data,...)
    res[[1]] <- result(obj,proc_res)
  }
  
  return(results(res))
}

updt.processing <- function(obj,par=NULL,value=NULL){
  if(par %in% names(obj$prep$par)) obj$prep$par[[par]] <- value
  if(par %in% names(obj$postp$par)) obj$postp$par[[par]] <- value
  
  return(obj)
}

is.processing.result <- function(res){
  is(res,"list") && names(res)==c("obj","res")
}

summary.processing <- function(obj,...){
  cat("Data-processing class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$prep$par) && is.null(obj$postp$par)) cat("Parameters: N/A\n")
}