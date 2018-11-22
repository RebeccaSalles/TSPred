#Class result (generally returned by the function 'run')
new_result <- function(obj, res, ..., subclass=NULL){
  stopifnot(is.processing(obj)||is.modeling(obj)||is.evaluating(obj)||is.tspred(obj))
  
  structure(
    list(
      obj = obj,
      res = res,
      ...
    ),
    class = c(subclass,"result")
  )
}

validate_result <- function(result_obj){
  values <- unclass(result_obj)
  
  if(!is.null(values$obj)&&!is.processing(values$obj)&&!is.modeling(values$obj)&&!is.evaluating(values$obj)&&!is.tspred(values$obj)) {
    stop("argument 'obj' must be a non-missing TSPred class object ('processing','modeling','evaluating' or 'tspred')",call. = FALSE)
  }
  
  return(result_obj)
}

result <- function(obj, res=NULL, ..., subclass=NULL){
  validate_result(new_result(obj, res, ..., subclass=subclass))
}

is.result <- function(result_obj){
  is(result_obj,"result")
}

updt.result <- function(result_obj,...){
  updt(result_obj$obj,...)
}


#Class results
new_results <- function(results, ..., subclass=NULL){
  
  if(!is.null(results) && length(results)>0) for(r in results) stopifnot(is.result(r))
  
  structure(
    list(
      results = results,
      ...
    ),
    class = c(subclass,"results")
  )
}

validate_results <- function(results_obj){
  values <- unclass(results_obj)
  
  if(!is.null(values$results) && length(values$results)>0)
    for(r in values$results)
      if(!is.result(r))
        stop("argument 'results' must be NULL or a list of result ('result') objects",call. = FALSE)
  
  return(results_obj)
}

results <- function(results=NULL, ..., subclass=NULL){
  
  if(!is.list(results)) results <- list(results)
  
  validate_results(new_results(results=results, ..., subclass=subclass))
}

is.results <- function(results_obj){
  is(results_obj,"results")
}

updt.results <- function(obj,par=NULL,value=NULL){
  
  for(r in c(1:length(obj$results))){
    result <- obj$results[[r]]
    
    if(is.null(value)){
      if(par %in% names(attributes(result$res))) value_found <- attr(result$res,par)
      else if(par %in% names(result$res)) value_found <- result$res$par
      else stop(paste("no value found for updating parameter",par,sep=" "),call. = FALSE)
    }
    else value_found <- value
    
    result$obj <- updt(result$obj, par=par, value=value_found)
    
    obj$results[[r]] <- result
  }
  
  return(obj)
}

objs.results <- function(obj,...){
  objs <- list()
  for(r in c(1:length(obj$results))){
    ts_name <- attr(obj$results[[r]]$res,"name")
    if(is.null(ts_name)) ts_name <- "tmp_name"
    objs[[ts_name]] <- obj$results[[r]]$obj
  }
  return(objs)
}

res.results <- function(obj,...){
  res <- list()
  if(length(obj$results)==1 && is.list(obj$results[[1]]$res) && "list" %in% class(obj$results[[1]]$res)){
    res <- obj$results[[1]]$res
  }
  else{
    for(r in c(1:length(obj$results))) {
      ts_name <- attr(obj$results[[r]]$res,"name")
      if(is.null(ts_name)) ts_name <- "tmp_name"
      res[[ts_name]] <- obj$results[[r]]$res
    }
  }
  return(res)
}

summary.results <- function(obj,...){
  for(r in c(1:length(obj$results))){
    if(length(obj$results)>1) cat("\nData object",r,"of",length(obj$results),":",attr(obj$results[[r]]$res,"name"),"\n")
    summary(obj$results[[r]]$obj)
  }
}