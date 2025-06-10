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
  methods::is(prep_obj,"prep")
}

#' @export
preprocess.prep <- function(obj,...){
  do.call(obj$func,c(list(...),obj$par))
}


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
  methods::is(postp_obj,"postp")
}

#' @export
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

#' Time series data processing
#'
#' Constructor for the \code{processing} class representing a time series
#' processing method based on a particular time series transformation.
#'
#' @aliases processing
#' @param prep_func A function for preprocessing the time series data.
#' @param prep_par List of named parameters required by \code{prep_func}.
#' @param postp_func A function for postprocessing the time series data.
#' Generally reverses the transformation performed by \code{prep_func}.
#' @param postp_par List of named parameters required by \code{postp_func}.
#' @param ... Other parameters to be encapsulated in the class object.
#' @param subclass Name of new specialized subclass object created in case it is provided.
#'
#' @return An object of class \code{processing}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords processing transformation preprocessing postprocessing
#' @examples
#' base <- exp(1)
#' lt <- processing(prep_func=TSPred::LogT, prep_par=list(base=base),
#'                  postp_func=TSPred::LogT.rev, postp_par=list(base=base),
#'                  method="Logarithmic transform", subclass="LT")
#' summary(lt)
#'
#' @export processing
processing <- function(prep_func, prep_par=NULL, postp_func=NULL, postp_par=NULL, ..., subclass=NULL){
  prep_obj <- do.call(prep,list(func=prep_func, par=prep_par, subclass=NULL))
  postp_obj <- NULL
  if(!is.null(postp_func))
    postp_obj <- do.call(postp,list(func=postp_func, par=postp_par, subclass=NULL))

  validate_processing(new_processing(prep_obj, postp_obj, ..., subclass=subclass))
}


is.processing <- function(processing_obj){
  methods::is(processing_obj,"processing")
}

#' @export
summary.processing <- function(object,...){
  obj <- object
  cat("Data-processing class object\n")

  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")

  if(is.null(obj$prep$par) && is.null(obj$postp$par)) cat("Parameters: N/A\n")
}

# #' @export
updt.processing <- function(obj,par=NULL,value=NULL){
  if(par %in% names(obj$prep$par)) obj$prep$par[[par]] <- value
  if(par %in% names(obj$postp$par)) obj$postp$par[[par]] <- value

  return(obj)
}

is.processing.result <- function(res){
  methods::is(res,"list") && names(res)==c("obj","res")
}


#' Preprocessing/Postprocessing time series data
#'
#' \code{preprocess} and \code{postprocess} are generic functions for
#' preprocessing and postprocessing time series data, respectively, based on
#' a particular transformation method defined in a \code{\link{processing}} object.
#' Generally, postprocessing reverses the transformation performed during preprocessing.
#'
#' @aliases preprocess postprocess
#' @param obj An object of class \code{\link{processing}} defining a particular transformation method.
#' @param data A list of time series to be transformed.
#' @param ... Other parameters passed to \code{prep_func}/\code{postp_func} of \code{obj}.
#' @param map Should the transformation be performed in each individual time series?
#' If \code{FALSE} the function processes the provided set of time series as a whole.
#'
#' @return A list containing \code{obj} and the transformed time series.
#' @author Rebecca Pontes Salles
#' @family processing
#'
#' @keywords processing transformation preprocessing postprocessing
#' @examples
#' data(NN5.A)
#' t <- LT(base = exp(1))
#' prep_ts <- preprocess(t,list(NN5.A[,10]))$results[[1]]$res
#' postp_ts <- postprocess(t,list(prep_ts))$results[[1]]$res
#'
#' @export preprocess
preprocess <- function(obj,...){
  UseMethod("preprocess")
}

#' @rdname preprocess
#' @export
preprocess.processing <- function(obj,data,...,map=TRUE){

  res <- list()
  if(map){

    for(d in c(1:length(data))){
      data_d <- stats::as.ts(data[[d]])
      if(!is.null(attr(data,"subset"))) attr(data_d,"subset") <- attr(data,"subset")

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

#' @rdname preprocess
#' @export
postprocess <- function(obj,...){
  UseMethod("postprocess")
}

#' @rdname preprocess
#' @export
postprocess.processing <- function(obj,data,...,map=TRUE){

  res <- list()
  if(map){
    for(d in c(1:length(data))){
      data_d <- stats::as.ts(data[[d]])
      if(!is.null(attr(data,"subset"))) attr(data_d,"subset") <- attr(data,"subset")

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
