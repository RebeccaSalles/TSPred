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

#' Prediction/modeling quality evaluation
#'
#' Constructor for the \code{evaluating} class representing a time series prediction
#' or modeling fitness quality evaluation based on a particular metric.
#' The \code{evaluating} class has two specialized subclasses \code{fitness} and 
#' \code{error} reagarding fitness criteria and prediction/modeling error metrics, respectively.
#'
#' @aliases evaluating fitness error
#' @param eval_func A function for computing a particular metric.
#' @param eval_par List of named parameters required by \code{eval_func}.
#' @param ... Other parameters to be encapsulated in the class object.
#' @param subclass Name of new specialized subclass object created in case it is provided. 
#'
#' @return An object of class \code{evaluating}. A list usually containing at least
#' the following elements: \item{func}{A function for computing a particular metric.}
#' \item{par}{Particular parameters required by \code{func}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords quality evaluation metric
#' @examples
#' 
#' e <- error(eval_func=TSPred::NMSE, eval_par=list(train.actual=NULL), method="Normalised Mean Squared Error", subclass="NMSE")
#' summary(e)
#'
#' f <- fitness(eval_func=stats::AIC, method="Akaike's Information Criterion", subclass="AIC")
#' summary(f)
#' 
#' @export evaluating
evaluating <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  validate_evaluating(new_evaluating(eval_func, eval_par, ..., subclass=subclass))
}

#Subclass fitness
#' @rdname evaluating
#' @export
fitness <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  evaluating(eval_func=eval_func, 
             eval_par=eval_par,
             ...,
             subclass=c(subclass,"fitness"))
}

#Subclass error
#' @rdname evaluating
#' @export
error <- function(eval_func, eval_par=NULL, ..., subclass=NULL){
  evaluating(eval_func=eval_func, 
             eval_par=eval_par,
             ...,
             subclass=c(subclass,"error"))
}

#' @export
summary.evaluating <- function(obj,...){
  cat("Evaluating class object\n")
  
  if(is.null(obj$method)) cat("Method: Description not provided\n")
  else cat("Method: ",obj$method,"\n")
  
  if(is.null(obj$par)) cat("Parameters: N/A\n")
}

#' @export
is.evaluating <- function(obj){
  is(obj,"evaluating")
}

#' @export
is.fitness <- function(fitness_obj){
  is(fitness_obj,"fitness")
}

#' @export
is.error <- function(error_obj){
  is(error_obj,"error")
}

#' Evaluating prediction/modeling quality
#'
#' \code{evaluate} is a generic function for evaluating the quality of time series prediction
#' or modeling fitness based on a particular metric defined in an \code{\link{evaluating}} object.
#' The function invokes particular \emph{methods} which
#' depend on the class of the first argument.
#'
#' @aliases evaluate
#' @param obj An object of class \code{\link{evaluating}} defining a particular metric.
#' @param mdl A time series model object for which fitness is to be evaluated.
#' @param test A vector or univariate time series containing actual values
#' for a time series that are to be compared against \code{pred}.
#' @param pred A vector or univariate time series containing time series
#' predictions that are to be compared against the values in \code{test}.
#' @param ... Other parameters passed to \code{eval_func} of \code{obj}.
#' @param fitness Should the function compute the fitness quality? If \code{TRUE} the function
#' uses \code{mdl} to compute fitness error, otherwise, it uses \code{test} and 
#' \code{pred} to compute prediction error.
#'
#' For \code{evaluate.fitness}, \code{test} and \code{pred} are ignored and can be set to \code{NULL}.
#' For \code{evaluate.error}, \code{mdl} is ignored if \code{fitness} is \code{FALSE}, otherwise,
#' \code{test} and \code{pred} are ignored and can be set to \code{NULL}.
#'
#' @return A list containing \code{obj} and the computed metric values.
#' @author Rebecca Pontes Salles
#' @family evaluate
#'
#' @keywords quality evaluation metric
#' @examples
#' data(CATS,CATS.cont)
#' mdl <- forecast::auto.arima(CATS[,1])
#' pred <- forecast::forecast(mdl, h=length(CATS.cont[,1]))
#'
#' evaluate(MSE, test=CATS.cont[,1], pred=pred)
#' evaluate(MSE, mdl, fitness=TRUE)
#' evaluate(AIC, mdl)
#' 
#' @export evaluate
evaluate <- function(obj,...){
  UseMethod("evaluate")
}

#' @rdname evaluate
#' @export
evaluate.evaluating <- function(obj, test, pred, ...){
  result <- do.call(obj$func,c(list(test),list(pred),list(...),obj$par))
  attr(result,"name") <- attr(test,"name")
  res <- list(result(obj,result))

  return(results(res))
}

#' @rdname evaluate
#' @export
evaluate.fitness <- function(obj, mdl, test=NULL, pred=NULL, ...){
  result <- do.call(obj$func,c(list(mdl),obj$par))
  attr(result,"name") <- attr(mdl,"name")
  res <- list(result(obj,result))
  
  return(results(res))
}

#' @rdname evaluate
#' @export
evaluate.error <- function(obj, mdl=NULL, test=NULL, pred=NULL, ..., fitness=FALSE){
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