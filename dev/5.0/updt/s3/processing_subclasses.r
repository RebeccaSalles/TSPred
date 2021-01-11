#' Time series transformation methods
#'
#' Constructors for the \code{processing} class representing a time series
#' processing method based on a particular time series transformation.
#'
#' @section Mapping-based nonstationary transformation methods:
#' 	Logarithmic transform. \code{prep_func} set as \code{\link{TSPred::LogT}} 
#'  and \code{postp_func} set as \code{\link{TSPred::LogT.rev}}.
#'
#' @param prep_par List of named parameters required by \code{prep_func}.
#' @param postp_par List of named parameters required by \code{postp_func}.
#' @param base \code{\link{TSPred::LogT}}
#'
#' @aliases processing
#'
#' @return An object of class \code{processing}.
#' @author Rebecca Pontes Salles
#' @family constructors
#' @references R. Salles, K. Belloze, F. Porto, P.H. Gonzalez, and E. Ogasawara. 
#' Nonstationary time series transformation methods: An experimental review.
#' Knowledge-Based Systems, 164:274–291, 2019.
#'
#' @keywords processing transformation preprocessing postprocessing
#' 
#' @rdname LT
#' @export LT
#Subclass LT
LT <- function(base = exp(1)){ #::TSPred
  processing(prep_func=TSPred::LogT, prep_par=list(base=base), 
             postp_func=TSPred::LogT.rev, postp_par=list(base=base),
             method="Logarithmic transform", subclass="LT")
}
#' @export
summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tBase: ",obj$prep$par$base,"\n")
}


#Subclass BCT
#' @rdname LT
#' @section Mapping-based nonstationary transformation methods:
#' 	Box-Cox transform. \code{prep_func} set as \code{\link{TSPred::BCT}} 
#'  and \code{postp_func} set as \code{\link{TSPred::BCT.rev}}.
#' @param lambda See \code{\link{TSPred::BCT}}
#' @export
BCT <- function(lambda=NULL,prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = TSPred::BCT, prep_par = c(list(lambda=lambda),prep_par),
             postp_func = TSPred::BCT.rev, postp_par = c(list(lambda=lambda),postp_par),
             method = "Box-Cox transform",..., subclass ="BCT")
}
#' @export
preprocess.BCT <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined lambda, update value of computed lambda parameter in the BCT object(s)
  if(is.null(obj$prep$par$lambda)) results <- updt(results, par="lambda")
  
  return(results)
}
#' @export
summary.BCT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tLambda: ",obj$prep$par$lambda,"\n")
  if(length(obj$prep$par)>1){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-1])
  }
}


#Subclass WT  #::TSPred
#' @rdname LT
#' @section Splitting-based nonstationary transformation methods:
#' 	Wavelet transform. \code{prep_func} set as \code{\link{TSPred::WaveletT}} 
#'  and \code{postp_func} set as \code{\link{TSPred::WaveletT.rev}}.
#' @param level See \code{\link{TSPred::WaveletT}}
#' @param filter See \code{\link{TSPred::WaveletT}}
#' @param boundary See \code{\link{TSPred::WaveletT}}
#' @export
WT <- function(level=NULL,filter=NULL,boundary="periodic",prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = TSPred::WaveletT, prep_par = c(list(level=level,filter=filter,boundary=boundary),prep_par),
             postp_func = TSPred::WaveletT.rev, postp_par = c(list(wt_obj=NULL),postp_par),
             method = "Wavelet transform",..., subclass ="WT")
}
#' @export
preprocess.WT <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(is.null(obj$postp$par$wt_obj)) results <- updt(results, par="wt_obj")
  if(is.null(obj$prep$par$level)||is.null(obj$prep$par$filter)){
    results <- updt(results, par="level", value=results[[1]][[1]]$obj$postp$par$wt_obj@level) 
    results <- updt(results, par="filter", value=results[[1]][[1]]$obj$postp$par$wt_obj@filter@wt.name)
  }
  
  return(results)
}
#' @export
postprocess.WT <- function(obj,...){
  NextMethod(obj,...,map=FALSE)
}
#' @export
summary.WT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tLevel: ",obj$prep$par$level,"\n")
  cat("\tFilter: ",obj$prep$par$filter,"\n")
  cat("\tBoundary: ",obj$prep$par$boundary,"\n")
  if(length(obj$prep$par)>3){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-(1:3)])
  }
}


#Subclass subset
#' @rdname LT
#' @section Data subsetting methods:
#' 	Subsetting data into training and testing sets. \code{prep_func} set as \code{\link{TSPred::train_test_subset}} 
#'  and \code{postp_func} set to \code{NULL}.
#' @param train_perc See \code{\link{TSPred::train_test_subset}}
#' @param test_len See \code{\link{TSPred::train_test_subset}}
#' @export
subsetting <- function(train_perc=0.8, test_len=NULL){
  processing(prep_func = TSPred::train_test_subset, prep_par = list(train_perc=train_perc,test_len=test_len),
             postp_func = NULL, postp_par = NULL,
             method = "Subsetting data into training and testing sets", subclass ="subsetting")
}
#' @export
summary.subsetting <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par$test_len))
    cat("\tTesting set length: ",obj$prep$par$test_len,"\n")
  else
    cat("\tTraining set percentage: ",obj$prep$par$train_perc,"\n")
}


#Subclass sw
#' @rdname LT
#' @section Data subsetting methods:
#' 	Sliding windows. \code{prep_func} set as \code{\link{TSPred::sw}} 
#'  and \code{postp_func} set to \code{NULL}.
#' @param window_len See \code{\link{TSPred::sw}}
#' @export
SW <- function(window_len=NULL){
  processing(prep_func = TSPred::sw, prep_par = list(k=window_len),
             postp_func = NULL, postp_par = NULL,
             method = "Sliding windows", subclass ="SW")
}
#' @export
preprocess.SW <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$k-1), data[[1]] )
  
  NextMethod(obj,data,...,map=map)
}
#' @export
is.SW <- function(sw_obj){
  is(sw_obj,"SW")
}
#' @export
summary.SW <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par))  cat("Parameters:\n")
  cat("\tWindow length: ",obj$prep$par$k,"\n")
}


#Subclass NAS
#' @rdname LT
#' @section Methods for handling missing values:
#' 	Missing values treatment. \code{prep_func} set as parameter \code{na.action}
#'  and \code{postp_func} set to \code{NULL}.
#' @param na.action Function for handling missing values in time series data
#' @export
NAS <- function(na.action=na.omit,prep_par=NULL){
  processing(prep_func = na.action, prep_par = c(list(prep_par)),
             postp_func = NULL, postp_par = NULL,
             method = "Missing values treatment", subclass ="NAS")
}
#' @export
summary.NAS <- function(obj,...){
  NextMethod()
  cat("\tFunction: ",as.character(substitute(obj$prep$func)),"\n")
  if(!is.null(obj$prep$par) && (length(obj$prep$par)>0)){
    cat("Parameters:\n")
    print(obj$prep$par)
  }
}


#Subclass minmax
#' @rdname LT
#' @section Normalization methods:
#' 	MinMax normalization. \code{prep_func} set as \code{\link{TSPred::minmax}} 
#'  and \code{postp_func} set to \code{\link{TSPred::minmax.rev}}.
#' @param min See \code{\link{TSPred::minmax}}
#' @param max See \code{\link{TSPred::minmax}}
#' @param byRow See \code{\link{TSPred::minmax}}
#' @export
MinMax <- function(min=NULL,max=NULL,byRow=TRUE){
  processing(prep_func = TSPred::minmax, prep_par = list(min=min,max=max,byRow=byRow),
             postp_func = TSPred::minmax.rev, postp_par = list(min=min,max=max),
             method = "MinMax normalization", subclass ="MinMax")
}
#' @export
preprocess.MinMax <- function(obj,...){
  if(obj$prep$par$byRow) obj$prep$par$min <- obj$prep$par$max <- NA
  
  results <- NextMethod()
  
  if(is.null(obj$prep$par$min) || obj$prep$par$byRow) results <- updt(results, par="min")
  if(is.null(obj$prep$par$max) || obj$prep$par$byRow) results <- updt(results, par="max")
  
  return(results)
}
#' @export
summary.MinMax <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tMin: ",obj$prep$par$min,"\n")
  cat("\tMax: ",obj$prep$par$max,"\n")
}


#Subclass AN
#' @rdname LT
#' @section Normalization methods:
#' 	Adaptive normalization. \code{prep_func} set as \code{\link{TSPred::an}} 
#'  and \code{postp_func} set to \code{\link{TSPred::an.rev}}.
#' @param min See \code{\link{TSPred::an}}
#' @param max See \code{\link{TSPred::an}}
#' @param byRow See \code{\link{TSPred::an}}
#' @param outlier.rm See \code{\link{TSPred::an}}
#' @param alpha See \code{\link{TSPred::an}}
#' @export
AN <- function(min=NULL,max=NULL,byRow=TRUE,outlier.rm=TRUE,alpha=1.5){
  processing(prep_func = TSPred::an, prep_par = list(min=min,max=max,byRow=byRow,outlier.rm=outlier.rm,alpha=alpha),
             postp_func = TSPred::an.rev, postp_par = list(min=min,max=max,an=NULL),
             method = "Adaptive normalization", subclass ="AN")
}
#' @export
preprocess.AN <- function(obj,...){
  if(obj$prep$par$byRow) obj$prep$par$min <- obj$prep$par$max <- NA
  
  results <- NextMethod()
  
  if(is.null(obj$prep$par$min) || obj$prep$par$byRow) results <- updt(results, par="min")
  if(is.null(obj$prep$par$max) || obj$prep$par$byRow) results <- updt(results, par="max")
  results <- updt(results, par="an")
  
  return(results)
}
#' @export
summary.AN <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tMin: ",obj$prep$par$min,"\n")
  cat("\tMax: ",obj$prep$par$max,"\n")
  cat("\tMeans: ",obj$postp$par$max,"\n")
}

#Subclass DIFF
#' @rdname LT
#' @section Mapping-based nonstationary transformation methods:
#' 	Differencing. \code{prep_func} set as \code{\link{TSPred::diff}} 
#'  and \code{postp_func} set as \code{\link{TSPred::diff.rev}}.
#' @param lag See \code{\link{TSPred::diff}}
#' @param differences See \code{\link{TSPred::diff}}
#' @param type See \code{\link{TSPred::diff}}
#' @export
DIFF <- function(lag=NULL, differences=NULL, type="simple",postp_par=list(addinit=FALSE)){
  processing(prep_func = TSPred::diff, prep_par = list(lag=lag,differences=differences,type=type),
             postp_func = TSPred::diff.rev, postp_par = c(list(lag=lag,differences=differences,type=type,xi=NULL),postp_par),
             method = "Differencing", subclass ="DIFF")
}
#' @export
preprocess.DIFF <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$lag*obj$prep$par$differences), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(is.null(obj$prep$par$lag)) results <- updt(results, par="lag")
  if(is.null(obj$prep$par$differences)) results <- updt(results, par="differences")
  if(is.null(obj$prep$par$type)) results <- updt(results, par="type")
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}
#' @export
summary.DIFF <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tType: ",obj$postp$par$type,"\n")
  cat("\tLag: ",obj$prep$par$lag,"\n")
  cat("\tDifferences: ",obj$prep$par$differences,"\n")
}


#Subclass MAS
#' @rdname LT
#' @section Mapping-based nonstationary transformation methods:
#' 	Moving average smoothing. \code{prep_func} set as \code{\link{TSPred::mas}} 
#'  and \code{postp_func} set as \code{\link{TSPred::mas.rev}}.
#' @param order See \code{\link{TSPred::mas}}
#' @export
MAS <- function(order=NULL,prep_par=NULL,postp_par=list(addinit=FALSE)){
  processing(prep_func = TSPred::mas, prep_par = c(list(order=order),prep_par),
             postp_func = TSPred::mas.rev, postp_par = c(list(order=order,xi=NULL),postp_par),
             method = "Moving average smoothing", subclass ="MAS")
}
#' @export
preprocess.MAS <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$order-1), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(is.null(obj$prep$par$order)) results <- updt(results, par="order")
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}
#' @export
summary.MAS <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tOrder: ",obj$postp$par$order,"\n")
}


#Subclass PCT
#' @rdname LT
#' @section Mapping-based nonstationary transformation methods:
#' 	Percentage change transform. \code{prep_func} set as \code{\link{TSPred::pct}} 
#'  and \code{postp_func} set as \code{\link{TSPred::pct.rev}}.
#' @export
PCT <- function(postp_par=NULL){
  processing(prep_func = TSPred::pct, prep_par = NULL,
             postp_func = TSPred::pct.rev, postp_par = c(list(xi=NULL),postp_par),
             method = "Percentage change transform", subclass ="PCT")
}
#' @export
preprocess.PCT <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),1), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}

#Subclass EMD  #::TSPred
#' @rdname LT
#' @section Splitting-based nonstationary transformation methods:
#' 	Empirical mode decomposition. \code{prep_func} set as \code{\link{TSPred::emd}} 
#'  and \code{postp_func} set as \code{\link{TSPred::emd.rev}}.
#' @param num_imfs See \code{\link{TSPred::emd}}
#' @param meaningfulImfs See \code{\link{TSPred::emd}}
#' @export
EMD <- function(num_imfs=0,meaningfulImfs=NULL,prep_par=NULL){
  processing(prep_func = TSPred::emd, prep_par = c(list(num_imfs=num_imfs,meaningfulImfs=meaningfulImfs),prep_par),
             postp_func = TSPred::emd.rev, postp_par = NULL,
             method = "Empirical mode decomposition", subclass ="EMD")
}
#' @export
preprocess.EMD <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(is.null(obj$prep$par$num_imfs)||obj$prep$par$num_imfs==0) results <- updt(results, par="num_imfs")
  if(is.null(obj$prep$par$meaningfulImfs)||obj$prep$par$meaningfulImfs==0) results <- updt(results, par="meaningfulImfs")
  
  return(results)
}
#' @export
postprocess.EMD <- function(obj,...){
  NextMethod(obj,...,map=FALSE)
}
#' @export
summary.EMD <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tNumber of IMF's: ",obj$prep$par$num_imfs,"\n")
  cat("\tMeaningful IMF's: ",obj$prep$par$meaningfulImfs,"\n")
  if(length(obj$prep$par)>2){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-(1:2)])
  }
}

#============== DO ==============
#Subclass detrend  #DO
#Subclass THieF  #DO
#Subclass zscore  #DO