#Subclass LT
LT <- function(base = exp(1)){ #::TSPred
  processing(prep_func=LogT, prep_par=list(base=base), 
             postp_func=LogT.rev, postp_par=list(base=base),
             method="Logarithmic transform", subclass="LT")
}
summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tBase: ",obj$prep$par$base,"\n")
}


#Subclass BCT
BCT <- function(lambda=NULL,prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = TSPred::BCT, prep_par = c(list(lambda=lambda),prep_par),
             postp_func = TSPred::BCT.rev, postp_par = c(list(lambda=lambda),postp_par),
             method = "Box-Cox transform",..., subclass ="BCT")
}
run.BCT <- function(obj,...,rev=FALSE){
  #get result from run.processing
  results <- NextMethod()
  
  #if preprocessing with undefined lambda, update value of computed lambda parameter in the BCT object(s)
  if(!rev && is.null(obj$prep$par$lambda)) results <- updt(results, par="lambda")
  
  return(results)
}
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
WT <- function(level=NULL,filter=NULL,boundary="periodic",prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = WaveletT, prep_par = c(list(level=level,filter=filter,boundary=boundary),prep_par),
             postp_func = WaveletT.rev, postp_par = c(list(wt_obj=NULL),postp_par),
             method = "Wavelet transform",..., subclass ="WT")
}
run.WT <- function(obj,...,rev=FALSE){
  #get result from run.processing
  results <- NextMethod()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(!rev){
    if(is.null(obj$postp$par$wt_obj)) results <- updt(results, par="wt_obj")
    if(is.null(obj$prep$par$level)||is.null(obj$prep$par$filter)){
      results <- updt(results, par="level", value=results[[1]][[1]]$obj$postp$par$wt_obj@level) 
      results <- updt(results, par="filter", value=results[[1]][[1]]$obj$postp$par$wt_obj@filter@wt.name)
    }
  }
  
  return(results)
}
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
subsetting <- function(train_perc=0.8, test_len=NULL){
  processing(prep_func = train_test_subset, prep_par = list(train_perc=train_perc,test_len=test_len),
             postp_func = NULL, postp_par = NULL,
             method = "Subsetting data into training and testing sets", subclass ="subsetting")
}
summary.subsetting <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par$test_len))
    cat("\tTesting set length: ",obj$prep$par$test_len,"\n")
  else
    cat("\tTraining set percentage: ",obj$prep$par$train_perc,"\n")
}


#Subclass sw
SW <- function(window_len=NULL){
  processing(prep_func = sw, prep_par = list(k=window_len),
             postp_func = NULL, postp_par = NULL,
             method = "Sliding windows", subclass ="SW")
}
summary.SW <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tWindow length: ",obj$prep$par$k,"\n")
}

#============== DO ==============

#Subclass PCT  #DO
#Subclass MAS  #DO
#Subclass detrend  #DO
#Subclass DIF  #DO
#Subclass EMD  #DO
#Subclass THieF  #DO
#Subclass SW  #DO
#Subclass minmax  #DO
#Subclass zscore  #DO
#Subclass AN  #DO
#Subclass partition  #DO