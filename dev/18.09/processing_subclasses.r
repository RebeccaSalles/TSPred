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


#Subclass WT  #DO
WT <- function(level=NULL,filter=NULL,boundary="periodic",prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = WaveletT, prep_par = c(list(level=level,filter=filter,boundary=boundary),prep_par),
             postp_func = WaveletT.rev, postp_par = c(list(wt_obj=NULL),postp_par),
             method = "Wavelet transform",..., subclass ="WT")
}
run.WT <- function(obj,...,rev=FALSE){
  #get result from run.processing
  results <- NextMethod()
  #browser()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(!rev){
    if(is.null(obj$postp$par$wt_obj)) results <- updt(results, par="wt_obj")
    if(is.null(obj$prep$par$level)) results <- updt(results, par="level", value=results[[1]]$obj$postp$par$wt_obj@level) 
    if(is.null(obj$prep$par$filter)) results <- updt(results, par="filter", value=results[[1]]$obj$postp$par$wt_obj@filter)
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











#============== DO ==============

#Subclass PCT  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass MAS  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass detrend  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass DIF  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass EMD  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass THieF  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass SW  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass minmax  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass zscore  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass AN  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}

#Subclass partition  #DO
LT <- function(){
  processing(prep_func=TSPred::LT, postp_func=TSPred::LT.rev, method="Logarithmic transform", subclass="LT")
}

summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par)){
    cat("\tPre-processing:\n")
    print(obj$prep$par)
  }
  if(!is.null(obj$postp$par)){
    cat("\tPost-processing:\n")
    print(obj$postp$par)
  }
}