WaveletT <- function(x,level=NULL,filter=c("haar", "d4", "la8", "bl14", "c6"),boundary="periodic",...){
  
  if(is.null(level)|is.null(filter)|length(filter)==0|length(filter)>1){
    fw <- fittestWavelet(x, filters=filter, n.levels=level,boundary=boundary,...)
    level <- fw$level
    filter <- fw$filter
  }
  
  require(wavelets)
  wt <- wavelets::modwt(x, filter=filter, n.levels=level,boundary=boundary)
  
  decomp_series <- c(wt@W,wt@V)
  attr(decomp_series,"wt_obj") <-  wt
  
  return(decomp_series)
}

WaveletT.rev <- function(pred=NULL,wt_obj){
  require(wavelets)
  
  wt <- wt_obj
  
  if(is.null(pred)) return(wavelets::imodwt(wt))
  else{
    level <- wt@level
    pred_W <- pred[1:level]
    pred_V <- pred[(level+1):(level+level)]
    n.ahead <- length(pred[[1]])
    
    #edits modwt object: adds n.ahead NAs to the end of original time series values
    newseries <- c(wt@series,rep(NA,n.ahead))
    wt@series <- as.matrix(newseries)
    wt@attr.X <- attributes(ts(newseries))
    
    #edits modwt object: adds predictions of wavelet and scaling coefficients for each level of decomposition
    for(l in 1:level){
      wt@W[[l]] <- as.matrix(c(wt@W[[l]],pred_W[[l]]))
      wt@V[[l]] <- as.matrix(c(wt@V[[l]],pred_V[[l]]))
    }
    #inverse maximal overlap discrete wavelet transform
    iwt <- wavelets::imodwt(wt)
    #gets prediction time series
    pred <- ts(tail(iwt,n.ahead),start=(length(iwt)-n.ahead+1))
    
    return(pred)
  }
}