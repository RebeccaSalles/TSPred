emd <- function(x,level=NULL,filter=c("haar", "d4", "la8", "bl14", "c6"),boundary="periodic",...){
  
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