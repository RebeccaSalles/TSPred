emd <- function(x, num_imfs=0, S_number=4L, num_siftings=50L, meaningfulImfs=NULL, h=1, ...){
  #browser()
  require(Rlibeemd)
  emdt <- Rlibeemd::emd(x, num_imfs=num_imfs, S_number=S_number, num_siftings=num_siftings)
  
  nimf <- ncol(emdt)
  
  #meaningful IMF's are by default all of them
  mimfs <- 1:nimf
  
  if(!is.null(meaningfulImfs)){
    if(meaningfulImfs==0){
      femd <- fittestEMD(x, h=h, num_imfs= nimf-1, S_number=S_number, num_siftings=num_siftings,...)
      mimfs <- femd$meaningfulImfs
      mimfs <- as.numeric(substring(mimfs, 1, 1)):nimf
    }
    else mimfs <- meaningfulImfs
  }
  
  #select meaningful IMF's
  emdt <- as.list(emdt[,mimfs])
  
  attr(emdt,"meaningfulImfs") <- mimfs
  attr(emdt,"num_imfs") <-  nimf
  
  return(emdt)
}

emd.rev <- function(pred){
  emd_rev <- 0
  #browser()
  for(p in pred){
    emd_rev <- emd_rev + as.numeric(p)
  }
  
  return(emd_rev)
}