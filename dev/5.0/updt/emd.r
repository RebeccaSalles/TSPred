#Finds and returns the fittest EMD #DO!
#References:
#[1] A Hilbert-Huang transform approach for predicting cyber-attacks, Kim et al.

#' Automatic empirical mode decomposition
#' 
#' The function automatically applies an empirical mode decomposition to a
#' provided univariate time series. Wrapper function for \code{\link[Rlibeemd]{emd}}
#' of the \code{Rlibeemd} package. It also allows the automatic selection
#' of meaningful IMFs using \code{\link{fittestEMD}}.
#' \code{emd.rev()} reverses the transformation.
#' 
#' @aliases emd emd.rev
#' @param x A numeric vector or univariate time series to be decomposed.
#' @param num_imfs Number of Intrinsic Mode Functions (IMFs) to compute. See \code{\link[Rlibeemd]{emd}}.
#' @param S_number,num_siftings See \code{\link[Rlibeemd]{emd}}.
#' @param meaningfulImfs Vector indicating the indices of the meaningful IMFs according to the
#' possible intervals \code{i:num_imfs} for \code{i=1,...,(num_imfs-1)}, where
#' \code{num_imfs} is the number of IMFs in a decomposition.
#' If \code{meaningfulImfs = NULL} (default), the function returns all IMF's produced by \code{\link[Rlibeemd]{emd}} as meaningful.
#' If \code{meaningfulImfs = 0} the function automatically selects the meaningful IMFs of
#' a decomposition using \code{\link{fittestEMD}}.
#' @param h See \code{\link{fittestEMD}}. Passed to \code{\link{fittestEMD}} if \code{meaningfulImfs = 0}.
#' @param pred A list containing IMFs produced by empirical mode decomposition.
#' @param ... Additional arguments passed to \code{\link{fittestEMD}}.
#'
#' @return A list containing the meaningful IMFs of the empirical mode decomposition of \code{x}.
#' A vector indicating the indices of the meaningful IMFs and the number of IMFs produced are passed as attributes
#' named "meaningfulImfs" and "num_imfs", respectively.
#'
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @seealso \code{\link{fittestEMD}}, \code{\link{fittestWavelet}}
#' @references Kim, D., Paek, S. H., & Oh, H. S. (2008). A Hilbert-Huang
#' transform approach for predicting cyber-attacks. Journal of the Korean
#' Statistical Society, 37(3), 277-283. %% ~put references to the
#' literature/web site here ~
#' 
#' @keywords emd decomposition transform meaningful Imfs
#'
#' @examples
#' 
#' data(CATS)
#' e <- emd(CATS[,1])
#' x <- emd.rev(e)
#' all(round(x,4)==round(CATS[,1],4))
#'
#' @export emd
emd <- function(x, num_imfs=0, S_number=4L, num_siftings=50L, meaningfulImfs=NULL, h=1,...){
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

#' @rdname emd
#' @export emd.rev
emd.rev <- function(pred){
  emd_rev <- 0
  #browser()
  for(p in pred){
    emd_rev <- emd_rev + as.numeric(p)
  }
  
  return(emd_rev)
}