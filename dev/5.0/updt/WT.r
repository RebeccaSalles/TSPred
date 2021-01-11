#Finds and returns the fittest wavelet  #DO!
#References:
#[1] Day-Ahead Electricity Price Forecasting Using the Wavelet Transform and ARIMA Models, Conejo et al.
#[2] Time series forecasting based on wavelet filtering, Joo and Kim
#[3] A Wavelet Based Prediction Method for Time Series, Stolojescu et al.

#' Automatic wavelet transform
#' 
#' The function automatically applies a maximal overlap discrete wavelet
#' transform to a provided univariate time series. Wrapper function for \code{\link[wavelets]{modwt}}
#' of the \code{wavelets} package. It also allows the automatic selection
#' of the level and filter of the transform using \code{\link{fittestWavelet}}.
#' \code{WaveletT.rev()} reverses the transformation based on the \code{\link[wavelets]{imodwt}} function.
#'
#' @aliases WaveletT WaveletT.rev
#' @param x A numeric vector or univariate time series to be decomposed.
#' @param level An integer specifying the level of the decomposition. If
#' \code{NULL}, it is automatically selected using \code{\link{fittestWavelet}}.
#' @param filter A character string indicating which
#' wavelet filter to use in the decomposition. If \code{NULL}, or a vector and \code{length(filters)>1}, the
#' wavelet transform filter is automatically selected using \code{\link{fittestWavelet}}.
#' @param boundary See \code{\link[wavelets]{modwt}}.
#' @param pred A list containing component series (such as) resulting from wavelet transform (\code{WaveletT()}).
#' @param wt_obj Object of class \code{\link[wavelets]{modwt}} containing the wavelet transformed series.
#' @param ... Additional arguments passed to \code{\link{fittestWavelet}}.
#'
#' @return A list containing each component series resulting from
#' the decomposition of \code{x} (\code{level} wavelet coefficients series and 
#' \code{level} scaling coefficients series).
#' An object of class \code{\link[wavelets]{modwt}} containing the wavelet transformed/decomposed
#' time series is passed as an attribute named "wt_obj".
#' This attribute is passed to \code{wt_obj} in \code{WaveletT.rev()}.
#'
#' @author Rebecca Pontes Salles
#' @family transformation methods
#' @seealso \code{\link{fittestWavelet}}, \code{\link{fittestEMD}}
#' @references A. J. Conejo, M. A. Plazas, R. Espinola, A. B. Molina, Day-ahead
#' electricity price forecasting using the wavelet transform and ARIMA models,
#' IEEE Transactions on Power Systems 20 (2005) 1035-1042.
#' 
#' T. Joo, S. Kim, Time series forecasting based on wavelet filtering, Expert
#' Systems with Applications 42 (2015) 3868-3874.
#' 
#' C. Stolojescu, I. Railean, S. M. P. Lenca, A. Isar, A wavelet based
#' prediction method for time series. In Proceedings of the 2010 International
#' Conference Stochastic Modeling Techniques and Data Analysis, Chania, Greece
#' (pp. 8-11) (2010). %% ~put references to the literature/web site here ~
#' @keywords wavelet decomposition transform
#'
#' @examples
#' 
#' data(CATS)
#' \dontrun{
#' w <- WaveletT(CATS[,1])
#' 
#' #plot wavelet transform/decomposition
#' plot(attr(w,"wt_obj"))
#' 
#' x <- WaveletT.rev(pred=NULL, attr(w,"wt_obj"))
#' 
#' all(round(x,4)==round(CATS[,1],4))
#' }
#'
#' @export WaveletT
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

#' @rdname WaveletT
#' @export WaveletT.rev
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