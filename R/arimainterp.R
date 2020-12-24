#' Interpolation of unknown values using automatic ARIMA fitting and prediction
#' 
#' The function predicts nonconsecutive blocks of N unknown values of a single
#' time series using the \code{\link{arimapred}} function and an interpolation
#' approach.
#' 
#' In order to avoid error accumulation, when possible, the function provides
#' the separate prediction of each half of the blocks of unknown values using
#' their past and future known values, respectively. If \code{extrap} is
#' \code{TRUE}, this strategy is not possible for the last of the blocks of
#' unknown values, for whose prediction the function uses only its past values.
#' By default the function omits any missing values found in \code{TimeSeries}.
#' 
#' @param TimeSeries A matrix, or data frame which contains a set of time
#' series used for fitting ARIMA models. Each column corresponds to one time
#' series. Each time series in \code{TimeSeries} is assumed to be a sequence of
#' known values of the single time series that intercalates blocks of unknown
#' values. The time series values in column 1 are lagged values of the ones in
#' column 2, and the values in these two columns are assumed to be intercalated
#' by the first block of N unknown values to be predicted. This is also valid
#' for columns 2 and 3, and so forth.
#' @param n.ahead A numeric value (N) with the number of consecutive unknown
#' values of each block which is to be predicted of \code{TimeSeries}, that is,
#' the length of the blocks of N unknown values.
#' @param extrap A Boolean parameter which defines whether one of the blocks of
#' N unknown values to be predicted follows the last sequence of known values
#' in \code{TimeSeries}. If \code{extrap} is \code{TRUE}, the last block of N
#' unknown values will be extrapolated from the last time series in
#' \code{TimeSeries}.
#' @param xreg A list of vectors, matrices, data frames or times series of
#' external regressors used for fitting the ARIMA models. The first component
#' of the list contains external regressors for the first time series in
#' \code{TimeSeries} and therefore must have the same number of rows as this
#' respective time series. This is also valid for the second component, and so
#' forth. Ignored if \code{NULL}.
#' @param newxreg A list of vectors, matrices, data frames or times series with
#' further values of \code{xreg} to be used for prediction of the blocks of N
#' unknown values. Each component of the list must have at least \code{n.ahead}
#' rows. Ignored if \code{NULL}.
#' @param se.fit If \code{se.fit} is \code{TRUE}, the standard errors of the
#' predictions are returned.
#' @return A vector of time series of predictions, or if \code{se.fit} is
#' \code{TRUE}, a vector of lists, each one with the components \code{pred},
#' the predictions, and \code{se}, the estimated standard errors. Both
#' components are time series. See the \code{\link{predict.Arima}} function in
#' the stats package and the function \code{\link{arimapred}}.
#' @author Rebecca Pontes Salles
#' @seealso \code{\link{arimapred}}, \code{\link{marimapred}}
#' @references H. Cheng, P.-N. Tan, J. Gao, and J. Scripps, 2006,
#' "Multistep-Ahead Time Series Prediction", In: W.-K. Ng, M. Kitsuregawa, J.
#' Li, and K. Chang, eds., Advances in Knowledge Discovery and Data Mining,
#' Springer Berlin Heidelberg, p. 765-774.
#' @keywords ARIMA automatic fitting adjustment prediction interpolation
#' @examples
#' 
#' \donttest{
#' data(CATS)
#' arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)
#' }
#' 
#' @export arimainterp
arimainterp <-
function(TimeSeries,n.ahead,extrap=TRUE,xreg=NULL,newxreg=NULL,se.fit=FALSE){
    if(is.null(TimeSeries) | ncol(TimeSeries)<2) stop("TimeSeries is required and must have positive length with at least 2 columns")
    if(is.null(n.ahead)) stop("The number of values to be predicted is unknown")
    
	FrstBlocksCols <- c(1:(ncol(TimeSeries)-1))
    LstBlocksCols <- c(2:(ncol(TimeSeries)))
    
    FrstBlocks <- data.frame(TimeSeries[,FrstBlocksCols])
	RevLstBlocks <- data.frame(TimeSeries[order(nrow(TimeSeries):1),LstBlocksCols])
	
	N1 <- ceiling(n.ahead/2)
	N2 <- floor(n.ahead/2)
    
	namescol <- NULL
	for (i in FrstBlocksCols) namescol[i] <- paste('Block',i)	
	
	reg <- xreg
	newreg <- newxreg
	if(!is.null(xreg)) reg <- data.frame(xreg[FrstBlocksCols])
	if(!is.null(newxreg)) newreg <- mapply(utils::head,data.frame(newxreg[FrstBlocksCols]),N1)
	FrstBlocksPredictions <- marimapred(FrstBlocks,n.ahead=N1,na.action=stats::na.omit,xreg=reg,newxreg=newreg,se.fit=se.fit)
	
	if(!is.null(xreg)) reg <- sapply(data.frame(xreg[LstBlocksCols]),rev)
	if(!is.null(newxreg)) newreg <- apply( data.frame(mapply(utils::tail,data.frame(newxreg[FrstBlocksCols]),N2)) ,2,rev)
	RevFrstBlocksPredictions <- marimapred(RevLstBlocks,n.ahead=N2,na.action=stats::na.omit,xreg=reg,newxreg=newreg,se.fit=se.fit)
	
	RevFrstBlocksPredictions <- apply(RevFrstBlocksPredictions,2,rev)

    InterpPredictions <- rbind(FrstBlocksPredictions,RevFrstBlocksPredictions)
    colnames(InterpPredictions) <- namescol
	
    #Extrapolation phase
	if(extrap){		
		LstBlock <- TimeSeries[ncol(TimeSeries)]
		
		reg <- xreg
		newreg <- newxreg
		if(!is.null(xreg)) reg <- xreg[ncol(TimeSeries)]
		if(!is.null(newxreg)) newreg <- newxreg[ncol(TimeSeries)]

		LstBlockExtrapPredictions <- marimapred(LstBlock,n.ahead=n.ahead,na.action=stats::na.omit,xreg=reg,newxreg=newreg,se.fit=se.fit)

		InterpPredictions <- cbind(InterpPredictions,LstBlockExtrapPredictions)
		colnames(InterpPredictions) <- c(namescol, paste('Block',length(namescol)+1))
	}

    return (InterpPredictions)
}
