\name{arimainterp}
\alias{arimainterp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interpolation of unknown values using automatic ARIMA fitting and prediction
%%  ~~function to do ... ~~
}
\description{
The function predicts nonconsecutive blocks of N unknown values of a single time series using the \code{\link{arimapred}} function and an interpolation approach.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arimainterp(TimeSeries, n.ahead, extrap = TRUE, xreg = NULL,
newxreg = NULL, se.fit= FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{A matrix, or data frame which contains a set of time series used for fitting ARIMA models. Each column corresponds to one time series. Each time series in \code{TimeSeries} is assumed to be a sequence of known values of the single time series that intercalates blocks of unknown values. The time series values in column 1 are lagged values of the ones in column 2, and the values in these two columns are assumed to be intercalated by the first block of N unknown values to be predicted. This is also valid for columns 2 and 3, and so forth.
%%     ~~Describe \code{TimeSeries} here~~
}
  \item{n.ahead}{A numeric value (N) with the number of consecutive unknown values of each block which is to be predicted of \code{TimeSeries}, that is, the length of the blocks of N unknown values.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{extrap}{A Boolean parameter which defines whether one of the blocks of N unknown values to be predicted follows the last sequence of known values in \code{TimeSeries}. If \code{extrap} is \code{TRUE}, the last block of N unknown values will be extrapolated from the last time series in \code{TimeSeries}.
%%     ~~Describe \code{extrap} here~~
}
  \item{xreg}{A list of vectors, matrices, data frames or times series of external regressors used for fitting the ARIMA models. The first component of the list contains external regressors for the first time series in \code{TimeSeries} and therefore must have the same number of rows as this respective time series. This is also valid for the second component, and so forth. Ignored if \code{NULL}.
%%     ~~Describe \code{xreg} here~~
}
  \item{newxreg}{A list of vectors, matrices, data frames or times series with further values of \code{xreg} to be used for prediction of the blocks of N unknown values. Each component of the list must have at least \code{n.ahead} rows. Ignored if \code{NULL}.
%%     ~~Describe \code{newxreg} here~~
}
  \item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the predictions are returned.
%%     ~~Describe \code{newxreg} here~~
}
}
\details{
In order to avoid error accumulation, when possible, the function provides the separate prediction of each half of the blocks of unknown values using their past and future known values, respectively. If \code{extrap} is \code{TRUE}, this strategy is not possible for the last of the blocks of unknown values, for whose prediction the function uses only its past values.
By default the function omits any missing values found in \code{TimeSeries}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of time series of predictions, or if \code{se.fit} is \code{TRUE}, a vector of lists, each one with the components \code{pred}, the predictions, and \code{se}, the estimated standard errors. Both components are time series. See the \code{\link{predict.Arima}} function in the stats package and the function \code{\link{arimapred}}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
H. Cheng, P.-N. Tan, J. Gao, and J. Scripps, 2006, "Multistep-Ahead Time Series Prediction", In: W.-K. Ng, M. Kitsuregawa, J. Li, and K. Chang, eds., Advances in Knowledge Discovery and Data Mining,  Springer Berlin Heidelberg, p. 765-774. 
%% ~put references to the literature/web site here ~
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{arimapred}}, \code{\link{marimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }
\keyword{ interpolation }