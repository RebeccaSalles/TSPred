\name{arimapred}
\alias{arimapred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic ARIMA fitting and prediction
%%  ~~function to do ... ~~
}
\description{
The function predicts and returns the next n consecutive values of a time series using an automatically fitted ARIMA model. It may also plot the predicted values against the actual ones using the function \code{\link{plotarimapred}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arimapred(timeseries, timeseries.cont = NULL, n.ahead = NULL, na.action = na.omit,
xreg = NULL, newxreg = NULL, se.fit = FALSE, plot = FALSE,
range.p = 0.2, ylab = NULL, xlab = NULL, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting an ARIMA model.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.cont}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. Ignored if \code{NULL}.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{n.ahead}{Number of consecutive values of the time series, which are to be predicted. If \code{n.ahead} is \code{NULL}, the number of consecutive values to be predicted is assumed to be equal to the number of rows in \code{timeseries.cont}. Required when \code{timeseries.cont} is \code{NULL}.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.cont}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.cont}.
%%     ~~Describe \code{na.action} here~~
}
  \item{xreg}{A vector, matrix, data frame or times series of external regressors used for fitting the ARIMA model.  It must have the same number of rows as \code{timeseries}. Ignored if \code{NULL}.
%%     ~~Describe \code{xreg} here~~
}
  \item{newxreg}{A vector, matrix, data frame or times series with new values of \code{xreg} to be used for prediction. Must have at least \code{n.ahead} rows or the number of rows in \code{timeseries.cont}. Ignored if \code{NULL}.
%%     ~~Describe \code{newxreg} here~~
}
  \item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the predictions are returned.
%%     ~~Describe \code{newxreg} here~~
}
  \item{plot}{If \code{plot} is \code{TRUE}, the function will generate a graphic of the predicted values against the actual ones in \code{timeseries.cont}.
%%     ~~Describe \code{plot} here~~
}
  \item{range.p}{A percentage which defines how much the range of the graphic's y-axis will be increased from the minimum limits imposed by data.
%%     ~~Describe \code{range.p} here~~
}
  \item{ylab}{A title for the graphic's y-axis. Ignored if \code{NULL}.
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{A title for the graphic's x-axis. Ignored if \code{NULL}.
%%     ~~Describe \code{xlab} here~~
}
  \item{main}{An overall title for the graphic. Ignored if \code{NULL}.
%%     ~~Describe \code{main} here~~
}
}
\details{The ARIMA model used for time series prediction is automatically fitted by the \code{\link{auto.arima}} function in the \code{forecast} package. In order to avoid drift errors, the function introduces an auxiliary regressor whose values are a sequence of consecutive integer numbers starting from 1. The fitted ARIMA model is used for prediction by the \code{\link{predict.Arima}} function in the \code{stats} package.
For more details, see the \code{\link{auto.arima}} function in the \code{forecast} package and the \code{\link{predict.Arima}} function in the stats package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A time series of predictions, or if \code{se.fit} is \code{TRUE}, a list with the components \code{pred}, the predictions, and \code{se}, the estimated standard errors. Both components are time series. See the \code{\link{predict.Arima}} function in the stats package.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting: principles and practice. OTexts.

R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
%% ~put references to the literature/web site here ~
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{auto.arima}}, \code{\link{predict.Arima}}, \code{\link{plotarimapred}}, \code{\link{marimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A,SantaFe.A.cont)
arimapred(SantaFe.A[,1],SantaFe.A.cont[,1])
arimapred(SantaFe.A[,1],n.ahead=100)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%## The function is currently defined as
%%function (timeseries, timeseries.cont = NULL, n.ahead = NULL, 
%%    na.action = na.omit, xreg = NULL, newxreg = NULL, se.fit = FALSE, plot = FALSE, 
%%    range.p = 0.2, ylab = NULL, xlab = NULL, main = NULL) 
%%{
%%    if (is.null(timeseries)) 
%%        stop("timeseries is required")
%%    if (is.null(timeseries.cont) & is.null(n.ahead)) 
%%        stop("the number of values to be forecasted is unknown")
%%    ts <- ts(na.action(timeseries))
%%    N <- length(timeseries.cont)
%%    if (!is.null(n.ahead)) 
%%        N <- n.ahead
%%    nobs <- length(ts)
%%    i <- nobs + 1
%%    f <- nobs + N
%%    reg <- cbind(1:nobs, xreg)
%%    fit <- auto.arima(ts, xreg = ts(reg, start = 1))
%%    newreg <- cbind(i:f, newxreg)
%%    pred <- predict(fit, n.ahead = N, newxreg = ts(newreg, start = i), 
%%        se.fit=se.fit)
%%    if (!is.null(timeseries.cont) & plot) {
%%        ts.cont <- ts(timeseries.cont, start = i)
%%        plotarimapred(ts.cont, fit, xlimI = i, xlimF = f, range.p, 
%%            xreg = newreg, ylab = ylab, xlab = xlab, main = main)
%%    }
%%    return(pred)
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }