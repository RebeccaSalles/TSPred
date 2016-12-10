\name{marimapred}
\alias{marimapred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple time series automatic ARIMA fitting and prediction
%%  ~~function to do ... ~~
}
\description{
The function predicts and returns the next n consecutive values of a set of time series using automatically fitted ARIMA models. Based on multiple application of the \code{\link{arimapred}} function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
marimapred(TimeSeries, TimeSeriesCont = NULL, n.ahead = NULL, na.action = na.omit,
xreg = NULL, newxreg = NULL, se.fit = FALSE, plot = FALSE,
range.p = 0.2, ylab = NULL, xlab = NULL, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{A vector, matrix, or data frame which contains a set of time series used for fitting ARIMA models. Each column corresponds to one time series.
%%     ~~Describe \code{TimeSeries} here~~
}
  \item{TimeSeriesCont}{A vector, matrix, or data frame containing continuation points for \code{TimeSeries} with actual values. Each column corresponds to one time series. Ignored if \code{NULL}.
%%     ~~Describe \code{TimeSeriesCont} here~~
}
  \item{n.ahead}{A numeric vector (or a single numeric value) with the number of consecutive values which are to be predicted of each respective time series in \code{TimeSeries}. If \code{n.ahead} is \code{NULL}, the number of values to be predicted of each time series in \code{TimeSeries} is assumed to be equal to the number of rows in each respective time series in \code{TimeSeriesCont}. Required when \code{TimeSeriesCont} is \code{NULL}.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{na.action}{A function for treating missing values in \code{TimeSeries} and \code{TimeSeriesCont}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{TimeSeries} or \code{TimeSeriesCont}.
%%     ~~Describe \code{na.action} here~~
}
  \item{xreg}{A list of vectors, matrices, data frames or times series of external regressors used for fitting the ARIMA models. The first component of the list contains external regressors for the first time series in \code{TimeSeries} and therefore must have the same number of rows as this respective time series. This is also valid for the second component, and so forth. Ignored if \code{NULL}.
%%     ~~Describe \code{xreg} here~~
}
  \item{newxreg}{A list of vectors, matrices, data frames or times series with new values of \code{xreg} to be used for prediction. The first component of the list must have at least the same number of rows as the respective first value in \code{n.ahead} or, if \code{n.ahead} is \code{NULL}, the number of continuation points in the respective first time series in \code{TimeSeriesCont}. This is also valid for the second component, and so forth. Ignored if \code{NULL}.
%%     ~~Describe \code{newxreg} here~~
}
  \item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the predictions are returned.
%%     ~~Describe \code{newxreg} here~~
}
  \item{plot}{A Boolean parameter which defines whether the function \code{\link{arimapred}} will generate a graphic. If \code{plot} is \code{TRUE},  graphics will be generated for each time series in \code{TimeSeries}.
%%     ~~Describe \code{plot} here~~
}
  \item{range.p}{A percentage which defines how much the range of the graphics' y-axis will be increased from the minimum limits imposed by data.
%%     ~~Describe \code{range.p} here~~
}
  \item{ylab}{A title for the graphics' y-axis. Ignored if \code{NULL}.
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{A title for the graphics' x-axis. Ignored if \code{NULL}.
%%     ~~Describe \code{xlab} here~~
}
  \item{main}{An overall title for the graphics. Ignored if \code{NULL}.
%%     ~~Describe \code{main} here~~
}
}
\details{ See the \code{\link{arimapred}} function.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of time series of predictions, if the number of consecutive values predicted of each time series in \code{TimeSeries} is the same, otherwise a list of time series of predictions.

If \code{se.fit} is \code{TRUE}, a vector of lists, each one with the components \code{pred}, the predictions, and \code{se}, the estimated standard errors. Both components are time series. See the \code{\link{predict.Arima}} function in the stats package and the function \code{\link{arimapred}}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{See the \code{\link{arimapred}} function.
%% ~put references to the literature/web site here ~
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{arimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A,SantaFe.A.cont)
marimapred(SantaFe.A,SantaFe.A.cont)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%## The function is currently defined as
%%function (TimeSeries, TimeSeriesCont = NULL, n.ahead = NULL, 
%%    na.action = na.omit, xreg = NULL, newxreg = NULL, plot = FALSE, 
%%    range.p = 0.2, ylab = NULL, xlab = NULL, main = NULL) 
%%{
%%    if (!is.null(TimeSeriesCont)) {
%%        if (!is.null(ylab)) {
%%            Predictions <- mapply(arimapred, TimeSeries, 
%%                TimeSeriesCont, MoreArgs = list(xreg = xreg, 
%%                  newxreg = newxreg, se.fit = se.fit, n.ahead = n.ahead, na.action = na.action, 
%%                  range.p = range.p, plot = plot, ylab = ylab, 
%%                  xlab = xlab, main = main), SIMPLIFY = TRUE, 
%%                USE.NAMES = TRUE)
%%        }
%%        else {
%%            Predictions <- mapply(arimapred, TimeSeries, 
%%                TimeSeriesCont, ylab = colnames(TimeSeries), 
%%                MoreArgs = list(xreg = xreg, newxreg = newxreg, se.fit = se.fit, 
%%                  n.ahead = n.ahead, na.action = na.action, range.p = range.p, 
%%                  plot = plot, xlab = xlab, main = main), SIMPLIFY = TRUE, 
%%                USE.NAMES = TRUE)
%%        }
%%    }
%%    else {
%%        if (!is.null(ylab)) {
%%            Predictions <- mapply(arimapred, TimeSeries, 
%%                MoreArgs = list(timeseries.cont = TimeSeriesCont, 
%%                  xreg = xreg, newxreg = newxreg, se.fit = se.fit, n.ahead = n.ahead, 
%%                  na.action = na.action, range.p = range.p, plot = plot, 
%%                  ylab = ylab, xlab = xlab, main = main), SIMPLIFY = TRUE, 
%%                USE.NAMES = TRUE)
%%        }
%%        else {
%%            Predictions <- mapply(arimapred, TimeSeries, 
%%                ylab = colnames(TimeSeries), MoreArgs = list(timeseries.cont = TimeSeriesCont, 
%%                  xreg = xreg, newxreg = newxreg, se.fit = se.fit, n.ahead = n.ahead, 
%%                  na.action = na.action, range.p = range.p, plot = plot, 
%%                  xlab = xlab, main = main), SIMPLIFY = TRUE, 
%%                USE.NAMES = TRUE)
%%        }
%%    }
%%    return(Predictions)
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }