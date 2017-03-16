\name{plotarimapred}
\alias{plotarimapred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot ARIMA predictions against actual values
%%  ~~function to do ... ~~
}
\description{The function plots ARIMA predictions against its actual values with prediction intervals.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotarimapred(ts.cont, fit.arima, xlim, range.percent = 0.2, xreg = NULL,
ylab = NULL, xlab = NULL, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts.cont}{A vector or univariate time series containing actual values for a time series that are to be plotted against its respective predictions. The number of consecutive values to be predicted is assumed to be equal to the number of rows in \code{ts.cont}. If \code{xreg} is used, the number of values to be predicted is set to the number of rows of \code{xreg}.
%%     ~~Describe \code{ts.cont} here~~
}
  \item{fit.arima}{A fitted ARIMA model for the time series that is to be predicted. An object of class "\code{Arima}", "\code{ar}" or "\code{fracdiff}". See the \code{object} argument of the \code{\link{forecast.Arima}} function in the forecast package.
%%     ~~Describe \code{fit.arima} here~~
}
  \item{xlim}{Numeric vector containing the initial and final limits of the x-axis to be plotted, respectively.
%%     ~~Describe \code{xlimI} here~~
}
  \item{range.percent}{A percentage which defines how much the range of the graphic's y-axis will be increased from the minimum limits imposed by data.
%%     ~~Describe \code{range.percent} here~~
}
  \item{xreg}{A vector, matrix, data frame or times series with new values of external regressors to be used for prediction (for class Arima objects only). See the \code{xreg} argument of the \code{\link{forecast.Arima}} function in the forecast package.
%%     ~~Describe \code{xreg} here~~
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
\details{The model in \code{fit.arima} is used for prediction by the \code{\link{forecast.Arima}} function in the \code{forecast} package. The resulting \code{forecast} object is then used for plotting the predictions and their intervals by the \code{\link{plot.forecast}} function also in the \code{forecast} package.
For more details, see the \code{\link{forecast.Arima}} and the  \code{\link{plot.forecast}} functions in the \code{forecast} package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{None.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{See the \code{\link{forecast.Arima}} and the \code{\link{plot.forecast}} functions in the forecast package.
%% ~put references to the literature/web site here ~
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{forecast.Arima}}, \code{\link{plot.forecast}}, \code{\link{arimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A,SantaFe.A.cont)
fit <- forecast::auto.arima(SantaFe.A)  
ts.cont <- ts(SantaFe.A.cont,start=1001)
plotarimapred(ts.cont, fit, xlim=c(1001,1100))
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%## The function is currently defined as
%%function (ts.cont, fit.arima, xlimI, xlimF, range.percent = 0.2, 
%%    xreg = NULL, ylab = NULL, xlab = NULL, main = NULL) 
%%{
%%    if (!is.null(xreg)) 
%%        ts.pred <- forecast(fit.arima, length(ts.cont), xreg = xreg)
%%    else ts.pred <- forecast(fit.arima, length(ts.cont))
%%    yrange.min <- min(ts.pred$lower, ts.cont) - range.percent * 
%%        abs(min(ts.pred$lower, ts.cont))
%%    yrange.max <- max(ts.pred$upper, ts.cont) + range.percent * 
%%        abs(max(ts.pred$upper, ts.cont))
%%    yrange <- range(yrange.min, yrange.max)
%%    plot(ts.pred, xlim = c(xlimI, xlimF), ylim = yrange, ylab = ylab, 
%%        xlab = xlab, main = main)
%%    lines(ts.cont, xlim = c(xlimI, xlimF), lty = 5, lwd = 2, 
%%        col = "black")
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ prediction }
\keyword{ plot }