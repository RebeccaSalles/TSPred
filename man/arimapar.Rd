\name{arimapar}
\alias{arimapar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get ARIMA model parameters.
%%  ~~function to do ... ~~
}
\description{The function returns the parameters of an automatically fitted ARIMA model, including non-seasonal and seasonal orders and drift.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arimapar(timeseries, na.action = na.omit, xreg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting an ARIMA model.
%%     ~~Describe \code{timeseries} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries}.
%%     ~~Describe \code{na.action} here~~
}
  \item{xreg}{A vector, matrix, data frame or times series of external regressors used for fitting the ARIMA model.  It must have the same number of rows as \code{timeseries}. Ignored if \code{NULL}.
%%     ~~Describe \code{xreg} here~~
}
}
\details{The ARIMA model whose adjusted parameters are presented is automatically fitted by the \code{\link{auto.arima}} function in the forecast package. In order to avoid drift errors, the function introduces an auxiliary regressor whose values are a sequence of consecutive integer numbers starting from 1.
For more details, see the \code{\link{auto.arima}} function in the forecast package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric vector giving the number of AR, MA, seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences of the automatically fitted ARIMA model. It is also presented the value of the fitted drift constant.
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
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}

\seealso{\code{\link{marimapar}}, \code{\link{arimapred}}, \code{\link{marimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A)
arimapar(SantaFe.A[,1])
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%## The function is currently defined as
%%function (timeseries, na.action = na.omit, xreg = NULL) 
%%{
%%    if (is.null(timeseries)) 
%%        stop("timeseries is required and must have positive length")
%%    ts <- ts(na.action(timeseries))
%%    nobs <- length(ts)
%%    reg <- cbind(1:nobs, xreg)
%%    fit <- auto.arima(ts, xreg = ts(reg, start = 1))
%%    if (tail(colnames(fit$var.coef), 1) == "drift") {
%%        ARIMAModelInfo <- cbind(AR = fit$arma[1], Diff = fit$arma[6], 
%%            MA = fit$arma[2], SeasonalAR = fit$arma[3], SeasonalDiff = fit$arma[7], 
%%            SeasonalMA = fit$arma[4], Period = fit$arma[5], Drift = tail(fit$coef, 
%%                1))
%%    }
%%    else {
%%        ARIMAModelInfo <- cbind(AR = fit$arma[1], Diff = fit$arma[6], 
%%            MA = fit$arma[2], SeasonalAR = fit$arma[3], SeasonalDiff = fit$arma[7], 
%%            SeasonalMA = fit$arma[4], Period = fit$arma[5], Drift = NA)
%%    }
%%    return(ARIMAModelInfo)
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ parameters }