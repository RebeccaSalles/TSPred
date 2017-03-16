\name{fittestLM}
\alias{fittestLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatically finding fittest linear model for prediction
%%  ~~function to do ... ~~
}
\description{
The function automatically evaluates and returns the fittest linear model among ARIMA and polynomial regression, with and without Kalman filtering, for prediction of a given univariate time series. Wrapper for the \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}} functions for automatic time series prediction, whose results are also returned.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fittestLM(timeseries, timeseries.test, maxorder = 5, na.action = na.omit,
se.fit = FALSE, filtered = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting the models.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. It is used as a testing set and base for calculation of prediction error measures. The number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{maxorder}{A numeric integer value corresponding to the maximal order of polynomial regression to be fitted and evaluated.
%%     ~~Describe \code{maxorder} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.test}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.test}.
%%     ~~Describe \code{na.action} here~~
}
  \item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the predictions are returned.
%%     ~~Describe \code{se.fit} here~~
}
  \item{filtered}{If \code{filtered} is \code{TRUE}, Kalman filtered time series observations are used for prediction, otherwise, Kalman smoothed observations are used for prediction.
%%     ~~Describe \code{filtered} here~~
}
}
\details{The results of the best evaluated models returned by \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}} are ranked and the fittest linear model for prediction of the given univariate time series is selected based on the lowest value of the TSPredC criterion.

The TSPredC criterion is calculated as the sum of the rank possitions of a model (1 = 1st position = better ranked model, 2 = 2nd position, etc.) on each calculated fitness or prediction accuracy measure.

The list of evaluated measures are:
\item{Model fitness measures:}{AIC, AICc, BIC and log-likelihood}
\item{Prediction accuracy measures:}{MSE, NMSE, MAPE, sMAPE and maximal error}
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{model}{An object containing the fittest evaluated linear model. The class of the model object is dependent on the results of the evaluation (ranking). See \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.}
\item{rank}{Data.frame with the fitness and prediction accuracy criteria computed for all models considered, ranked by the TSPredC value.}
\item{ranked.results}{A list of lists containing the ranked results of the functions \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.}
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}}, \code{\link{fittestPolyRKF}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS,CATS.cont)
fittest <- fittestLM(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE, filtered=TRUE)

#fittest model information
fittest$rank[1,]

#predictions of the fittest model
fittest$ranked.results[[1]]$pred
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fittest }% __ONLY ONE__ keyword per line
\keyword{ linear }
\keyword{ model }
\keyword{ ARIMA }
\keyword{ polynomial }
\keyword{ regression }
\keyword{ automatic }
\keyword{ fitting }
\keyword{ Kalman }
\keyword{ filter }
\keyword{ adjustment }
\keyword{ prediction }
\keyword{ evaluation }
\keyword{ criterion }
\keyword{ errors }