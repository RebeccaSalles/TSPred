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
fittestLM(timeseries, timeseries.test=NULL, h=NULL, level=0.95, na.action=na.omit,
filtered=TRUE, order=NULL, minorder=0, maxorder=5, raw = FALSE, initQ=NULL,
rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness"),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting the models.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. It is used as a testing set and base for calculation of prediction error measures. Ignored if \code{NULL}.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{h}{Number of consecutive values of the time series to be predicted. If \code{h} is \code{NULL}, the number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}. Required when \code{timeseries.test} is \code{NULL}.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{level}{Confidence level for prediction intervals.
%%     ~~Describe \code{na.action} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.test}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.test}.
%%     ~~Describe \code{na.action} here~~
}
  \item{filtered}{See \code{\link{fittestArimaKF}} and \code{\link{fittestPolyRKF}}.
%%     ~~Describe \code{filtered} here~~
}
  \item{initQ}{See \code{\link{fittestArimaKF}} and \code{\link{fittestPolyRKF}}.
%%     ~~Describe \code{filtered} here~~
}
  \item{order}{See \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{minorder}{See \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{maxorder}{See \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{raw}{See \code{\link{fittestPolyR}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{...}{See \code{\link{fittestArima}} and \code{\link{fittestArimaKF}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{rank.by}{Character string. Criteria used for ranking candidate models. See 'Details'.
%%     ~~Describe \code{filtered} here~~
}
}
\details{The results of the best evaluated models returned by \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}} are ranked and the fittest linear model for prediction of the given univariate time series is selected based on the criteria in \code{rank.by}.

The ranking criteria in \code{rank.by} may be set as a prediction error measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}}, \code{\link{sMAPE}} or \code{\link{MaxError}}), or as a fitness criteria (such as \code{\link{AIC}}, \code{\link{AICc}}, \code{\link{BIC}} or \code{\link{logLik}}). See \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} or \code{\link{fittestPolyRKF}}.

If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the candidate models are ranked by all the mentioned prediction error measures or fitness criteria, respectively. The wheight of the ranking criteria is equally distributed. In this case, a \code{rank.position.sum} criterion is produced for ranking the candidate models. The \code{rank.position.sum} criterion is calculated as the sum of the rank positions of a model (1 = 1st position = better ranked model, 2 = 2nd position, etc.) on each calculated ranking criteria.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{model}{An object containing the fittest evaluated linear model. The class of the model object is dependent on the results of the evaluation (ranking). See \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}.}
\item{rank}{Data.frame with the fitness and/or prediction accuracy criteria computed for all models considered, ranked by \code{rank.by}.}
\item{ranked.results}{A list of lists containing the ranked results of the functions \code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}} and \code{\link{fittestPolyRKF}}. Also ranked by \code{rank.by}.}
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}}, \code{\link{fittestPolyRKF}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
data(CATS,CATS.cont)
fittest <- fittestLM(CATS[,1],CATS.cont[,1])

#fittest model information
fittest$rank[1,]

#predictions of the fittest model
fittest$ranked.results[[1]]$pred
}
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