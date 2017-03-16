\name{fittestPolyR}
\alias{fittestPolyR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic fitting and prediction of polynomial regression
%%  ~~function to do ... ~~
}
\description{
The function predicts and returns the next n consecutive values of a univariate time series using the best evaluated automatically fitted polynomial regression model. It also evaluates the fitness of the produced model, using AICc, AIC, BIC and logLik criteria, and its prediction accuracy, using the MSE, NMSE, MAPE, sMAPE and maximal error accuracy measures.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fittestPolyR(timeseries, timeseries.test, maxorder = 5, na.action = na.omit,
se.fit = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting a polynomial regression model.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. The number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}. It is used as a testing set and base for calculation of prediction error measures.
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
}
\details{A set with possible polynomial regression models of orders from 0 to \code{maxorder} is generated with help from the \code{\link{dredge}} function from the \code{MuMIn} package.

The set of generated models are used for time series prediction. Fitness measures (AICc, AIC, BIC, log-likelihood) and prediction accuracy measures (MSE, NMSE, MAPE, sMAPE, maximal error) are calculated for each model.

The models are ranked and the best evaluated fitted polynomial regression model is selected based on the lowest value of the TSPredC criterion.

The TSPredC criterion is calculated as the sum of the rank possitions of a model (1 = 1st position = better ranked model, 2 = 2nd position, etc.) on each calculated fitness or prediction accuracy measure.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{model}{An object of class "lm" containing the best evaluated polynomial regression model.}
\item{AICc}{Numeric value of the computed AICc criterion of the best evaluated model.}
\item{AIC}{Numeric value of the computed AIC criterion of the best evaluated model.}
\item{BIC}{Numeric value of the computed BIC criterion of the best evaluated model.}
\item{logLik}{Numeric value of the computed log-likelihood of the best evaluated model.}
\item{pred}{A time series of predictions of the best evaluated model. If \code{se.fit} is \code{TRUE}, the estimated standard errors are included.}
\item{MSE}{Numeric value of the resulting MSE error of prediction.}
\item{NMSE}{Numeric value of the resulting NMSE error of prediction.}
\item{MAPE}{Numeric value of the resulting MAPE error of prediction.}
\item{sMAPE}{Numeric value of the resulting sMAPE error of prediction.}
\item{MaxError}{Numeric value of the maximal error of prediction.}
\item{rank}{Data.frame with the coefficients and the fitness and prediction accuracy criteria computed for all polynomial regression models considered, ranked by the TSPredC value. It has the attribute \code{"model.calls"}, which is a list of objects of class "expression" containing the calls of all the evaluated polynomial regression models, also ranked by the TSPredC value.}
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
\seealso{\code{\link{fittestPolyRKF}}, \code{\link{fittestLM}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS,CATS.cont)
fPolyR <- fittestPolyR(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE)
#predicted values and estimated standard errors
pred <- fPolyR$pred
#model information
fPolyR$rank[1,]

#plotting the time series data
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),xlab="Time",ylab="PR")
#plotting predicted values
lines(ts(pred$fit,start=981),lwd=2,col='blue')
#plotting estimated standard errors (confidence interval)
lines(ts(pred$fit+pred$se.fit,start=981),lwd=2,col='light blue')
lines(ts(pred$fit-pred$se.fit,start=981),lwd=2,col='light blue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ polynomial }% __ONLY ONE__ keyword per line
\keyword{ regression }
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }
\keyword{ evaluation }
\keyword{ criterion }
\keyword{ errors }