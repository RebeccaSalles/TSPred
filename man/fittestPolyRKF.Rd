\name{fittestPolyRKF}
\alias{fittestPolyRKF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic fitting and prediction of polynomial regression with Kalman filter 
%%  ~~function to do ... ~~
}
\description{
The function predicts and returns the next n consecutive values of a univariate time series using the best evaluated polynomial regression model automatically fitted with Kalman filter. It also evaluates the fitness of the produced model, using AICc, AIC, BIC and logLik criteria, and its prediction accuracy, using the MSE, NMSE, MAPE, sMAPE and maximal error accuracy measures.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fittestPolyRKF(timeseries, timeseries.test=NULL, h=NULL, na.action=na.omit,
level=0.9, order=NULL, minorder=0, maxorder=5, initQ=NULL, filtered = TRUE, 
rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik","errors","fitness")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting a polynomial regression model with Kalman filter.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. It is used as a testing set and base for calculation of prediction error measures. Ignored if \code{NULL}.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{h}{Number of consecutive values of the time series to be predicted. If \code{h} is \code{NULL}, the number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}. Required when \code{timeseries.test} is \code{NULL}.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.test}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.test}.
%%     ~~Describe \code{na.action} here~~
}
  \item{level}{Confidence level for prediction intervals. See the \code{\link{predict.SSModel}} function in the \code{KFAS} package.
%%     ~~Describe \code{na.action} here~~
}
  \item{order}{A numeric integer value corresponding to the order of polynomial regression to be fitted. If \code{NULL}, the order of the polynomial regression returned by the function is automatically selected within the interval \code{minorder:maxorder}. See 'Details'.
%%     ~~Describe \code{maxorder} here~~
}
  \item{minorder}{A numeric integer value corresponding to the minimum order of candidate polynomial regression to be fitted and evaluated. Ignored if \code{order} is provided. See 'Details'.
%%     ~~Describe \code{maxorder} here~~
}
  \item{maxorder}{A numeric integer value corresponding to the maximal order of candidate polynomial regression to be fitted and evaluated. Ignored if \code{order} is provided. See 'Details'.
%%     ~~Describe \code{maxorder} here~~
}
  \item{filtered}{If \code{filtered} is \code{TRUE}, Kalman filtered time series observations are used for prediction, otherwise, Kalman smoothed observations are used for prediction.
%%     ~~Describe \code{filtered} here~~
}
  \item{initQ}{Numeric argument regarding the initial values for the covariance of disturbances parameter to be optimized over. The initial values to be optimized are set to \code{rep(initQ,(order+1))}. See the \code{Q} argument of the \code{\link{SSModel}} function in the \code{KFAS} package and the examples in \code{\link{KFAS}}. If \code{NULL}, \code{initQ} is automatically set. See 'Details'.
%%     ~~Describe \code{filtered} here~~
}
  \item{rank.by}{Character string. Criteria used for ranking candidate models generated using different options of values for \code{order} and/or \code{initQ}. Ignored if both \code{order} and \code{initQ} are provided. See 'Details'.
%%     ~~Describe \code{filtered} here~~
}
}
\details{The polynomial regression model produced and returned by the function is generated and represented as state space model (\code{\link{SSModel}}) based on code from the \code{dlmodeler} package. See \code{dlmodeler.polynomial}. The model is optimized using the Kalman filter and functions of the \code{KFAS} package (see \code{\link{fitSSM}}).

If \code{order} is \code{NULL}, it is automatically selected. For that, a set of candidate polynomial regression state space models of orders from \code{minorder} to \code{maxorder} is generated and evaluated. Also, if \code{initQ} is \code{NULL}, it is automatically set as either \code{log(var(timeseries))} or \code{0}. For that, candidate models receive different initial parameterization of \code{initQ} during the model optimization process. The value options of \code{order} and/or \code{initQ} which generate the best ranked candidate polynomial regression model acoording to the criteria in \code{rank.by} are selected.

The ranking criteria in \code{rank.by} may be set as a prediction error measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}}, \code{\link{sMAPE}} or \code{\link{MaxError}}), or as a fitness criteria (such as \code{\link{AIC}}, \code{\link{AICc}}, \code{\link{BIC}} or \code{\link{logLik}}). In the former case, the candidate models are used for time series prediction and the error measures are calculated by means of a cross-validation process. In the latter case, the candidate models are fitted and fitness criteria are calculated based on all observations in \code{timeseries}.

If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the candidate models are ranked by all the mentioned prediction error measures or fitness criteria, respectively. The wheight of the ranking criteria is equally distributed. In this case, a \code{rank.position.sum} criterion is produced for ranking the candidate models. The \code{rank.position.sum} criterion is calculated as the sum of the rank positions of a model (1 = 1st position = better ranked model, 2 = 2nd position, etc.) on each calculated ranking criteria.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{model}{An object of class "SSModel" containing the best evaluated polynomial regression model fitted with Kalman Filter.}
\item{order}{The order argument provided (or automatically selected) for the best evaluated polynomial regression model fitted with Kalman Filter.}
\item{initQ}{The initQ argument provided (or automatically selected) for optimization of the best evaluated polynomial regression model fitted with Kalman Filter.}
\item{AICc}{Numeric value of the computed AICc criterion of the best evaluated model.}
\item{AIC}{Numeric value of the computed AIC criterion of the best evaluated model.}
\item{BIC}{Numeric value of the computed BIC criterion of the best evaluated model.}
\item{logLik}{Numeric value of the computed log-likelihood of the best evaluated model.}
\item{pred}{A list with the components \code{mean}, \code{lower} and \code{upper}, containing the predictions of the best evaluated model and the lower and upper limits for prediction intervals, respectively. All components are time series. See \code{\link{predict.SSModel}}.}
\item{MSE}{Numeric value of the resulting MSE error of prediction. Require \code{timeseries.test}.}
\item{NMSE}{Numeric value of the resulting NMSE error of prediction. Require \code{timeseries.test}.}
\item{MAPE}{Numeric value of the resulting MAPE error of prediction. Require \code{timeseries.test}.}
\item{sMAPE}{Numeric value of the resulting sMAPE error of prediction. Require \code{timeseries.test}.}
\item{MaxError}{Numeric value of the maximal error of prediction. Require \code{timeseries.test}.}
\item{rank.val}{Data.frame with the fitness or prediction accuracy criteria computed for all candidate polynomial regression with Kalman filter models  ranked by \code{rank.by}. It has the attribute \code{"ranked.models"}, which is a list of objects of class "SSModel" containing all the candidate polynomial regression models fitted with Kalman Filter, also ranked by \code{rank.by}. Only provided if \code{order} or \code{initQ} were automatically selected.}
\item{rank.by}{Ranking criteria used for ranking candidate models and producing \code{rank.val}.}
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
\seealso{\code{\link{fittestPolyR}}, \code{\link{fittestLM}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
data(CATS,CATS.cont)
fPolyRKF <- fittestPolyRKF(CATS[,1],CATS.cont[,1])
#predicted values
pred <- fPolyRKF$pred

#extracting Kalman filtered and smoothed time series from the best fitted model
fs <- KFAS::KFS(fPolyRKF$model,filtering=c("state","mean"),smoothing=c("state","mean"))
f <- fitted(fs, filtered = TRUE) #Kalman filtered time  series
s <- fitted(fs) #Kalman smoothed time  series
#plotting the time series data
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),
 xlab="Time",ylab="PRKF")
#plotting the Kalman filtered time series
lines(f,col='red',lty=2,lwd=2)
#plotting the Kalman smoothed time series
lines(s,col='green',lty=2,lwd=2)
#plotting predicted values
lines(ts(pred$mean,start=981),lwd=2,col='blue')
#plotting prediction intervals
lines(ts(pred$lower,start=981),lwd=2,col='light blue')
lines(ts(pred$upper,start=981),lwd=2,col='light blue')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ polynomial }% __ONLY ONE__ keyword per line
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