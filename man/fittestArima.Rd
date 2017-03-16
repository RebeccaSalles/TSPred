\name{fittestArima}
\alias{fittestArima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic ARIMA fitting, prediction and accuracy evaluation
%%  ~~function to do ... ~~
}
\description{
The function predicts and returns the next n consecutive values of a univariate time series using an automatically best fitted ARIMA model. It also evaluates the fitness of the produced model, using AICc, AIC, BIC and logLik criteria, and its prediction accuracy, using the MSE, NMSE, MAPE, sMAPE and maximal error accuracy measures.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fittestArima(timeseries, timeseries.test, na.action = na.omit, se.fit = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series which contains the values used for fitting an ARIMA model.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. The number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}. It is used as a testing set and base for calculation of prediction error measures.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.test}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.test}.
%%     ~~Describe \code{na.action} here~~
}
  \item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the predictions are returned.
%%     ~~Describe \code{se.fit} here~~
}
}
\details{The ARIMA model used for time series prediction is automatically fitted by the \code{\link{auto.arima}} function in the \code{\link[pkg:forecast-package]{forecast}} package. The fitted ARIMA model is used for prediction by the \code{\link{predict.Arima}} function in the \code{\link[pkg:stats-package]{stats}} package.
The fitness criteria AICc, AIC (\code{\link{AIC}}), BIC (\code{\link{BIC}}) and log-likelihood (\code{\link{logLik}}) are extracted from the fitted ARIMA model. Also, the prediction accuracy of the model is computed by means of MSE (\code{\link{MSE}}), NMSE (\code{\link{NMSE}}), MAPE (\code{\link{MAPE}}), sMAPE (\code{\link{sMAPE}}) and maximal error (\code{\link{MAXError}}) measures.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{model}{A list of class "ARIMA" containing the best fitted ARIMA model. See the \code{\link{auto.arima}} function in the \code{\link[pkg:forecast-package]{forecast}} package.}
\item{AICc}{Numeric value of the computed AICc criterion of the fitted model.}
\item{AIC}{Numeric value of the computed AIC criterion of the fitted model.}
\item{BIC}{Numeric value of the computed BIC criterion of the fitted model.}
\item{logLik}{Numeric value of the computed log-likelihood of the fitted model.}
\item{pred}{A time series of predictions, or if \code{se.fit} is \code{TRUE}, a list with the components \code{pred}, the predictions, and \code{se}, the estimated standard errors. Both components are time series. See the \code{\link{predict.Arima}} function in the stats package.}
\item{MSE}{Numeric value of the resulting MSE error of prediction.}
\item{NMSE}{Numeric value of the resulting NMSE error of prediction.}
\item{MAPE}{Numeric value of the resulting MAPE error of prediction.}
\item{sMAPE}{Numeric value of the resulting sMAPE error of prediction.}
\item{MaxError}{Numeric value of the maximal error of prediction.}
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
\seealso{\code{\link{fittestArimaKF}}, \code{\link{fittestLM}}, \code{\link{marimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS,CATS.cont)
fArima <- fittestArima(CATS[,1],CATS.cont[,1], se.fit=TRUE)
#predicted values
pred <- fArima$pred$pred
#estimated standard errors
se <- fArima$pred$se
#model information
cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC,
 logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE, 
 MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)

#plotting the time series data
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),
 xlab="Time",ylab="ARIMA")
#plotting the predicted values
lines(ts(pred,start=981),lwd=2,col='blue')
#plotting the estimated standard errors (confidence interval)
lines(ts(pred+se,start=981),lwd=2,col='light blue')
lines(ts(pred-se,start=981),lwd=2,col='light blue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }
\keyword{ evaluation }
\keyword{ criterion }
\keyword{ errors }