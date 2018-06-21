\name{TSPred-package}
\alias{TSPred-package}
\alias{TSPred}
\docType{package}
\title{Functions for Benchmarking Time Series Prediction
%%~~ package title ~~
}
\description{
Functions for time series preprocessing, decomposition, prediction and accuracy assessment using automatic linear modelling. The generated linear models and its yielded prediction errors can be used for benchmarking other time series prediction methods and for creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.
%%~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab TSPred\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2018-06-20\cr
Imports: \tab forecast, KFAS, stats, MuMIn, EMD, wavelets, vars\cr
LinkingTo: \tab dlmodeler\cr
License: \tab GPL (>= 2)\cr
BugReports: \tab \url{https://github.com/RebeccaSalles/TSPred/issues}\cr
URL: \tab \url{https://github.com/RebeccaSalles/TSPred/wiki}\cr
}
\describe{
Most important functions:

\item{\code{fittestLM}}{Automatically finding fittest linear model for prediction.}
\item{\code{fittestArima}}{Automatic ARIMA fitting, prediction and accuracy evaluation.}
\item{\code{fittestArimaKF}}{Automatic ARIMA fitting and prediction with Kalman filter.}
\item{\code{fittestPolyR}}{Automatic fitting and prediction of polynomial regression.}
\item{\code{fittestPolyRKF}}{Automatic fitting and prediction of polynomial regression with Kalman filter.}

\item{\code{fittestMAS}}{Automatic prediction with moving average smoothing.}
\item{\code{fittestWavelet}}{Automatic prediction with wavelet transform.}
\item{\code{fittestEMD}}{Automatic prediction with empirical mode decomposition.}
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Rebecca Pontes Salles <rebeccapsalles@acm.org> and Eduardo Ogasawara from CEFET/RJ
%%~~ The author and/or maintainer of the package ~~
}
\note{The authors thank CNPq for partially sponsoring this work.}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\keyword{ ARIMA }
\keyword{ time series }
\keyword{ prediction }
\keyword{ baseline }
\keyword{ benchmark }
\keyword{ datasets }
\keyword{ competition }
\keyword{ fittest }
\keyword{ linear }
\keyword{ model }
\keyword{ polynomial }
\keyword{ regression }
\keyword{ automatic }
\keyword{ fitting }
\keyword{ Kalman }
\keyword{ filter }
\keyword{ adjustment }
\keyword{ evaluation }
\keyword{ criterion }
\keyword{ errors }
\keyword{ transform }
\keyword{ decomposition }
\keyword{ wavelet }
\keyword{ EMD }

\seealso{\code{\link{fittestArima}}, \code{\link{fittestArimaKF}}, \code{\link{fittestPolyR}}, \code{\link{fittestPolyRKF}}, \code{\link{fittestLM}}, \code{\link{fittestMAS}}, \code{\link{fittestWavelet}}, \code{\link{fittestEMD}}
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
#======== Fittest linear model ========
\dontrun{
data(CATS,CATS.cont)
fittest <- fittestLM(CATS[,1],CATS.cont[,1])

#fittest model information
fittest$rank[1,]

#predictions of the fittest model
fittest$ranked.results[[1]]$pred
}
#======== ===================== ========

#======== ARIMA fitting and prediction ========
#Example 1 - a single univariate time series
data(SantaFe.A,SantaFe.A.cont)
arimapred(SantaFe.A[,1],n.ahead=100)


#Example 2 - allowing the prediction of multiple univariate time series
marimapred(SantaFe.A,SantaFe.A.cont)

\dontrun{
#Example 3 - automatic fitting, prediction and accuracy evaluation
data(CATS,CATS.cont)
fArima <- fittestArima(CATS[,1],CATS.cont[,1])
#predicted values
pred <- fArima$pred$mean
#model information
cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC,
 logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE, 
 MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)

#plotting the time series data
plot(c(CATS[,1],CATS.cont[,1]),type='o',lwd=2,xlim=c(960,1000),ylim=c(0,200),
 xlab="Time",ylab="ARIMA")
#plotting the predicted values
lines(ts(pred,start=981),lwd=2,col='blue')
#plotting prediction intervals
lines(ts(fArima$pred$upper[,2],start=981),lwd=2,col='light blue')
lines(ts(fArima$pred$lower[,2],start=981),lwd=2,col='light blue')


#Example 4 - automatic fitting with Kalman filter, prediction and accuracy evaluation
data(CATS,CATS.cont)
fArimaKF <- fittestArimaKF(CATS[,2],CATS.cont[,2])
#predicted values
pred <- fArimaKF$pred

#extracting Kalman filtered and smoothed time series from the best fitted model
fs <- KFAS::KFS(fArimaKF$model,filtering=c("state","mean"),smoothing=c("state","mean"))
f <- fitted(fs, filtered = TRUE) #Kalman filtered time  series
s <- fitted(fs) #Kalman smoothed time  series
#plotting the time series data
plot(c(CATS[,2],CATS.cont[,2]),type='o',lwd=2,xlim=c(960,1000),ylim=c(200,600),
 xlab="Time",ylab="ARIMAKF")
#plotting the Kalman filtered time series
lines(f,col='red',lty=2,lwd=2)
#plotting the Kalman smoothed time series
lines(s,col='green',lty=2,lwd=2)
#plotting predicted values
lines(ts(pred$mean,start=981),lwd=2,col='blue')
#plotting prediction intervals
lines(ts(pred$upper,start=981),lwd=2,col='light blue')
lines(ts(pred$lower,start=981),lwd=2,col='light blue')
#======== ================ ========

#======== Polynomial regression fitting and prediction ========
#Example 1 - automatic fitting, prediction and accuracy evaluation
data(CATS,CATS.cont)
fPolyR <- fittestPolyR(CATS[,3],CATS.cont[,3])
#predicted values
pred <- fPolyR$pred

#plotting the time series data
plot(c(CATS[,3],CATS.cont[,3]),type='o',lwd=2,xlim=c(960,1000),ylim=c(-100,300),
xlab="Time",ylab="PR")
#plotting predicted values
lines(ts(pred$mean,start=981),lwd=2,col='blue')
#plotting prediction intervals
lines(ts(pred$lower,start=981),lwd=2,col='light blue')
lines(ts(pred$upper,start=981),lwd=2,col='light blue')


#Example 2 - automatic fitting with Kalman filter, prediction and accuracy evaluation
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
#======== ============================================ ========

#======== Automatic moving average smoothing and ARIMA prediction ========
fMAS <- fittestMAS(CATS[,1],h=20,model="arima")

#automatically selected order of moving average
mas.order <- fMAS$order
#======== ======================================================= ========

#======== Automatic wavelet transform and ARIMA prediction ========
fW <- fittestWavelet(CATS[,1],h=20,model="arima")

#plot wavelet transform/decomposition
plot(fW$WT)
#======== ================================================ ========

#======== Automatic empirical mode decomposition and VAR prediction ========
femd <- fittestEMD(CATS[,1],h=20)
#======== =========================================================== ========
}
%%~~ simple examples of the most important functions ~~
}
