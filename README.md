[![Build Status](https://travis-ci.org/RebeccaSalles/TSPred.svg?branch=master)](https://travis-ci.org/RebeccaSalles/TSPred)
[![codecov](https://codecov.io/gh/RebeccaSalles/TSPred/branch/master/graph/badge.svg)](https://codecov.io/gh/RebeccaSalles/TSPred)
[![cran version](http://www.r-pkg.org/badges/version/TSPred)](http://cran.r-project.org/package=TSPred)
[![downloads](http://cranlogs.r-pkg.org/badges/TSPred)](http://cranlogs.r-pkg.org/badges/TSPred)
[![Research software impact](http://depsy.org/api/package/cran/TSPred/badge.svg)](http://depsy.org/package/r/TSPred)

## TSPred Package for R : Functions for Benchmarking Time Series Prediction

__Current Version:__ 3.0.2
__Date:__ 2017-04

__Authors:__ Rebecca Pontes Salles (<rebeccapsalles@acm.org>) and Eduardo Ogasawara (<eogasawara@ieee.org>)
 
__Description:__ Functions for time series prediction and accuracy assessment using automatic linear modelling. The generated linear models and its yielded prediction errors can be used for benchmarking other time series prediction methods and for creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.

__Available at CRAN:__ <https://CRAN.R-project.org/package=TSPred>

__Reference manual:__ [TSPred.pdf](http://cran.r-project.org/web/packages/TSPred/TSPred.pdf)

__Acknowledgements:__ The authors thank CNPq for partially sponsoring this work.

---
### Usage:
~~~~~~
#Install TSPred package
> install.packages("TSPred")

#Load TSPred package
> library("TSPred")
~~~~~~
#####
### Most important functions:

* __fittestLM__ - Automatically finding fittest linear model for prediction.
* __fittestArima__ - Automatic ARIMA fitting, prediction and accuracy evaluation.
* __fittestArimaKF__ - Automatic ARIMA fitting and prediction with Kalman filter.
* __fittestPolyR__ - Automatic fitting and prediction of polynomial regression.
* __fittestPolyRKF__ - Automatic fitting and prediction of polynomial regression with Kalman filter.
* __arimapred__ - Automatic ARIMA fitting and prediction.
* __marimapred__ - Multiple time series automatic ARIMA fitting and prediction.
* __arimainterp__ - Interpolation of unknown values using automatic ARIMA fitting and prediction.

### __Examples:__
#### Fittest linear model:
~~~~~~
> data(CATS,CATS.cont)
> fittest <- fittestLM(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE, filtered=TRUE)
#fittest model information
> fittest$rank[1,]
#predictions of the fittest model
> fittest$ranked.results[[1]]$pred
~~~~~~
#### ARIMA fitting and prediction:
##### 1 - Single univariate time series:
~~~~~~
> data(SantaFe.A)
> arimapred(SantaFe.A[,1],n.ahead=100)
~~~~~~
##### 2 - Allowing the prediction of multiple univariate time series:
~~~~~~
> data(NN3.A,NN3.A.cont)
> marimapred(NN3.A,NN3.A.cont,plot=TRUE)
~~~~~~
##### 3 - Time series interpolation:
~~~~~~
> data(CATS)
> arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)
~~~~~~
##### 4 - Automatic fitting, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fArima <- fittestArima(CATS[,1],CATS.cont[,1])
#predicted values
> pred <- fArima$pred$pred
#model information
> cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC, logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE, MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)
~~~~~~
##### 5 - Automatic fitting with Kalman filter, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fArimaKF <- fittestArimaKF(CATS[,2],CATS.cont[,2], se.fit=TRUE, filtered=TRUE)
#predicted values and estimated standard errors
> pred <- fArimaKF$pred
#model information
> fArimaKF$rank[1,]
~~~~~~
#### Polynomial regression fitting and prediction:
##### 1 - Automatic fitting, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fPolyR <- fittestPolyR(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE)
#predicted values and estimated standard errors
> pred <- fPolyR$pred
#model information
> fPolyR$rank[1,]
~~~~~~
##### 2 - Automatic fitting with Kalman filter, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fPolyRKF <- fittestPolyRKF(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE, filtered=TRUE)
#predicted values and estimated standard errors
> pred <- fPolyRKF$pred
#model information
> fPolyRKF$rank[1,]
~~~~~~
