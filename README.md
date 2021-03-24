<!--- [![Build Status](https://travis-ci.org/RebeccaSalles/TSPred.svg?branch=master)](https://travis-ci.org/RebeccaSalles/TSPred)-->
[![codecov](https://codecov.io/gh/RebeccaSalles/TSPred/branch/master/graph/badge.svg)](https://codecov.io/gh/RebeccaSalles/TSPred)
[![cran version](http://www.r-pkg.org/badges/version/TSPred)](http://cran.r-project.org/package=TSPred)
[![downloads](http://cranlogs.r-pkg.org/badges/TSPred)](http://cranlogs.r-pkg.org/badges/TSPred)
[![Research software impact](http://depsy.org/api/package/cran/TSPred/badge.svg)](http://depsy.org/package/r/TSPred)

## TSPred Package for R : Functions for Benchmarking Time Series Prediction

__Current Version:__ 5.1
__Date:__ 2021-01

__Authors:__ Rebecca Pontes Salles (<rebeccapsalles@acm.org>) and Eduardo Ogasawara (<eogasawara@ieee.org>)
 
__Description:__ Functions for defining and conducting a time series prediction process including pre(post)processing, decomposition, modeling, prediction, and accuracy assessment. The generated models and their yielded prediction errors can be used for benchmarking other time series prediction methods and for creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.

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
#### Fittest linear models:
* __fittestLM__ - Automatically finding fittest linear model for prediction.
* __fittestArima__ - Automatic ARIMA fitting, prediction and accuracy evaluation.
* __fittestArimaKF__ - Automatic ARIMA fitting and prediction with Kalman filter.
* __fittestPolyR__ - Automatic fitting and prediction of polynomial regression.
* __fittestPolyRKF__ - Automatic fitting and prediction of polynomial regression with Kalman filter.
#### Automatic preprocessing/decomposition and prediction:
* __fittestMAS__ - Automatic prediction with moving average smoothing.
* __fittestWavelet__ - Automatic prediction with wavelet transform.
* __fittestEMD__ - Automatic prediction with empirical mode decomposition.

### __Examples:__
#### Fittest linear model:
~~~~~~
> data(CATS,CATS.cont)
> fittest <- fittestLM(CATS[,1],CATS.cont[,1])
#fittest model information
> fittest$rank[1,]
#predictions of the fittest model
> fittest$ranked.results[[1]]$pred
~~~~~~
#### ARIMA fitting and prediction:
###### 1 - Single univariate time series:
~~~~~~
> data(SantaFe.A)
> arimapred(SantaFe.A[,1],n.ahead=100)
~~~~~~
###### 2 - Allowing the prediction of multiple univariate time series:
~~~~~~
> data(NN3.A,NN3.A.cont)
> marimapred(NN3.A,NN3.A.cont,plot=TRUE)
~~~~~~
###### 3 - Time series interpolation:
~~~~~~
> data(CATS)
> arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)
~~~~~~
###### 4 - Automatic fitting, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fArima <- fittestArima(CATS[,1],CATS.cont[,1])
#predicted values
> pred <- fArima$pred$mean
#model information
> cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC, logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE, MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)
~~~~~~
###### 5 - Automatic fitting with Kalman filter, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fArimaKF <- fittestArimaKF(CATS[,2],CATS.cont[,2])
#predicted values and estimated standard errors
> pred <- fArimaKF$pred
~~~~~~
#### Polynomial regression fitting and prediction:
###### 1 - Automatic fitting, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fPolyR <- fittestPolyR(CATS[,3],CATS.cont[,3])
#predicted values
> pred <- fPolyR$pred
~~~~~~
###### 2 - Automatic fitting with Kalman filter, prediction and accuracy evaluation:
~~~~~~
> data(CATS,CATS.cont)
> fPolyRKF <- fittestPolyRKF(CATS[,1],CATS.cont[,1])
#predicted values
> pred <- fPolyRKF$pred
~~~~~~
#### Automatic moving average smoothing and ARIMA prediction:
~~~~~~
> data(CATS,CATS.cont)
> fMAS <- fittestMAS(CATS[,1],h=20,model="arima")
#automatically selected order of moving average
> mas.order <- fMAS$order
~~~~~~
#### Automatic wavelet transform and ARIMA prediction:
~~~~~~
> data(CATS,CATS.cont)
> fW <- fittestWavelet(CATS[,1],h=20,model="arima")
#plot wavelet transform/decomposition
> plot(fW$WT)
~~~~~~
#### Automatic empirical mode decomposition and VAR prediction:
~~~~~~
> data(CATS,CATS.cont)
> femd <- fittestEMD(CATS[,1],h=20)
~~~~~~
