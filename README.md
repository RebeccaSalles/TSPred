[![Build Status](https://travis-ci.org/RebeccaSalles/TSPred.svg?branch=master)](https://travis-ci.org/RebeccaSalles/TSPred)
[![codecov](https://codecov.io/gh/RebeccaSalles/TSPred/branch/master/graph/badge.svg)](https://codecov.io/gh/RebeccaSalles/TSPred)
[![cran version](http://www.r-pkg.org/badges/version/TSPred)](http://cran.r-project.org/package=TSPred)
[![downloads](http://cranlogs.r-pkg.org/badges/TSPred)](http://cranlogs.r-pkg.org/badges/TSPred)
[![Research software impact](http://depsy.org/api/package/cran/TSPred/badge.svg)](http://depsy.org/package/r/TSPred)

## TSPred Package for R : Functions for Benchmarking Time Series Prediction

__Current Version:__ 3.0
__Date:__ 2017-03

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
~~~~~~
#Example 1 - a single univariate time series
> data(SantaFe.A)
> arimapred(SantaFe.A[,1],n.ahead=100)

#Example 2 - allowing the prediction of multiple univariate time series
> data(NN3.A,NN3.A.cont)
> marimapred(NN3.A,NN3.A.cont,plot=TRUE)

#Example 3 - time series interpolation
> data(CATS)
> arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)

#Example 4 - automatic fitting, prediction and accuracy evaluation
> data(CATS,CATS.cont)
> fArima <- fittestArima(CATS[,1],CATS.cont[,1], se.fit=TRUE)
#predicted values
> pred <- fArima$pred$pred
#model information
> cbind(AICc=fArima$AICc, AIC=fArima$AIC, BIC=fArima$BIC, logLik=fArima$logLik, MSE=fArima$MSE, NMSE=fArima$NMSE, MAPE=fArima$MSE, sMAPE=fArima$MSE, MaxError=fArima$MaxError)

#Example 5 - automatic fitting with Kalman filter, prediction and accuracy evaluation
> data(CATS,CATS.cont)
> fArimaKF <- fittestArimaKF(CATS[,2],CATS.cont[,2], se.fit=TRUE, filtered=TRUE)
#predicted values and estimated standard errors
> pred <- fArimaKF$pred
#model information
> fArimaKF$rank[1,]
~~~~~~
#### Polynomial regression fitting and prediction:
~~~~~~
#Example 1 - automatic fitting, prediction and accuracy evaluation
> data(CATS,CATS.cont)
> fPolyR <- fittestPolyR(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE)
#predicted values and estimated standard errors
> pred <- fPolyR$pred
#model information
> fPolyR$rank[1,]

#Example 2 - automatic fitting with Kalman filter, prediction and accuracy evaluation
> data(CATS,CATS.cont)
> fPolyRKF <- fittestPolyRKF(CATS[,1],CATS.cont[,1], maxorder=5, se.fit=TRUE, filtered=TRUE)
#predicted values and estimated standard errors
> pred <- fPolyRKF$pred
#model information
> fPolyRKF$rank[1,]
~~~~~~

---
Developed Baseline Evaluation Processes:
--
The TSPred R-Package enables the evaluation of time series prediction methods against ARIMA. ARIMA establishes a baseline linear prediction model that can be consistently used to compare with several other machine learning methods. In order to aid such comparision, we have included some benchmark prediction competition datasets. Some of the cited evaluation processes with respect to 5 of the most important time series prediction competitions organized so far are presented in the following.

* __[The Santa Fe Time Series Competition Experiment](https://sourceforge.net/p/gpca/wiki/Santa%20Fe%20Competition/)__
* __[The EUNITE Competition Experiment](https://sourceforge.net/p/gpca/wiki/EUNITE%20Competition/)__
* __[The CATS Competition Experiment](https://sourceforge.net/p/gpca/wiki/CATS%20Competition/)__
* __[The NN3 / NN5 Competition Experiment](https://sourceforge.net/p/gpca/wiki/NN3-NN5%20Competition/)__

These competitions were adopted as they maintain their datasets and results available, besides making accessible the papers of a large number of competitors, which describe their applied methods. The works presented in them comprehend a large variety of machine learning methods, which demonstrate great efforts done by the community of scientists on the time series prediction problem through years.

Furthermore, all the selected competitions provide free and easy access to the performance evaluation metrics used, as well as the ranked prediction errors found by each of their 125 competitors. This enabled us to compare the prediction errors and performance of the competitors’ methods against the baseline. Thus, the test and analysis of prediction results is facilitated.

These selected benchmarks differ from each other in many aspects, such as number of time series and their length, number of observations to be predicted, seasonality, missing data, prediction error metrics, etc.
