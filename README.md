## TSPred Package for R : Functions for Baseline-Based Time Series Prediction

__Current Version:__ 2.0
__Date:__ 2015-04

__Authors:__ Rebecca Pontes Salles (<rebeccapsalles@acm.org>) and Eduardo Ogasawara (<eogasawara@ieee.org>)
 
__Description:__ Functions for time series prediction and accuracy assessment using automatic ARIMA modelling. The generated ARIMA models and its yielded prediction errors are intended to be used as baseline for evaluating the practical value of other time series prediction methods and creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.

__Available at CRAN:__ <http://cran.r-project.org/web/packages/TSPred/index.html>

__Reference manual:__ [TSPred.pdf](http://cran.r-project.org/web/packages/TSPred/TSPred.pdf)

__Acknowledgements:__ The authors thank CNPq for partially sponsoring this work.

---
###Usage:
~~~~~~
#Install TSPred package
> install.packages("TSPred")

#Load TSPred package
> library("TSPred")
~~~~~~
#####
#####Most important functions:

* __arimapred__ - Automatic ARIMA fitting and prediction.
* __marimapred__ - Multiple time series automatic ARIMA fitting and prediction.

######__Examples:__
~~~~~~
> data(SantaFe.A)
> arimapred(SantaFe.A[,1],n.ahead=100)

> data(NN3.A,NN3.A.cont)
> marimapred(NN3.A,NN3.A.cont,plot=TRUE)
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
