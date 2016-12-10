\name{TSPred-package}
\alias{TSPred-package}
\alias{TSPred}
\docType{package}
\title{Functions for Baseline-Based Time Series Prediction
%%~~ package title ~~
}
\description{
Functions for time series prediction and accuracy assessment using automatic ARIMA modelling. The generated ARIMA models and its yielded prediction errors are intended to be used as baseline for evaluating the practical value of other time series prediction methods and creating a demand for the refinement of such methods. For this purpose, benchmark data from prediction competitions may be used.
%%~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab TSPred\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-11\cr
Depends: \tab forecast, stats\cr
License: \tab GPL (>= 2)\cr
URL: \tab \url{https://sourceforge.net/p/gpca/wiki/TSPred/}\cr
}
\describe{
Most important functions:
\item{\code{arimapred}}{Automatic ARIMA fitting and prediction.}
\item{\code{marimapred}}{Multiple time series automatic ARIMA fitting and prediction.}
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Rebecca Pontes Salles <rebeccapsalles@gmail.com> and Eduardo Ogasawara from CEFET/RJ
%%~~ The author and/or maintainer of the package ~~
}
\note{The authors thank CNPq for partially sponsoring this work.}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\keyword{ ARIMA }
\keyword{ time series }
\keyword{ prediction }
\keyword{ baseline }
\keyword{ datasets }
\keyword{ competition }
\seealso{\code{\link{arimapred}}, \code{\link{marimapred}}
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(SantaFe.A)
arimapred(SantaFe.A[,1],n.ahead=100)
%%~~ simple examples of the most important functions ~~
}
