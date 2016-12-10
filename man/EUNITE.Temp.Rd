\name{EUNITE.Temp}
\alias{EUNITE.Temp}
\docType{data}
\title{Temperatures of the EUNITE Competition
%%   ~~ data name/kind ... ~~
}
\description{The EUNITE Competition dataset composed of a univariate time series of average daily temperatures measured between 1995 and 1998.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("EUNITE.Temp")}
\format{
  A data frame with 1461 observations on the following variable.
  \describe{
    \item{\code{Temperature}}{a numeric vector with average daily temperatures measured in the period 1995-1998.}
  }
}
\details{The EUNITE Competition proposed the prediction of maximum daily electrical loads based on half-an-hour loads (\code{\link{EUNITE.Loads}}) and average daily temperatures of 1997-1998, where the latter is used as a regressor.
Competitors were asked to predict the 31 values corresponding to the daily maximum electrical loads of January 1999 (\code{\link{EUNITE.Loads.cont}}).
For the posed prediction problem, the average daily temperatures of January 1999 must also be predicted and for that, the use of data on average daily temperatures of 1995-1996 was allowed.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{EUNITE 1999, Electricity Load Forecast using Intelligent Adaptive Technology: The EUNITE Network Competition. URL: \url{http://neuron.tuke.sk/competition/index.php}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{B.-J. Chen, M.-W. Chang, and C.-J. Lin, 2004, Load forecasting using support vector Machines: a study on EUNITE competition 2001, IEEE Transactions on Power Systems, v. 19, n. 4 (Nov.), p. 1821-1830.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{EUNITE.Temp.cont}}, \code{\link{EUNITE.Loads}}, \code{\link{EUNITE.Reg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(EUNITE.Temp)
str(EUNITE.Temp)
plot(ts(EUNITE.Temp))
}
\keyword{datasets}
\keyword{EUNITE}
\keyword{Time Series}
\keyword{Competition}