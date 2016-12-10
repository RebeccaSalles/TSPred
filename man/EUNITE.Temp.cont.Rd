\name{EUNITE.Temp.cont}
\alias{EUNITE.Temp.cont}
\docType{data}
\title{Continuation dataset of the temperatures of the EUNITE Competition
%%   ~~ data name/kind ... ~~
}
\description{A dataset with a univariate time series providing 31 points beyond the end of the time series in \code{\link{EUNITE.Temp}} containing average daily temperatures measured in January 1999.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("EUNITE.Temp.cont")}
\format{
  A data frame with 31 observations on the following variable.
  \describe{
    \item{\code{Temperature}}{a numeric vector containing further observations of \code{Temperature} in \code{\link{EUNITE.Temp}} relative to January 1999.}
  }
}
\details{Contains the 31 values corresponding to the average daily temperatures of January 1999 which were to be predicted of \code{\link{EUNITE.Temp}} as demanded by the EUNITE Competition.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{EUNITE 1999, Electricity Load Forecast using Intelligent Adaptive Technology: The EUNITE Network Competition. URL: \url{http://neuron.tuke.sk/competition/index.php}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{B.-J. Chen, M.-W. Chang, and C.-J. Lin, 2004, Load forecasting using support vector Machines: a study on EUNITE competition 2001, IEEE Transactions on Power Systems, v. 19, n. 4 (Nov.), p. 1821-1830.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{EUNITE.Temp}}, \code{\link{EUNITE.Loads}}, \code{\link{EUNITE.Reg}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(EUNITE.Temp.cont)
str(EUNITE.Temp.cont)
plot(ts(EUNITE.Temp.cont))
}
\keyword{datasets}
\keyword{EUNITE}
\keyword{Time Series}
\keyword{Competition}