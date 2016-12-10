\name{EUNITE.Reg.cont}
\alias{EUNITE.Reg.cont}
\docType{data}
\title{Continuation dataset of the electrical loads regressors of the EUNITE Competition
%%   ~~ data name/kind ... ~~
}
\description{A dataset of regressor variables for electrical loads measured in January 1999, providing 31 points beyond the end of the data in \code{\link{EUNITE.Reg}}.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("EUNITE.Reg.cont")}
\format{
  A data frame with 31 observations on the following 2 variables.
  \describe{
    \item{\code{Holiday}}{a numeric vector containing further data of the variable \code{Holiday} in \code{\link{EUNITE.Reg}} relative to January 1999.}
    \item{\code{Weekday}}{a numeric vector containing further data of the variable \code{Weekday} in \code{\link{EUNITE.Reg}} relative to January 1999.}
  }
}
\details{Contains the 31 values of the regressors used for the prediction of the daily maximum electrical loads of January 1999 from \code{\link{EUNITE.Loads}} as demanded by the EUNITE Competition.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{EUNITE 1999, Electricity Load Forecast using Intelligent Adaptive Technology: The EUNITE Network Competition. URL: \url{http://neuron.tuke.sk/competition/index.php}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{B.-J. Chen, M.-W. Chang, and C.-J. Lin, 2004, Load forecasting using support vector Machines: a study on EUNITE competition 2001, IEEE Transactions on Power Systems, v. 19, n. 4 (Nov.), p. 1821-1830.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{EUNITE.Reg}}, \code{\link{EUNITE.Loads}}, \code{\link{EUNITE.Temp}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(EUNITE.Reg.cont)
str(EUNITE.Reg.cont)
}
\keyword{datasets}
\keyword{EUNITE}
\keyword{Time Series}
\keyword{Competition}