\name{SantaFe.D}
\alias{SantaFe.D}
\docType{data}
\title{Time series D of the Santa Fe Time Series Competition
%%   ~~ data name/kind ... ~~
}
\description{A univariate computer-generated time series.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("SantaFe.D")}
\format{
  A data frame with 100000 observations on the following variable.
  \describe{
    \item{\code{V1}}{a numeric vector containing the observations of the univariate time series D of the Santa Fe Time Series Competition.}
  }
}
\details{One of the benchmarks of the Santa Fe Time Series Competition, time series D, is composed of a four-dimensional nonlinear time series with non-stationary properties and 100,000 observations. Competitors were asked to correctly predict the next 500 observations of this time series (\code{\link{SantaFe.D.cont}}). The performance evaluation done by the Santa Fe Competition was based on the NMSE errors of prediction found by the competitors.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{The Santa Fe Time Series Competition Data, URL: \url{http://www-psych.stanford.edu/~andreas/Time-Series/SantaFe.html}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{A.S. Weigend, 1993, Time Series Prediction: Forecasting The Future And Understanding The Past. Reading, MA, Westview Press.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{SantaFe.D.cont}}, \code{\link{SantaFe.A}}, \code{\link{SantaFe.A.cont}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.D)
str(SantaFe.D)
plot(ts(SantaFe.D),xlim=c(1,2000))
}
\keyword{datasets}
\keyword{Santa Fe}
\keyword{Time Series}
\keyword{Competition}