\name{SantaFe.A}
\alias{SantaFe.A}
\docType{data}
\title{Time series A of the Santa Fe Time Series Competition
%%   ~~ data name/kind ... ~~
}
\description{A univariate time series derived from laser-generated data recorded from a Far-Infrared-Laser in a chaotic state.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("SantaFe.A")}
\format{
  A data frame with 1000 observations on the following variable.
  \describe{
    \item{\code{V1}}{a numeric vector containing the observations of the univariate time series A of the Santa Fe Time Series Competition.}
  }
}
\details{The main benchmark of the Santa Fe Time Series Competition, time series A, is composed of a clean low-dimensional nonlinear and stationary time series with 1,000 observations. Competitors were asked to correctly predict the next 100 observations (\code{\link{SantaFe.A.cont}}). The performance evaluation done by the Santa Fe Competition was based on the NMSE errors of prediction found by the competitors.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{The Santa Fe Time Series Competition Data, URL: \url{http://www-psych.stanford.edu/~andreas/Time-Series/SantaFe.html}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{A.S. Weigend, 1993, Time Series Prediction: Forecasting The Future And Understanding The Past. Reading, MA, Westview Press.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{SantaFe.A.cont}}, \code{\link{SantaFe.D}}, \code{\link{SantaFe.D.cont}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A)
str(SantaFe.A)
plot(ts(SantaFe.A))
}
\keyword{datasets}
\keyword{Santa Fe}
\keyword{Time Series}
\keyword{Competition}