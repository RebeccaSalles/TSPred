\name{SantaFe.A.cont}
\alias{SantaFe.A.cont}
\docType{data}
\title{Continuation dataset of the time series A of the Santa Fe Time Series Competition
%%   ~~ data name/kind ... ~~
}
\description{A univariate time series providing 100 points beyond the end of the time series A  in \code{\link{SantaFe.A}}.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("SantaFe.A.cont")}
\format{
  A data frame with 100 observations on the following variable.
  \describe{
    \item{\code{V1}}{a numeric vector containing further observations of the univariate time series A of the Santa Fe Time Series Competition in \code{\link{SantaFe.A}}.}
  }
}
\details{Contains the 100 observations which were to be predicted of the time series A (\code{\link{SantaFe.A}}) as demanded by the Santa Fe Time Series Competition.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{The Santa Fe Time Series Competition Data, URL: \url{http://www.comp-engine.org/timeseries/time-series_data_source/source-151/}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{A.S. Weigend, 1993, Time Series Prediction: Forecasting The Future And Understanding The Past. Reading, MA, Westview Press.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{SantaFe.A}}, \code{\link{SantaFe.D}}, \code{\link{SantaFe.D.cont}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A.cont)
str(SantaFe.A.cont)
plot(ts(SantaFe.A.cont))
}
\keyword{datasets}\keyword{datasets}
\keyword{Santa Fe}
\keyword{Time Series}
\keyword{Competition}