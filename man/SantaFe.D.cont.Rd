\name{SantaFe.D.cont}
\alias{SantaFe.D.cont}
\docType{data}
\title{Continuation dataset of the time series D of the Santa Fe Time Series Competition
%%   ~~ data name/kind ... ~~
}
\description{A univariate time series providing 500 points beyond the end of the time series D  in \code{\link{SantaFe.D}}.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("SantaFe.D.cont")}
\format{
  A data frame with 500 observations on the following variable.
  \describe{
    \item{\code{V1}}{a numeric vector containing further observations of the univariate time series D of the Santa Fe Time Series Competition in \code{\link{SantaFe.D}}.}
  }
}
\details{Contains the 500 observations which were to be predicted of the time series D (\code{\link{SantaFe.D}}) as demanded by the Santa Fe Time Series Competition.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{The Santa Fe Time Series Competition Data, URL: \url{http://www-psych.stanford.edu/~andreas/Time-Series/SantaFe.html}.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{A.S. Weigend, 1993, Time Series Prediction: Forecasting The Future And Understanding The Past. Reading, MA, Westview Press.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{SantaFe.D}}, \code{\link{SantaFe.A}}, \code{\link{SantaFe.A.cont}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.D.cont)
str(SantaFe.D.cont)
plot(ts(SantaFe.D.cont))
}
\keyword{datasets}
\keyword{Santa Fe}
\keyword{Time Series}
\keyword{Competition}