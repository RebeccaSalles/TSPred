\name{CATS}
\alias{CATS}
\docType{data}
\title{Time series of the CATS Competition
%%   ~~ data name/kind ... ~~
}
\description{A univariate artificial time series presenting 5 non-consecutive blocks of 20 unknown points.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("CATS")}
\format{
  A data frame with 980 observations on the following 5 variables.
  \describe{
    \item{\code{V1}}{a numeric vector containing the known points 1-980 of the CATS time series.}
    \item{\code{V2}}{a numeric vector containing the known points 1001-1980 of the CATS time series.}
    \item{\code{V3}}{a numeric vector containing the known points 2001-2980 of the CATS time series.}
    \item{\code{V4}}{a numeric vector containing the known points 3001-3980 of the CATS time series.}
    \item{\code{V5}}{a numeric vector containing the known points 4001-4980 of the CATS time series.}
  }
}
\details{The CATS Competition presented an artificial time series with 5,000 points, among which 100 are unknown.
The competition proposed that the competitors predicted the 100 unknown values from the given time series, which are grouped into five non-consecutive blocks of 20 successive values (\code{\link{CATS.cont}}).
The unknown points of the series are the 981-1000, 1981-2000, 2981-3000, 3981-4000 and 4981-5000.
The performance evaluation done by the CATS Competition was based on the MSEs computed on the 100 unknown values (E1) and on the 80 first unknown values (E2). The E2 error was considered relevant because some of the proposed methods used interpolation techniques, which cannot be applied in the case of the fifth set of unknown points.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\references{A. Lendasse, E. Oja, O. Simula, M. Verleysen, and others, 2004, Time Series Prediction Competition: The CATS Benchmark, In: IJCNN'2004-International Joint Conference on Neural Networks

A. Lendasse, E. Oja, O. Simula, and M. Verleysen, 2007, Time series prediction competition: The CATS benchmark, Neurocomputing, v. 70, n. 13-15 (Aug.), p. 2325-2329.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{CATS.cont}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
str(CATS)
plot(ts(CATS["V5"]))
}
\keyword{datasets}
\keyword{CATS}
\keyword{Time Series}
\keyword{Competition}