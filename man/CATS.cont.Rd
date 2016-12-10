\name{CATS.cont}
\alias{CATS.cont}
\docType{data}
\title{Continuation dataset of the time series of the CATS Competition
%%   ~~ data name/kind ... ~~
}
\description{A dataset of providing the 5 blocks of 20 unknown points of the univariate time series in \code{\link{CATS}}
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("CATS.cont")}
\format{
  A data frame with 20 observations on the following 5 variables.
  \describe{
    \item{\code{V1}}{a numeric vector containing the unknown points 981-1000 of the CATS time series in \code{\link{CATS}}}
    \item{\code{V2}}{a numeric vector containing the unknown points 1981-2000 of the CATS time series in \code{\link{CATS}}}
    \item{\code{V3}}{a numeric vector containing the unknown points 2981-3000 of the CATS time series in \code{\link{CATS}}}
    \item{\code{V4}}{a numeric vector containing the unknown points 3981-4000 of the CATS time series in \code{\link{CATS}}}
    \item{\code{V5}}{a numeric vector containing the unknown points 4981-5000 of the CATS time series in \code{\link{CATS}}}
  }
}
\details{Contains the 100 unknown observations which were to be predicted of the CATS time series in (\code{\link{CATS}}) as demanded by the CATS Competition.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{A. Lendasse, E. Oja, O. Simula, M. Verleysen, and others, 2004, Time Series Prediction Competition: The CATS Benchmark, In: IJCNN'2004-International Joint Conference on Neural Networks
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{A. Lendasse, E. Oja, O. Simula, and M. Verleysen, 2007, Time series prediction competition: The CATS benchmark, Neurocomputing, v. 70, n. 13-15 (Aug.), p. 2325-2329.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{CATS}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS.cont)
str(CATS.cont)
plot(ts(CATS.cont["V5"]))
}
\keyword{datasets}
\keyword{CATS}
\keyword{Time Series}
\keyword{Competition}