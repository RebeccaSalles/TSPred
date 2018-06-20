\name{LT}
\alias{LT}
\alias{LT.rev}
\alias{LT10}
\alias{LT10.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logarithmic Transformation
%%  ~~function to do ... ~~
}
\description{The \code{LT()} function returns a natural logarithmic transformation of the provided time series. Analogously, \code{LT10()} returns a common (i.e., base 10) logarithmic transformation. \code{LT.rev()} and \code{LT10.rev()} reverse the transformations, respectively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LT(x)
LT.rev(x)
LT10(x)
LT10.rev(x)
}
\arguments{
\item{x}{A numeric vector or univariate time series of class \code{ts}.}
}
\value{
A vector of the same length as x containing the transformed values.
}
\references{
R. H. Shumway, D. S. Stoffer, Time Series Analysis and Its Applications:
With R Examples, Springer, New York, NY, 4 edition, 2017.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{DIF}},\code{\link{detrend}}, \code{\link{MAS}}, \code{\link{BCT}}, \code{\link{PCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(NN5.A)
LT(NN5.A[,10])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ logarithm }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }