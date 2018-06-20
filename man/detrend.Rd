\name{detrend}
\alias{detrend}
\alias{detrend.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detrending Transformation
%%  ~~function to do ... ~~
}
\description{The \code{detrend()} function performs a detrending transformation and removes a trend from the provided time series. \code{detrend.rev()} reverses the transformation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
detrend(x,trend)

detrend.rev(x,trend)
}
\arguments{
\item{x}{A numeric vector or univariate time series of class \code{ts}.}

\item{trend}{A numeric vector or univariate time series containing the trend to be removed. Generally, the fitted values of a model object.}
}
\value{
A vector of the same length as \code{x} containing the residuals of \code{x} after trend removal.
}
\references{
R. H. Shumway, D. S. Stoffer, Time Series Analysis and Its Applications:
With R Examples, Springer, New York, NY, 4 edition, 2017.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{DIF}},\code{\link{BCT}}, \code{\link{MAS}}, \code{\link{LT}}, \code{\link{PCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS,CATS.cont)
fpoly <- fittestPolyR(CATS[,1],h=20)
trend <- fitted(fpoly$model)

residuals <- detrend(CATS[,1],trend)
x <- detrend.rev(residuals,trend)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ detrending }
\keyword{ trend }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }