\name{PCT}
\alias{PCT}
\alias{PCT.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Percentage Change Transformation
%%  ~~function to do ... ~~
}
\description{The \code{PCT()} function returns a transformation of the provided time series using a Percentage Change transformation. \code{PCT.rev()} reverses the transformation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCT(x)

PCT.rev(p,x0)
}
\arguments{
\item{x}{A numeric vector or univariate time series of class \code{ts}.}

\item{p}{A numeric vector or univariate time series of percentage changes. Possibly returned by \code{PCT()}.}

\item{x0}{Initial value/observation of \code{x} (\code{x[1]}). First known non-transformed value used to recursively obtain the original series.}
}
\details{
The Percentage Change transformation is given approximately by \deqn{}{log( x[2:n] / x[1:(n-1)] ) = log( x[2:n] ) - log( x[1:(n-1)] ) } where \code{n=length(x)}.
}
\value{
A vector of length \code{length(x)-1} containing the transformed values.
}
\references{
R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{DIF}},\code{\link{detrend}}, \code{\link{MAS}}, \code{\link{LT}}, \code{\link{BCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(NN5.A)
ts <- na.omit(NN5.A[,10])
length(ts)

pct <- PCT(ts)
length(pct)

x0 <- ts[1]
pct.rev <- PCT.rev(pct,x0)
x <- c(x0,pct.rev)

all(round(x,4)==round(ts,4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ percentage change }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }