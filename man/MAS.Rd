\name{MAS}
\alias{MAS}
\alias{MAS.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moving average smoothing
%%  ~~function to do ... ~~
}
\description{The \code{MAS()} function returns a simple moving average smoother of the provided time series. \code{MAS.rev()} reverses the transformation(smoothing) process.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MAS(x,order)

MAS.rev(xm,xinit,order,addinit=TRUE)
}
\arguments{
\item{x}{A numeric vector or univariate time series.}

\item{order}{Order of moving average smoother.}

\item{xm}{A numeric vector or univariate time series that was moving average smoothed. Possibly returned by \code{MAS()}.}

\item{xinit}{Initial \code{order-1} values/observations used for reverse smoothing. First \code{order-1} known non-transformed values used to recursively obtain the original series.}

\item{addinit}{If \code{TRUE}, \code{xinit} is included in the return.}
}
\details{
The moving average smoother transformation is given by \deqn{}{(1/k) * ( x[t] + x[t+1] + ... + x[t+k-1] )} where \code{k=order}, \code{t} assume values in the range \code{1:(n-k+1)}, and \code{n=length(x)}. See also the \code{\link[forecast]{ma}} of the \code{forecast} package.
}
\value{
Numerical time series of length \code{length(x)-order+1} containing the simple moving average smoothed values.
}
\references{
R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{DIF}},\code{\link{detrend}}, \code{\link{PCT}}, \code{\link{LT}}, \code{\link{BCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
\dontrun{
#automatically select order of moving average
order <- fittestMAS(CATS[,1],h=20,model="arima")$order
}

order <- 5
m <- MAS(CATS[,1],order=order)

xinit <- head(CATS[,1],order-1)
x <- MAS.rev(m,xinit,order,addinit=TRUE)

all(round(x,4)==round(CATS[,1],4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ moving average }
\keyword{ smoother }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }