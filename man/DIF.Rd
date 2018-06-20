\name{DIF}
\alias{DIF}
\alias{DIF.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Differencing Transformation
%%  ~~function to do ... ~~
}
\description{The \code{DIF()} function returns a simple or seasonal differencing transformation of the provided time series. \code{DIF.rev()} reverses the transformation. Wrapper functions for \code{\link{diff}} and \code{\link[stats]{diffinv}} of the \code{stats} package, respectively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DIF(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = NULL, 
type = c("simple","seasonal"), ...)

DIF.rev(x, lag = ifelse(type=="simple", 1, frequency(x)), differences = 1,
xi, type=c("simple","seasonal"))
}
\arguments{
\item{x}{A numeric vector or univariate time series containing the values to be differenced.}

\item{lag}{Integer indicating the lag parameter. Default set to \code{1} if \code{type = "simple"}, or \code{frequency(x)} if \code{type = "seasonal"}.}

\item{differences}{Integer representing the order of the difference. If \code{NULL}, the order of the difference is automatically selected using \code{\link[forecast]{ndiffs}} (if \code{type = "simple"}) or \code{\link[forecast]{nsdiffs}} (if \code{type = "seasonal"}) from the \code{forecast} package.}

\item{type}{Character string. Indicates if the function should perform simple or seasonal differencing.}

\item{xi}{Numeric vector or time series containing the initial values for the integrals. If missing, zeros are used.}

\item{...}{Additional arguments passed to \code{\link[forecast]{ndiffs}} (if \code{type = "simple"}) or \code{\link[forecast]{nsdiffs}} (if \code{type = "seasonal"}) from the \code{forecast} package.}
}
\value{
\code{x} if \code{differences} is automatically selected, and is not set as greater than \code{0}.

Same as \code{\link{diff}} otherwise.
}
\references{
R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting: principles and practice. OTexts.

R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{BCT}},\code{\link{detrend}}, \code{\link{MAS}}, \code{\link{LT}}, \code{\link{PCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
d <- DIF(CATS[,1], differences = 1)
x <- DIF.rev(as.vector(d), differences = attributes(d)$ndiffs, xi = CATS[1,1])
all(round(x,4)==round(CATS[,1],4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ differencing }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }