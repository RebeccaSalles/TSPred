\name{arimaparameters}
\alias{arimaparameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get ARIMA model parameters
%%  ~~function to do ... ~~
}
\description{The function returns the parameters of a fitted ARIMA model, including non-seasonal and seasonal orders and drift.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arimaparameters(fit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{An object of class "Arima" containing a fitted ARIMA model.
}
}
\details{The \code{fit} object could possibly be the result of \code{\link[forecast]{auto.arima}} or \code{\link[forecast]{Arima}} of the \code{forecast} package, or \code{\link[stats]{arima}} of the \code{stats} package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list giving the number of AR, MA, seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences of the provided ARIMA model. The value of the fitted drift constant is also presented.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting: principles and practice. OTexts.

R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
%% ~put references to the literature/web site here ~
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}

\seealso{\code{\link{fittestArima},\code{\link{arimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A)
arimaparameters(forecast::auto.arima(SantaFe.A[,1]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ parameters }