\name{marimapar}
\alias{marimapar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get parameters of multiple ARIMA models.
%%  ~~function to do ... ~~
}
\description{The function returns the parameters of a set of automatically fitted ARIMA models, including non-seasonal and seasonal orders and drift. Based on multiple application of the \code{\link{arimapar}} function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
marimapar(TimeSeries, na.action = na.omit, xreg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TimeSeries}{A vector, matrix, or data frame which contains a set of time series used for fitting ARIMA models. Each column corresponds to one time series.
%%     ~~Describe \code{TimeSeries} here~~
}
  \item{na.action}{A function for treating missing values in \code{TimeSeries}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{TimeSeries}.
%%     ~~Describe \code{na.action} here~~
}
  \item{xreg}{A vector, matrix, data frame or times series of external regressors used for fitting all the ARIMA models.  It must have the same number of rows as \code{TimeSeries}. Ignored if \code{NULL}.
%%     ~~Describe \code{xreg} here~~
}
}
\details{See the \code{\link{arimapar}} function.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list of numeric vectors, each one giving the number of AR, MA, seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences of the automatically fitted ARIMA models. It is also presented the value of the fitted drift constants.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{See the \code{\link{arimapar}} function.
%% ~put references to the literature/web site here ~
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{arimapar}}, \code{\link{arimapred}}, \code{\link{marimapred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A)
marimapar(SantaFe.A)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%
%%## The function is currently defined as
%%function (TimeSeries, na.action = na.omit, xreg = NULL) 
%%{
%%    return(lapply(TimeSeries, ARIMAModelInfo, na.action = na.action, 
%%        xreg = xreg))
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ARIMA }% __ONLY ONE__ keyword per line
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ parameters }