\name{sMAPE}
\alias{sMAPE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sMAPE error of prediction
%%  ~~function to do ... ~~
}
\description{The function calculates the sMAPE error between actual and predicted values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sMAPE(actual, prediction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{actual}{A vector or univariate time series containing actual values for a time series that are to be compared against its respective predictions.
%%     ~~Describe \code{actual} here~~
}
  \item{prediction}{A vector or univariate time series containing time series predictions that are to be compared against the values in \code{actual}.
%%     ~~Describe \code{forecast} here~~
}
}
\value{A numeric value of the sMAPE error of prediction.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Z. Chen and Y. Yang, 2004, Assessing forecast accuracy measures, Preprint Series, n. 2004-2010, p. 2004-10. 
%% ~put references to the literature/web site here ~
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{MAPE}}, \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAXError}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(SantaFe.A,SantaFe.A.cont)
pred <- marimapred(SantaFe.A,n.ahead=100)
sMAPE(SantaFe.A.cont[,1], pred)
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%%## The function is currently defined as
%%function (actual, prediction) 
%%{
%%    if (length(actual) != length(prediction)) 
%%        stop("actual and prediction have different lengths")
%%    n <- length(actual)
%%    res <- (1/n) * sum(abs(actual - prediction)/((abs(actual) + 
%%        abs(prediction))/2))
%%    res
%%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SMAPE }
\keyword{ prediction }% __ONLY ONE__ keyword per line
\keyword{ error }