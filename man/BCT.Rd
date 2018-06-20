\name{BCT}
\alias{BCT}
\alias{BCT.rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Box Cox Transformation
%%  ~~function to do ... ~~
}
\description{The \code{BCT()} function returns a transformation of the provided time series using a Box-Cox transformation. \code{BCT.rev()} reverses the transformation. Wrapper functions for \code{\link[forecast]{BoxCox}} and \code{\link[forecast]{InvBoxCox}} of the \code{forecast} package, respectively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BCT(x,lambda=NULL,...)

BCT.rev(x,lambda,...)
}
\arguments{
\item{x}{A numeric vector or univariate time series of class \code{ts}.}

\item{lambda}{Box-Cox transformation parameter. If \code{NULL}, \code{lambda} is selected using \code{\link[forecast]{BoxCox.lambda}} of the \code{forecast} package.}

\item{...}{Additional arguments passed to the \code{\link[forecast]{BoxCox.lambda}} function for \code{BCT()}, and to the \code{\link[forecast]{InvBoxCox}} function for \code{BCT.rev()}.}
}
\details{
If \code{lambda} is not \code{0}, the Box-Cox transformation is given by \deqn{f_\lambda(x) =\frac{x^\lambda -
1}{\lambda}}{(x^lambda - 1)/lambda} If \eqn{\lambda=0}{lambda=0}, the Box-Cox transformation is given by
\deqn{f_0(x)=\log(x)}{log(x)}.
}
\value{
A vector of the same length as x containing the transformed values.
}
\references{
Box, G. E. P. and Cox, D. R. (1964) An analysis of
transformations. \emph{JRSS B} \bold{26} 211--246.
}
\author{Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{DIF}},\code{\link{detrend}}, \code{\link{MAS}}, \code{\link{LT}}, \code{\link{PCT}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
BCT(CATS[,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Box-Cox }
\keyword{ transform }% __ONLY ONE__ keyword per line
\keyword{ time series }