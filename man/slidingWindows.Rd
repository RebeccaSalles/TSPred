\name{slidingWindows}
\alias{slidingWindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generating sliding windows of data
%%  ~~function to do ... ~~
}
\description{
The function extracts all possible subsequences (of the same length) of a time series (or numeric vector), generating a set of sliding windows of data, often used to train machine learning methods.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
slidingWindows(timeseries,swSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series from which the sliding windows are to be extracted.
%%     ~~Describe \code{timeseries} here~~
}
  \item{swSize}{Numeric value of the required size/length of each sliding window.
%%     ~~Describe \code{timeseries.cont} here~~
}
}
\details{The function returns all (overlapping) subsequences of size \code{swSize} of \code{timeseries}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric matrix of size (length(\code{timeseries})-\code{swSize}+1) by \code{swSize}, where each line is a sliding window.
}
\references{
Lampert, C. H., Blaschko, M. B., and Hofmann, T. (2008). Beyond sliding windows: Object localization by efficient subwindow search. In Computer Vision and Pattern Recognition, 2008. CVPR 2008. IEEE Conference on, pages 1–8. IEEE.

Keogh, E. and Lin, J. (2005). Clustering of time-series subsequences is meaningless: Implications for previous and future research. Knowledge and Information Systems, 8(2):154–177.
%% ~put references to the literature/web site here ~
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\examples{
data("CATS")
SW <- slidingWindows(CATS[,1],4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sliding }% __ONLY ONE__ keyword per line
\keyword{ windows }