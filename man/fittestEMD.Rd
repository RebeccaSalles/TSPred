\name{fittestEMD}
\alias{fittestEMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic prediction with empirical mode decomposition
%%  ~~function to do ... ~~
}
\description{
The function automatically applies an empirical mode decomposition to a provided univariate time series. The resulting components of the decomposed series are used as base for predicting and returning the next n consecutive values of the provided univariate time series using also automatically fitted models (VAR and polynomial regression). It also evaluates fitness and prediction accuracy of the produced models.
}
\usage{
fittestEMD(timeseries, timeseries.test=NULL, h=NULL, max.imf=10, 
boundary=c("none","wave","symmetric","periodic","evenodd"), level=0.95,
na.action=na.omit, rank.by=c("MSE","NMSE","MAPE","sMAPE","MaxError","AIC","AICc","BIC","logLik",
"errors","fitness"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{A vector or univariate time series.
%%     ~~Describe \code{timeseries} here~~
}
  \item{timeseries.test}{A vector or univariate time series containing a continuation for \code{timeseries} with actual values. It is used as a testing set and base for calculation of prediction error measures. Ignored if \code{NULL}.
%%     ~~Describe \code{timeseries.cont} here~~
}
  \item{h}{Number of consecutive values of the time series to be predicted. If \code{h} is \code{NULL}, the number of consecutive values to be predicted is assumed to be equal to the length of \code{timeseries.test}. Required when \code{timeseries.test} is \code{NULL}.
%%     ~~Describe \code{n.ahead} here~~
}
  \item{max.imf}{The maximum number of IMF's. See \code{\link[EMD]{emd}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{boundary}{A vector containing character strings indicating boundary conditions for the empirical mode decomposition. If \code{length(boundary)>1}, the boundary used for generating the return of the function is automatically selected. If \code{NULL}, all supported boundaries are considered for automatic selection. See 'Details'. For more details on all the supported boundaries see \code{\link[EMD]{emd}}.
%%     ~~Describe \code{maxorder} here~~
}
  \item{level}{Confidence level for prediction intervals. See \code{\link[stats]{predict.lm}} and \code{\link[vars]{predict}}.
%%     ~~Describe \code{na.action} here~~
}
  \item{na.action}{A function for treating missing values in \code{timeseries} and \code{timeseries.test}. The default function is \code{\link{na.omit}}, which omits any missing values found in \code{timeseries} or \code{timeseries.test}.
%%     ~~Describe \code{na.action} here~~
}
  \item{rank.by}{Character string. Criteria used for ranking candidate decompositions/models/predictions generated during parameter selection. See 'Details'.
%%     ~~Describe \code{filtered} here~~
}
}
\details{The function produces an empirical mode decomposition of \code{timeseries}. See the \code{\link[EMD]{emd}} function. The IMF's and residue series resulting from the decomposition are separately used as base for model fitting and prediction. The IMF's are fitted and predicted using a \code{\link[vars]{VAR}} model. The residue series is fitted and predicted using a polynomial regression model provided by \code{\link{fittestPolyR}}. The set of predictions for all IMF's and residue series are then reversed transformed in order to produce the next \code{h} consecutive values of the provided univariate time series in \code{timeseries}. See the \code{\link[EMD]{emd.pred}} function.

If \code{length(boundary)>1} or \code{boundary=NULL}, it is automatically selected. For that, a set of empirical mode decompositions with different options of boundary condition is generated and used for model fitting and prediction. Also, the function automatically selects the meaningful IMF's of a decomposition. For that, the function produces \code{\link[vars]{VAR}} models for different selections of meaningful IMF's according to the possible intervals \code{i:nimf} for \code{i=1,...,(nimf-1)}, where \code{nimf} is the number of IMF's in a decomposition. The options of \code{boundary} and/or meaningful IMF's of a decomposition which generate the best ranked model fitness/predictions acoording to the criteria in \code{rank.by} are selected.

The ranking criteria in \code{rank.by} may be set as a prediction error measure (such as \code{\link{MSE}}, \code{\link{NMSE}}, \code{\link{MAPE}}, \code{\link{sMAPE}} or \code{\link{MAXError}}), or as a fitness criteria (such as \code{\link{AIC}}, \code{\link{AICc}}, \code{\link{BIC}} or \code{\link{logLik}}). In the former case, the candidate empirical mode decompositions are used for time series prediction and the error measures are calculated by means of a cross-validation process. In the latter case, the component series of the candidate decompositions are modeled and model fitness criteria are calculated based on all observations in \code{timeseries}. In particular, the fitness criteria calculated for ranking the candidate decompositions correspond to the \code{\link[vars]{VAR}} models produced for the IMF's.

If \code{rank.by} is set as \code{"errors"} or \code{"fitness"}, the candidate decompositions are ranked by all the mentioned prediction error measures or fitness criteria, respectively. The wheight of the ranking criteria is equally distributed. In this case, a \code{rank.position.sum} criterion is produced for ranking the candidate decompositions. The \code{rank.position.sum} criterion is calculated as the sum of the rank positions of a decomposition (1 = 1st position = better ranked model, 2 = 2nd position, etc.) on each calculated ranking criteria.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A list with components:
\item{emd}{Same as \code{\link[EMD]{emd}}. Contains the empirical mode decomposition of \code{timeseries}.}
\item{meaningfulImfs}{Character string indicating the automatically selected meaningful IMF's of the decomposition.}
\item{boundary}{The provided or automatically selected boundary condition of the decomposition.}
\item{varImfs}{The \code{\link[vars]{VAR}} model fitted to the meaningful IMF's of the empirical mode decomposition.}
\item{polyRresidue}{The polynomial regression model fitted to the residue of the decomposition.}
\item{AICc}{Numeric value of the computed AICc criterion of the fitted VAR model in \code{varImfs}.}
\item{AIC}{Numeric value of the computed AIC criterion of the fitted VAR model in \code{varImfs}.}
\item{BIC}{Numeric value of the computed BIC criterion of the fitted VAR model in \code{varImfs}.}
\item{logLik}{Numeric value of the computed log-likelihood of the fitted VAR model in \code{varImfs}.}
\item{pred}{A list with the components \code{mean}, \code{lower} and \code{upper}, containing the predictions based on the best evaluated decomposition and the lower and upper limits for prediction intervals, respectively. All components are time series.}
\item{MSE}{Numeric value of the resulting MSE error of prediction. Require \code{timeseries.test}.}
\item{NMSE}{Numeric value of the resulting NMSE error of prediction. Require \code{timeseries.test}.}
\item{MAPE}{Numeric value of the resulting MAPE error of prediction. Require \code{timeseries.test}.}
\item{sMAPE}{Numeric value of the resulting sMAPE error of prediction. Require \code{timeseries.test}.}
\item{MaxError}{Numeric value of the maximal error of prediction. Require \code{timeseries.test}.}
\item{rank.val}{Data.frame with the fitness or prediction accuracy criteria computed based on all candidate decompositions ranked by \code{rank.by}. It has the attribute \code{"ranked.models"}, which is a list of \code{\link[vars]{VAR}} models for all the candidate decompositions, also ranked by \code{rank.by}.}
\item{rank.by}{Ranking criteria used for ranking candidate decompositions and producing \code{rank.val}.}
}
\references{
Kim, D., Paek, S. H., & Oh, H. S. (2008). A Hilbert-Huang transform approach for predicting cyber-attacks. Journal of the Korean Statistical Society, 37(3), 277-283.
%% ~put references to the literature/web site here ~
}
\author{
Rebecca Pontes Salles
%%  ~~who you are~~
}
\seealso{\code{\link{fittestWavelet}}, \code{\link{fittestMAS}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(CATS)
\dontrun{
femd <- fittestEMD(CATS[,1],h=20)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ emd }% __ONLY ONE__ keyword per line
\keyword{ decomposition }
\keyword{ transform }
\keyword{ automatic }
\keyword{ fitting }
\keyword{ adjustment }
\keyword{ prediction }
\keyword{ evaluation }
\keyword{ criterion }
\keyword{ errors }
\keyword{ time series }