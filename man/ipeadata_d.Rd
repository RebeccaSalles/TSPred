\name{ipeadata_d}
\alias{ipeadata_d}
\alias{ipeadata_d.cont}
\docType{data}
\title{The Ipea Most Requested Dataset (daily)
}
\description{
The Institute of Applied Economic Research of Brazil (Ipea) (Ipea, 2017) is a public institution of Brazil that provides support to the federal government with regard to public policies: fiscal, social, and economic. Ipea provides public datasets derived from real economic and financial data of the world.

The \code{ipeadata_d} dataset is provided by Ipea. It comprehends the most requested time series collected in daily rates. The \code{ipeadata_d} dataset comprehend observations of exchange rates (R$/US$), exports/imports prices, interest rates, and more, measured from 1962 to September of 2017.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ipeadata_d")
data("ipeadata_d.cont")}
\format{
  The \code{ipeadata_d} dataset contains 12 time series of 901 to 8154 observations.
  The 12 time series are provided as the following variables of a data frame.
  \describe{
    \item{[1]}{Stock Index: São Paulo Stock Exchange  - closed - - - Bolsa de Valores, Mercadorias e Futuros (BM&FBovespa) - GM366_IBVSP366.}
    \item{[2]}{Exchange rate - R$ / US$ - commercial - purchase - mean - R$ - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_ERC366.}
    \item{[3]}{Euro area - exchange rate - euro / US$ - mean - Euro - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_EREURO366.}
    \item{[4]}{Exchange rate - R$ / US$ - parallel - selling - mean - R$ - Economic value - GM366_ERPV366.}
    \item{[5]}{	Exchange rate - R$ / US$ - commercial - selling - mean - R$ - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_ERV366.}
    \item{[6]}{Interest Rate: Over / Selic - (\% p.a.) - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_TJOVER366.}
    \item{[7]}{Interest rate - TR - (\% p.m.) - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_TJTR366.}
    \item{[8]}{Imports - weekly mean - US$ - Ministério do Desenvolvimento, Indústria e Comércio, Secretaria de Comércio Exterior (MDIC/Secex) - SECEX366_MVTOT366.}
    \item{[9]}{Exports - weekly mean - US$ - Ministério do Desenvolvimento, Indústria e Comércio, Secretaria de Comércio Exterior (MDIC/Secex) - SECEX366_XVTOT366.}
    \item{[10]}{EMBI + Risco-Brasil - - - JP Morgan - JPM366_EMBI366.}
    \item{[11]}{Interest rate - Selic - fixed by Copom - (\% p.a.) - Banco Central do Brasil, Boletim, Seção mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM366_TJOVER366.}
    \item{[12]}{Interest Rate: Over / Selic   - - - Instituto de Pesquisa Econômica Aplicada (Ipea) - GM366_TJOVERV366.}
  }
}
\details{The data had missing data removed by the function \code{\link{na.omit}}.

\code{ipeadata_d.cont} provide 30 points beyond the end of the time series in \code{ipeadata_d}. Intended for use as testing set.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{Ipea, Ipeadata. Macroeconomic and regional data, Technical Report, \url{http://www.ipeadata.gov.br}, 2017. The "Most request series" section and filtered by "Frequency" equal to "Daily".
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{Ipea, Ipeadata. Macroeconomic and regional data, Technical Report, \url{http://www.ipeadata.gov.br}, 2017.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{ipeadata_m}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ipeadata_d)
str(ipeadata_d)
plot(ts(ipeadata_d[1]))
}
\keyword{datasets}
\keyword{ipeadata}
\keyword{time teries}