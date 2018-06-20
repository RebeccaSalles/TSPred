\name{ipeadata_m}
\alias{ipeadata_m}
\alias{ipeadata_m.cont}
\docType{data}
\title{The Ipea Most Requested Dataset (monthly)
}
\description{
The Institute of Applied Economic Research of Brazil (Ipea) (Ipea, 2017) is a public institution of Brazil that provides support to the federal government with regard to public policies: fiscal, social, and economic. Ipea provides public datasets derived from real economic and financial data of the world.

The \code{ipeadata_m} dataset is provided by Ipea. It comprehends the most requested time series collected in monthly rates. The \code{ipeadata_m} dataset comprehend observations of exchange rates (R$/US$), exports/imports prices, interest rates, minimum wage, unemployment rate, and more, measured from 1930 to September of 2017.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ipeadata_m")
data("ipeadata_m.cont")}
\format{
  The \code{ipeadata_m} dataset contains 23 time series of 156 to 1019 observations.
  The 23 time series are provided as the following variables of a data frame.
  \describe{
    \item{[1]}{Exchange rate - Brazilian real (R$) / US dollar (US$) - purchase - average - R$ - Banco Central do Brasil, Boletim, Seção Balanço de Pagamentos (Bacen / Boletim / BP) - BM12_ERC12.}
    \item{[2]}{Exchange rate - Brazilian real (R$) / US dollar (US$) - selling - average - R$ - Banco Central do Brasil, Boletim, Seção Balanço de Pagamentos (Bacen / Boletim / BP) - BM12_ERV12.}
    \item{[3]}{IGP-DI - general price index domestic supply (aug 1994 = 100) - - - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDI12.}
    \item{[4]}{Imports - prices - index (average 2006 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_MDPT12.}
    \item{[5]}{Exports - prices - index (average 2006 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_XPT12.}
    \item{[6]}{INPC - national consumer price index (dec 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_INPC12.}
    \item{[7]}{INPC - national consumer price index - growth rate - (\% p.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_INPCBR12.}
    \item{[8]}{IPCA - extended consumer price index (dec 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12.}
    \item{[9]}{Unemployment rate - open - RMSP - (\%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDAGSP12.}
    \item{[10]}{Unemployment rate - hidden - RMSP - (\%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOTSP12.}
    \item{[11]}{Unemployment rate - hidden - precarious - RMSP - (\%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOPSP12.}
    \item{[12]}{Real minimum wage - R$ - Instituto de Pesquisa Econômica Aplicada (Ipea) - GAC12_SALMINRE12	.}
    \item{[13]}{IGP-M - general price index at market prices (aug 1994 = 100) - - - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPM12.}
    \item{[14]}{IPCA - extended consumer price index - growth rate - (\% p.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCAG12.}
    \item{[15]}{IGP-DI - general price index domestic supply - growth rate - (\% p.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDIG12.}
    \item{[16]}{IGP-M - general price index at market prices - growth rate - (\% p.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPMG12.}
    \item{[17]}{IGP-OG - general price index overall supply - growth rate - (\% p.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPOGG12.}
    \item{[18]}{IPCA 15 - extended consumer price index - growth rate - (\% p.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA15G12.}
    \item{[19]}{GDP - R$ - Banco Central do Brasil, Boletim, Seção Atividade Econômica (Bacen / Boletim / Ativ. Ec.) - BM12_PIB12.}
    \item{[20]}{Minimum wage - R$ - Ministério do Trabalho e Emprego (MTE) - MTE12_SALMIN12.}
    \item{[21]}{Interest rate - Overnight/Selic - (\% p.m.) - Banco Central do Brasil, Boletim, Seção mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM12_TJOVER12.}
    \item{[22]}{Unemployment rate - São Paulo - (\%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDTGSP12.}
    \item{[23]}{Unemployment rate - reference: 30 days - RMs - (\%) - Instituto Brasileiro de Geografia e Estatística, Pesquisa Mensal de Emprego (IBGE/PME) - obs:  PME closed in 2016-mar. - PMEN12_TD12.}
  }
}
\details{The data had missing data removed by the function \code{\link{na.omit}}.

\code{ipeadata_m.cont} provide 12 points beyond the end of the time series in \code{ipeadata_m}. Intended for use as testing set.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{Ipea, Ipeadata. Macroeconomic and regional data, Technical Report, \url{http://www.ipeadata.gov.br}, 2017. The "Most request series" section and filtered by "Frequency" equal to "Monthly".
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{Ipea, Ipeadata. Macroeconomic and regional data, Technical Report, \url{http://www.ipeadata.gov.br}, 2017.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{ipeadata_d}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ipeadata_m)
str(ipeadata_m)
plot(ts(ipeadata_m[1]))
}
\keyword{datasets}
\keyword{ipeadata}
\keyword{time teries}