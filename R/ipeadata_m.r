#' The Ipea Most Requested Dataset (monthly)
#' 
#' The Institute of Applied Economic Research of Brazil (Ipea) (Ipea, 2017) is
#' a public institution of Brazil that provides support to the federal
#' government with regard to public policies: fiscal, social, and economic.
#' Ipea provides public datasets derived from real economic and financial data
#' of the world.
#' 
#' The \code{ipeadata_m} dataset is provided by Ipea. It comprehends the most
#' requested time series collected in monthly rates. The \code{ipeadata_m}
#' dataset comprehend observations of exchange rates (R$/US$), exports/imports
#' prices, interest rates, minimum wage, unemployment rate, and more, measured
#' from 1930 to September of 2017. 
#' 
#' The data had missing data removed by the function \code{\link{na.omit}}.
#' 
#' \code{ipeadata_m.cont} provide 12 points beyond the end of the time series
#' in \code{ipeadata_m}. Intended for use as testing set. 
#' 
#' @name ipeadata_m
#' @aliases ipeadata_m ipeadata_m.cont
#' @docType data
#' @format The \code{ipeadata_m} dataset contains 23 time series of 156 to 1019
#' observations.  The 23 time series are provided as the following variables of
#' a data frame.  \describe{ \item{BM12_ERC12}{Exchange rate - Brazilian real
#' (R$) / US dollar (US$) - purchase - average - R$ - Bacen / Boletim / BP.}
#' \item{BM12_ERV12}{Exchange rate - Brazilian real (R$) / US dollar (US$) -
#' selling - average - R$ - Bacen / Boletim / BP.} \item{IGP12_IGPDI12}{IGP-DI
#' - general price index domestic supply (aug 1994 = 100) - FGV/Conj. Econ. -
#' IGP.} \item{FUNCEX12_MDPT12}{Imports - prices - index (average 2006 = 100) -
#' Funcex.} \item{FUNCEX12_XPT12}{Exports - prices - index (average 2006 = 100)
#' - Funcex.} \item{PRECOS12_INPC12}{INPC - national consumer price index (dec
#' 1993 = 100) - IBGE/SNIPC.} \item{PRECOS12_INPCBR12}{INPC - national consumer
#' price index - growth rate - (\% p.m.) - IBGE/SNIPC.}
#' \item{PRECOS12_IPCA12}{IPCA - extended consumer price index (dec 1993 = 100)
#' - IBGE/SNIPC.} \item{SEADE12_TDAGSP12}{Unemployment rate - open - RMSP -
#' (\%) - Seade/PED.} \item{SEADE12_TDOTSP12}{Unemployment rate - hidden - RMSP
#' - (\%) - Seade/PED.} \item{SEADE12_TDOPSP12}{Unemployment rate - hidden -
#' precarious - RMSP - (\%) - Seade/PED.} \item{GAC12_SALMINRE12}{Real minimum
#' wage - R$ - Ipea.} \item{IGP12_IGPM12}{IGP-M - general price index at market
#' prices (aug 1994 = 100) - FGV/Conj. Econ. - IGP.}
#' \item{PRECOS12_IPCAG12}{IPCA - extended consumer price index - growth rate -
#' (\% p.m.) - IBGE/SNIPC.} \item{IGP12_IGPDIG12}{IGP-DI - general price index
#' domestic supply - growth rate - (\% p.m.) - FGV/Conj. Econ. - IGP.}
#' \item{IGP12_IGPMG12}{IGP-M - general price index at market prices - growth
#' rate - (\% p.m.) - FGV/Conj. Econ. - IGP.} \item{IGP12_IGPOGG12}{IGP-OG -
#' general price index overall supply - growth rate - (\% p.m.) - FGV/Conj.
#' Econ. - IGP.} \item{PRECOS12_IPCA15G12}{IPCA 15 - extended consumer price
#' index - growth rate - (\% p.m.) - IBGE/SNIPC.} \item{[BM12_PIB12}{GDP - R$ -
#' Bacen / Boletim / Ativ. Ec..} \item{MTE12_SALMIN12}{Minimum wage - R$ -
#' MTE.} \item{BM12_TJOVER12}{Interest rate - Overnight/Selic - (\% p.m.) -
#' Bacen/Boletim/M. Finan..} \item{SEADE12_TDTGSP12}{Unemployment rate - Sao
#' Paulo - (\%) - Seade/PED.} \item{PMEN12_TD12}{Unemployment rate - reference:
#' 30 days - RMs - (\%) - IBGE/PME - obs: PME closed in 2016-mar.} }
#' @seealso \code{\link{ipeadata_d}} ~
#' @references Ipea, Ipeadata. Macroeconomic and regional data, Technical
#' Report, \url{http://www.ipeadata.gov.br}, 2017. 
#' @source Ipea, Ipeadata. Macroeconomic and regional data, Technical Report,
#' \url{http://www.ipeadata.gov.br}, 2017. The "Most request series" section
#' and filtered by "Frequency" equal to "Monthly". 
#' @keywords datasets ipeadata time teries
#' @examples
#' 
#' data(ipeadata_m)
#' str(ipeadata_m)
#' plot(ts(ipeadata_m[1]))
#' 
"ipeadata_m"
#> [1] "ipeadata_m"