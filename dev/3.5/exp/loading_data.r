#Pacotes necessarios
#install.packages(c("TSPred","readr"))

library("TSPred")

data("NN3.A","NN3.A.cont")
data("NN5.A","NN5.A.cont")
data("CATS","CATS.cont")

#Prepare Ipeadata datasets (ipeadata_d,ipeadata_d.cont, ipeadata_m,ipeadata_m.cont)
library(readr)
ipeadata_d_path <- "C:/Users/Rebecca/Desktop/ipeadata_mostused_daily.csv"
ipeadata_m_path <- "C:/Users/Rebecca/Desktop/ipeadata_mostused_monthly.csv"
ipeadata_d <- read_delim(ipeadata_d_path, 
                         ";", escape_double = FALSE,
						 col_types = cols(Data = col_skip(), 
										  `EMBI + Risco-Brasil - - - JP Morgan - JPM366_EMBI366` = col_double(), 
										  `Exporta��es - m�dia semanal - US$ - Minist�rio do Desenvolvimento, Ind�stria e Com�rcio Exterior, Secretaria de Com�rcio Exterior - SECEX366_XVTOT366` = col_double(), 
										  `Importa��es - m�dia semanal - US$ - Minist�rio do Desenvolvimento, Ind�stria e Com�rcio Exterior, Secretaria de Com�rcio Exterior - SECEX366_MVTOT366` = col_double(), 
										  `Taxa de c�mbio - R$ / US$ - comercial - compra - m�dia - R$ - Banco Central do Brasil, Sistema Gerenciador de S�ries Temporais (Bacen Outras/SGS) - GM366_ERC366` = col_double(), 
										  `Taxa de c�mbio - R$ / US$ - comercial - venda - m�dia - R$ - Banco Central do Brasil, Sistema Gerenciador de S�ries Temporais (Bacen Outras/SGS) - GM366_ERV366` = col_double(), 
										  `Taxa de c�mbio - R$ / US$ - paralelo - venda - m�dia - R$ - Valor Econ�mico - GM366_ERPV366` = col_double(), 
										  `Taxa de juros - Over / Selic - (% a.a.) - Banco Central do Brasil, Sistema Gerenciador de S�ries Temporais (Bacen Outras/SGS) - GM366_TJOVER366` = col_double(), 
										  `Taxa de juros - Over / Selic - volatilidade - - - Instituto de Pesquisa Econ�mica Aplicada (IPEA) - GM366_TJOVERV366` = col_double(), 
										  `Taxa de juros - Selic - fixada pelo Copom - (% a.a.) - Banco Central do Brasil, Boletim, Se��o mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM366_TJOVER366` = col_double(), 
										  `Taxa de juros - TR - (% a.m.) - Banco Central do Brasil, Sistema Gerenciador de S�ries Temporais (Bacen Outras/SGS) - GM366_TJTR366` = col_double(), 
										  `Zona do Euro - taxa de c�mbio - euro / US$ - m�dia - Euro - Banco Central do Brasil, Sistema Gerenciador de S�ries Temporais (Bacen Outras/SGS) - GM366_EREURO366` = col_double(), 
										  `�ndice de a��es - Ibovespa - fechamento - - - Bolsa de Valores, Mercadorias e Futuros (BM&FBovespa) - GM366_IBVSP366` = col_double()), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)
ipeadata_m <- read_delim(ipeadata_m_path, 
                         ";", escape_double = FALSE,
						 col_types = cols(Data = col_skip(), 
										  `Exporta��es - pre�os - �ndice (m�dia 2006 = 100) - - - Funda��o Centro de Estudos do Com�rcio Exterior (Funcex) - FUNCEX12_XPT12` = col_double(), 
										  `IGP-DI - geral - �ndice (ago. 1994 = 100) - - - Funda��o Getulio Vargas, Conjuntura Econ�mica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDI12` = col_double(), 
										  `IGP-M - geral - �ndice (ago. 1994 = 100) - - - Funda��o Getulio Vargas, Conjuntura Econ�mica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPM12` = col_double(), 
										  `INPC - geral - �ndice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_INPC12` = col_double(), 
										  `IPCA - geral - �ndice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12` = col_double(), 
										  `Importa��es - pre�os - �ndice (m�dia 2006 = 100) - - - Funda��o Centro de Estudos do Com�rcio Exterior (Funcex) - FUNCEX12_MDPT12` = col_double(), 
										  `Infla��o - IGP-DI - (% a.m.) - Funda��o Getulio Vargas, Conjuntura Econ�mica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDIG12` = col_double(), 
										  `Infla��o - IGP-M - (% a.m.) - Funda��o Getulio Vargas, Conjuntura Econ�mica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPMG12` = col_double(), 
										  `Infla��o - IGP-OG - (% a.m.) - Funda��o Getulio Vargas, Conjuntura Econ�mica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPOGG12` = col_double(), 
										  `Infla��o - INPC - (% a.m.) - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_INPCBR12` = col_double(), 
										  `Infla��o - IPCA - (% a.m.) - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCAG12` = col_double(), 
										  `Infla��o - IPCA-15 - (% a.m.) - Instituto Brasileiro de Geografia e Estat�stica, Sistema Nacional de �ndices de Pre�os ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA15G12` = col_double(), 
										  `PIB - R$ - Banco Central do Brasil, Boletim, Se��o Atividade Econ�mica (Bacen / Boletim / Ativ. Ec.) - BM12_PIB12` = col_double(), 
										  `Sal�rio m�nimo - R$ - Minist�rio do Trabalho e Emprego (MTE) - MTE12_SALMIN12` = col_double(), 
										  `Sal�rio m�nimo real - R$ - Instituto de Pesquisa Econ�mica Aplicada (IPEA) - GAC12_SALMINRE12` = col_double(), 
										  `Taxa de c�mbio - R$ / US$ - comercial - compra - m�dia - R$ - Banco Central do Brasil, Boletim, Se��o Balan�o de Pagamentos (Bacen / Boletim / BP) - BM12_ERC12` = col_double(), 
										  `Taxa de c�mbio - R$ / US$ - comercial - venda - m�dia - R$ - Banco Central do Brasil, Boletim, Se��o Balan�o de Pagamentos (Bacen / Boletim / BP) - BM12_ERV12` = col_double(), 
										  `Taxa de desemprego - RMSP - (%) - Funda��o Sistema Estadual de An�lise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDTGSP12` = col_double(), 
										  `Taxa de desemprego - aberto - RMSP - (%) - Funda��o Sistema Estadual de An�lise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDAGSP12` = col_double(), 
										  `Taxa de desemprego - oculto - RMSP - (%) - Funda��o Sistema Estadual de An�lise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOTSP12` = col_double(), 
										  `Taxa de desemprego - oculto - prec�rio - RMSP - (%) - Funda��o Sistema Estadual de An�lise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOPSP12` = col_double(), 
										  `Taxa de desemprego - refer�ncia: 30 dias - RMs - (%) - Instituto Brasileiro de Geografia e Estat�stica, Pesquisa Mensal de Emprego (IBGE/PME) - obs:  PME foi encerrada em mar�o de 2016, com a divulga��o dos resultados referentes ao m�s de fevereiro de 2016. - PMEN12_TD12` = col_double(), 
										  `Taxa de juros - Over / Selic - (% a.m.) - Banco Central do Brasil, Boletim, Se��o mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM12_TJOVER12` = col_double()), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)

n.ahead_d = 30
ipeadata_d.cont <- do.call(data.frame.na, lapply(ipeadata_d, function(x) tail(na.omit(x), n.ahead_d)) )
ipeadata_d <- do.call(data.frame.na, lapply(ipeadata_d, function(x) head(na.omit(x), length(na.omit(x))-n.ahead_d)) )

n.ahead_m = 12
ipeadata_m.cont <- do.call(data.frame.na, lapply(ipeadata_m, function(x) tail(na.omit(x), n.ahead_m)) )
ipeadata_m <- do.call(data.frame.na, lapply(ipeadata_m, function(x) head(na.omit(x), length(na.omit(x))-n.ahead_m)) )