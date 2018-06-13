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
										  `Exportações - média semanal - US$ - Ministério do Desenvolvimento, Indústria e Comércio Exterior, Secretaria de Comércio Exterior - SECEX366_XVTOT366` = col_double(), 
										  `Importações - média semanal - US$ - Ministério do Desenvolvimento, Indústria e Comércio Exterior, Secretaria de Comércio Exterior - SECEX366_MVTOT366` = col_double(), 
										  `Taxa de câmbio - R$ / US$ - comercial - compra - média - R$ - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_ERC366` = col_double(), 
										  `Taxa de câmbio - R$ / US$ - comercial - venda - média - R$ - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_ERV366` = col_double(), 
										  `Taxa de câmbio - R$ / US$ - paralelo - venda - média - R$ - Valor Econômico - GM366_ERPV366` = col_double(), 
										  `Taxa de juros - Over / Selic - (% a.a.) - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_TJOVER366` = col_double(), 
										  `Taxa de juros - Over / Selic - volatilidade - - - Instituto de Pesquisa Econômica Aplicada (IPEA) - GM366_TJOVERV366` = col_double(), 
										  `Taxa de juros - Selic - fixada pelo Copom - (% a.a.) - Banco Central do Brasil, Boletim, Seção mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM366_TJOVER366` = col_double(), 
										  `Taxa de juros - TR - (% a.m.) - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_TJTR366` = col_double(), 
										  `Zona do Euro - taxa de câmbio - euro / US$ - média - Euro - Banco Central do Brasil, Sistema Gerenciador de Séries Temporais (Bacen Outras/SGS) - GM366_EREURO366` = col_double(), 
										  `Índice de ações - Ibovespa - fechamento - - - Bolsa de Valores, Mercadorias e Futuros (BM&FBovespa) - GM366_IBVSP366` = col_double()), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)
ipeadata_m <- read_delim(ipeadata_m_path, 
                         ";", escape_double = FALSE,
						 col_types = cols(Data = col_skip(), 
										  `Exportações - preços - índice (média 2006 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_XPT12` = col_double(), 
										  `IGP-DI - geral - índice (ago. 1994 = 100) - - - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDI12` = col_double(), 
										  `IGP-M - geral - índice (ago. 1994 = 100) - - - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPM12` = col_double(), 
										  `INPC - geral - índice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_INPC12` = col_double(), 
										  `IPCA - geral - índice (dez. 1993 = 100) - - - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA12` = col_double(), 
										  `Importações - preços - índice (média 2006 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_MDPT12` = col_double(), 
										  `Inflação - IGP-DI - (% a.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPDIG12` = col_double(), 
										  `Inflação - IGP-M - (% a.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPMG12` = col_double(), 
										  `Inflação - IGP-OG - (% a.m.) - Fundação Getulio Vargas, Conjuntura Econômica - IGP (FGV/Conj. Econ. - IGP) - IGP12_IGPOGG12` = col_double(), 
										  `Inflação - INPC - (% a.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_INPCBR12` = col_double(), 
										  `Inflação - IPCA - (% a.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCAG12` = col_double(), 
										  `Inflação - IPCA-15 - (% a.m.) - Instituto Brasileiro de Geografia e Estatística, Sistema Nacional de Índices de Preços ao Consumidor (IBGE/SNIPC) - PRECOS12_IPCA15G12` = col_double(), 
										  `PIB - R$ - Banco Central do Brasil, Boletim, Seção Atividade Econômica (Bacen / Boletim / Ativ. Ec.) - BM12_PIB12` = col_double(), 
										  `Salário mínimo - R$ - Ministério do Trabalho e Emprego (MTE) - MTE12_SALMIN12` = col_double(), 
										  `Salário mínimo real - R$ - Instituto de Pesquisa Econômica Aplicada (IPEA) - GAC12_SALMINRE12` = col_double(), 
										  `Taxa de câmbio - R$ / US$ - comercial - compra - média - R$ - Banco Central do Brasil, Boletim, Seção Balanço de Pagamentos (Bacen / Boletim / BP) - BM12_ERC12` = col_double(), 
										  `Taxa de câmbio - R$ / US$ - comercial - venda - média - R$ - Banco Central do Brasil, Boletim, Seção Balanço de Pagamentos (Bacen / Boletim / BP) - BM12_ERV12` = col_double(), 
										  `Taxa de desemprego - RMSP - (%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDTGSP12` = col_double(), 
										  `Taxa de desemprego - aberto - RMSP - (%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDAGSP12` = col_double(), 
										  `Taxa de desemprego - oculto - RMSP - (%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOTSP12` = col_double(), 
										  `Taxa de desemprego - oculto - precário - RMSP - (%) - Fundação Sistema Estadual de Análise de Dados, Pesquisa de Emprego e Desemprego (Seade/PED) - SEADE12_TDOPSP12` = col_double(), 
										  `Taxa de desemprego - referência: 30 dias - RMs - (%) - Instituto Brasileiro de Geografia e Estatística, Pesquisa Mensal de Emprego (IBGE/PME) - obs:  PME foi encerrada em março de 2016, com a divulgação dos resultados referentes ao mês de fevereiro de 2016. - PMEN12_TD12` = col_double(), 
										  `Taxa de juros - Over / Selic - (% a.m.) - Banco Central do Brasil, Boletim, Seção mercado financeiro e de capitais (Bacen/Boletim/M. Finan.) - BM12_TJOVER12` = col_double()), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE)

n.ahead_d = 30
ipeadata_d.cont <- do.call(data.frame.na, lapply(ipeadata_d, function(x) tail(na.omit(x), n.ahead_d)) )
ipeadata_d <- do.call(data.frame.na, lapply(ipeadata_d, function(x) head(na.omit(x), length(na.omit(x))-n.ahead_d)) )

n.ahead_m = 12
ipeadata_m.cont <- do.call(data.frame.na, lapply(ipeadata_m, function(x) tail(na.omit(x), n.ahead_m)) )
ipeadata_m <- do.call(data.frame.na, lapply(ipeadata_m, function(x) head(na.omit(x), length(na.omit(x))-n.ahead_m)) )