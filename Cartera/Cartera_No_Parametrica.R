################################################################################
#Cargar librerias
################################################################################

#Descargar datos
library(quantmod)  
#Graficos
library(ggplot2) 
#Complemeto graficos
require(tidyr)
#Test de Dickey-Fuller
library(tseries)
#Zivot & Andrews Unit Root Test
library(urca) 
# Estimar correlograma  
library(astsa)
#Extraer stock sp500
library(rvest)
#Garch
require(fGarch)
#forecast
library(forecast)
#Complemento grafici
library(plotly)




################################################################################
#Cargar Tickers
################################################################################

#-------------------------------------------------------------------------------
#Stocks S&P500
#-------------------------------------------------------------------------------



url ="https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 = url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
SP500 = SP500[[1]]
stocks=SP500$Symbol







################################################################################
#Construir data frame
################################################################################



#-------------------------------------------------------------------------------
#Definir variables
#-------------------------------------------------------------------------------

ticker=sample(stocks,10,replace = FALSE)
       


x_barra=NULL
v_t=NULL



#-------------------------------------------------------------------------------
#Cargar Fechas
#-------------------------------------------------------------------------------
#Primero año--->mes--->dia

f_init='2011-01-01'
f_final='2021-01-01'




#-------------------------------------------------------------------------------
#inicio ciclo while
#-------------------------------------------------------------------------------


e=1


while (e<=length(ticker)) {
  



#--------------------------------------------------------------------------------
#Cargar simbolo 1

eq=ticker[e]


#-------------------------------------------------------------------------------
#Descargar datos de yahoo

#Primero año--->mes--->dia
data_eq=new.env()
getSymbols(eq, src = 'yahoo', from = f_init, to=f_final,env = data_eq, 
           auto.assign = T, periodicity = "d")
suppressWarnings(try(for(i in ls(data_eq)) data_eq[[i]] = adjustOHLC(data_eq[[i]],
                 use.Adjusted=T),silent = TRUE)) 





#-------------------------------------------------------------------------------
#Extraer Precio ajustado

x_t=log(as.numeric(data_eq[[eq]][,6]))
dx_t=diff(x_t,lag = 1)


#-------------------------------------------------------------------------------

model= suppressWarnings(garchFit( ~ arma(1,1) + garch(1, 1),data=dx_t, trace = F))
#summary(model)


x_barra[e]=model@fit$coef[1]


v_t=cbind(v_t, model@residuals)


#-------------------------------------------------------------------------------
#Fin ciclo while

e=e+1

}


#-------------------------------------------------------------------------------
#Crear data frame
#-------------------------------------------------------------------------------

v_t=data.frame(v_t)
colnames(v_t)=ticker

















