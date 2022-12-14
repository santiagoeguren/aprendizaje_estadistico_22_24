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
#Complmento ggplot2
library(viridis)
#Complmento ggplot2
library(extrafont)
#Sumar columnas
library(dplyr)



################################################################################
#Cargar  Funciones
################################################################################


#-------------------------------------------------------------------------------
#Estimar ETL No Parametrico

f_ETL_no_parametric=function(e,r){
  
  n=length(r) 
  r=sort(r)
  
  
  
  etl_resultado=1/e*((1/n)*sum(r[c(1:(round(n*e,0)))])+
                       (e-((round(n*e,0))/(n)))*r[round(n*e+1,0)])
  
  
  return(etl_resultado)
  
}



################################################################################
#Cargar Tickers
################################################################################

#-------------------------------------------------------------------------------
#Stocks S&P500




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
#inicio ciclo ii

ER_model=NULL
ETL_model=NULL
STARI_model=NULL

ER_n=NULL
ETL_n=NULL
STARI_n=NULL




ii=1





while (ii<=50) {
  



#-------------------------------------------------------------------------------
#Definir variables

n_stocks=6


ticker=sample(stocks,n_stocks,replace = FALSE)
       


x_barra=NULL
v_t=NULL


################################################################################
#Descargar Datos
################################################################################



#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2009-01-01'
f_final='2019-01-01'




#-------------------------------------------------------------------------------
#inicio ciclo while



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


################################################################################
#Crear data frame
################################################################################


v_t=data.frame(v_t)
colnames(v_t)=ticker




################################################################################
#Train
################################################################################


#-------------------------------------------------------------------------------
#Crear pesos

all_weights=NULL


i=1

while (i<=50000) {
  


weights=sample(1:100, length(ticker), replace=T)
weights=round(weights/sum(weights),3)

all_weights=rbind(all_weights,weights)

i=i+1

}

#-------------------------------------------------------------------------------
#Colocar restriccion

all_weights[all_weights<=0.01]=NA
all_weights=na.omit(all_weights)




#-------------------------------------------------------------------------------
#Definir variables


ER=NULL
ETL=NULL
STARI=NULL


#-------------------------------------------------------------------------------
#Estimar variables

i=1

while (i<=length(all_weights[,1])) {
  


ER=c(ER,sum(all_weights[i,]*x_barra))

ETL=c(ETL,-1*f_ETL_no_parametric(0.05,rowSums(all_weights[i,]*v_t)))

STARI=c(STARI,sum(all_weights[i,]*x_barra)/(-1*f_ETL_no_parametric(0.05,rowSums(all_weights[i,]*v_t))))


i=i+1
}

#Controlar la multi all_weights[i,]*v_t

#rowSums(all_weights[1,]*v_t)


#-------------------------------------------------------------------------------
#Construir Data frame

dat=data.frame(ER,ETL,STARI,all_weights=all_weights)


dat=dat[order(-dat$STARI),]

#Portfolio con mayor STARR
port_STARI=dat[1,]
port_STARI

#-------------------------------------------------------------------------------
#Grear grafico




g=ggplot(dat, aes(x = ETL, y = ER))
g=g+geom_point(mapping = aes(color = STARI),alpha = 0.5)
g=g+geom_point(data=port_STARI,aes(x =ETL , y = ER),size=3,col="red")
g=g+ scale_color_viridis(direction = -1, option = "D", "STARI")
g=g+labs(x = "ETL", y = "Retorno",
         title = "Frontera de inversión",
         subtitle = "ETL vs Retorno",
         caption = "Fuente: Propia")
g=g+theme_minimal()
g=g+theme(plot.background = element_rect(fill = "white", color = NA),
          text = element_text(family = "OfficinaSansITC"))
g=g+theme(text = element_text(family = "OfficinaSansITC")) + 
  theme(axis.text.x = element_text(size = 12, color = "gray30")) + 
  theme(axis.text.y = element_text(size = 12, color = "gray30")) + 
  theme(plot.title = element_text(color = "gray10", size = 18)) +   
  theme(plot.subtitle = element_text(color = "gray40", size = 16)) + 
  theme(plot.caption = element_text(color = "gray40", size = 12)) + 
  theme(axis.title.x = element_text(hjust = 0, size = 14, color = "grey20")) + 
  theme(axis.title.y = element_text(vjust = 1, size = 14, color = "grey20")) + 
  theme(legend.text = element_text(size = 12, color = "grey40")) + 
  theme(legend.title = element_text(size = 15, color = "grey30"))
#g







################################################################################
#Test
################################################################################


#-------------------------------------------------------------------------------
#Definir variables


x_barra=NULL
v_t=NULL




#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2019-01-02'
f_final='2020-01-01'




#-------------------------------------------------------------------------------
#inicio ciclo while



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


################################################################################
#Crear data frame
################################################################################


v_t=data.frame(v_t)
colnames(v_t)=ticker




################################################################################
#Train
################################################################################


#-------------------------------------------------------------------------------
#Crear pesos

all_weights=NULL




all_weights=as.numeric(port_STARI[1,seq(4,3+n_stocks,1)])
all_weights=rbind(all_weights,rep(1,n_stocks)/n_stocks)
all_weights






#-------------------------------------------------------------------------------
#Definir variables


ER=NULL
ETL=NULL
STARI=NULL


#-------------------------------------------------------------------------------
#Estimar variables

i=1

while (i<=length(all_weights[,1])) {
  
  
  
  ER=c(ER,sum(all_weights[i,]*x_barra))
  
  ETL=c(ETL,-1*f_ETL_no_parametric(0.05,rowSums(all_weights[i,]*v_t)))
  
  STARI=c(STARI,sum(all_weights[i,]*x_barra)/(-1*f_ETL_no_parametric(0.05,rowSums(all_weights[i,]*v_t))))
  
  
  i=i+1
}

#Controlar la multi all_weights[i,]*v_t

#rowSums(all_weights[1,]*v_t)


#-------------------------------------------------------------------------------
#Construir Data frame

dat=data.frame(ER,ETL,STARI,all_weights=all_weights)


ER_model[ii]=dat$ER[1]
ETL_model[ii]=dat$ETL[1]
STARI_model[ii]=dat$STARI[1]
  

ER_n[ii]=dat$ER[2]
ETL_n[ii]=dat$ETL[2]
STARI_n[ii]=dat$STARI[2]


ii=ii+1

}



summary(ER_model)
summary(ER_n)

summary(ETL_model)
summary(ETL_n)



summary(STARI_model)
summary(STARI_n)




