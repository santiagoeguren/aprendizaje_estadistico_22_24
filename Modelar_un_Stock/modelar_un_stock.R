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
#mixed_tempered_stable
library(MixedTS)

#################################################################################
#Cargar Datos
################################################################################


#-------------------------------------------------------------------------------
#Cargar Fechas
#-------------------------------------------------------------------------------
#Primero año--->mes--->dia

f_init='2011-01-01'
f_final='2021-01-01'


#--------------------------------------------------------------------------------
#Cargar simbolo 1

eq="KO"


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
summary(model)


v_t= model@residuals


plot(v_t,t = "l")

#################################################################################
#Estimar MTSP
################################################################################



#-------------------------------------------------------------------------------
# Density of MixedTS with Gamma
#-------------------------------------------------------------------------------

ParamEx1=setMixedTS.param(mu0=0, mu=0, sigma=1, a=2,
                          alpha=1.5, lambda_p=1, lambda_m=1, 
                          Mixing="Gamma")






Rand1=rMixedTS(x=length(v_t),object=ParamEx1, setSup=10,setInf=-10,N=2^9)


Rand1@Data=v_t




est1=mle.MixedTS(object=Rand1 , setSup=10,setInf=-10,N=2^9)


summary(est1)





#-------------------------------------------------------------------------------
# Density of MixedTS with Gamma
#-------------------------------------------------------------------------------

ParamEx1=setMixedTS.param(mu0=-0.0681    , mu=0.601  , sigma= 1.053  , a= 1.16  ,
                          alpha= 1.47  , lambda_p=1.02  , lambda_m=1.03  , 
                          Mixing="Gamma")






#-------------------------------------------------------------------------------
#Determinar funcion densidad

x=seq(-0.05,0.05,length=100)

dens=dMixedTS(x=x,object=ParamEx1,setSup=0.01,setInf=-0.1,N=2^7)

#help(dMixedTS)

#-------------------------------------------------------------------------------
#Graficar

plot(dens)
hist(v_t,n=60, probability = TRUE,border = "white",
     col = "lightgray",add=T)



