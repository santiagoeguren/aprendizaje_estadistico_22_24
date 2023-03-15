################################################################################
#Cargar librerias
################################################################################

#Graficos
library(ggplot2)
#Mean-Variance Mixture
library(nvmix)
#GARCH
library(rugarch)
#GARCH
library(qrmtools)
#Descargar datos
library(quantmod) 
#Extraer stock sp500
library(rvest)
#Pso
library(pso)





################################################################################
#Descargar datos
################################################################################

#------------------------------------------------------------------------------
#Cargar Tickers

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
#Training
################################################################################



#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2008-01-01'
f_final='2018-01-01'



#-------------------------------------------------------------------------------
#Definir cantidad de stock

n_stocks=3

ticker=sample(stocks,n_stocks,replace = FALSE)

dx_t=NULL

#-------------------------------------------------------------------------------
#Descargar precios adj.

for (e in 1:n_stocks){

 

#--------------------------------------------------------------------------------
#Cargar simbolo 1

eq=ticker[e]


#-------------------------------------------------------------------------------
#Descargar datos de yahoo


#"""
#Todos los precios estas ajustados
#"""

data_eq=new.env()
getSymbols(eq, src = 'yahoo', from = f_init, to=f_final,env = data_eq, 
           auto.assign = T, periodicity = "d")
suppressWarnings(try(for(i in ls(data_eq)) data_eq[[i]] = adjustOHLC(data_eq[[i]],
                 use.Adjusted=T),silent = TRUE)) 




#-------------------------------------------------------------------------------
#Extraer Precio ajustado

x_t=log(as.numeric(data_eq[[eq]][,6]))
dx_t_partial=diff(x_t,lag = 1)


#-------------------------------------------------------------------------------
#Crear data frame

dx_t=cbind(dx_t,dx_t_partial)


}

colnames(dx_t)=ticker


################################################################################
#Modelar ARIMA-GARCH
################################################################################


#-------------------------------------------------------------------------------
#Crear modelo para cada stock

uspec=rep(list(ugarchspec(distribution.model = "std")), ncol(dx_t))
uspec


#-------------------------------------------------------------------------------
#Modelar ARIMA-GARCH

fit.ARMA.GARCH=fit_ARMA_GARCH(dx_t, ugarchspec.list = uspec, verbose = FALSE)

#Resultado
fit.ARMA.GARCH

#Resultado
fits=fit.ARMA.GARCH$fit 
fits
summary(fits)

#-------------------------------------------------------------------------------
#Extraer los residuos

#"""
#Divide por el sigma para cada periodo
#"""

resi=lapply(fits, residuals, standardize =TRUE) 





for (i in 1:n_stocks){
  
     resi[[i]]=resi[[i]]*sqrt(fits[[i]]@fit$coef[4]/(1-fits[[i]]@fit$coef[5]-fits[[i]]@fit$coef[6]))
    
  
}

#Graficar, los dos primeros

plot(resi[[1]])
plot(resi[[2]])


#Gestimar varianzas las  dos primeras y cov

var(resi[[1]])
var(resi[[2]])
cov(resi[[1]],resi[[2]])



#Armar estructura de datos

v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)



################################################################################
#Modelar residuos
###############################################################################


#Estimacion de parametros

fit.results = fitnvmix(v_t, qmix =  "inverse.gamma", mix.param.bounds =  c(0.01, 10))

#Da los parametros estimados para cada accion

fit.results



#-------------------------------------------------------------------------------
#Gotness of fit


qqplot_maha(fitnvmix_object = fit.results)


################################################################################
#Optimizacion: Inverse Gamma
################################################################################


#-------------------------------------------------------------------------------
#Monte Carlo


#Generar pesos

all_weights=NULL


i=1

while (i<=50000) {
  
  
  
  weights=sample(1:100, length(ticker), replace=T)
  weights=round(weights/sum(weights),3)
  
  all_weights=rbind(all_weights,weights)
  
  i=i+1
  
}


#-------------------------------------------------------------------------------
#Inverse gamma


ES_Monte=NULL


i=1

while (i<=length(all_weights[,1])) {
  


A=matrix(all_weights[i,], ncol = 1)


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
         loc = as.numeric(t(A)%*%fit.results$loc),
         scale = as.numeric(t(A)%*%fit.results$scale%*%A),
         nu=fit.results$nu)      



ES_Monte[i] = ES[1]

i=i+1

}


#Crear data frame

dat_inverse_Gamma=data.frame(ES_Monte=ES_Monte,all_weights=all_weights)
dat_inverse_Gamma=dat_inverse_Gamma[order(-dat_inverse_Gamma$ES_Monte),]


################################################################################
#Particle Swarm Op
################################################################################

fitness = function(x){
  
  
  
  A=matrix(rep(NA,n_stocks), ncol = 1)
  
  
  
  #Hay un error en la matreix A
  
  
  
  for (i in 1:n_stocks){
    
   A[i,1]=x[i]
    
  }
  
 
  
  #Estimar ES
  
  
  ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
           loc = as.numeric(t(A)%*%fit.results$loc),
           scale = as.numeric(t(A)%*%fit.results$scale%*%A),
           nu=fit.results$nu)    
  
  
  #Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 
  
  fitness = ES[1]
  
  fitness = fitness + 1e9 * (round(sum(A),10)-1)^2
  
  
  
  
  return(fitness)
  
}


#"""
#Por defecto minimiza
#"""

pso_finance = psoptim(par = rep(NA,n_stocks), fn = function(x){fitness(x)}, 
                      lower = rep(0.05,n_stocks), upper =rep(1,n_stocks), 
                      control = list(maxit = 10000, s = 100))


pso_finance
















################################################################################
#Comparar out the sample
################################################################################



#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2018-01-01'
f_final='2023-01-01'



#-------------------------------------------------------------------------------
#Definir cantidad de stock

dx_t=NULL

#-------------------------------------------------------------------------------
#Descargar precios adj.

for (e in 1:n_stocks){
  
  
  
  #--------------------------------------------------------------------------------
  #Cargar simbolo 1
  
  eq=ticker[e]
  
  
  #-------------------------------------------------------------------------------
  #Descargar datos de yahoo
  
  
  #"""
  #Todos los precios estas ajustados
  #"""
  
  data_eq=new.env()
  getSymbols(eq, src = 'yahoo', from = f_init, to=f_final,env = data_eq, 
             auto.assign = T, periodicity = "d")
  suppressWarnings(try(for(i in ls(data_eq)) data_eq[[i]] = adjustOHLC(data_eq[[i]],
                                                                       use.Adjusted=T),silent = TRUE)) 
  
  
  
  
  #-------------------------------------------------------------------------------
  #Extraer Precio ajustado
  
  x_t=log(as.numeric(data_eq[[eq]][,6]))
  dx_t_partial=diff(x_t,lag = 1)
  
  
  #-------------------------------------------------------------------------------
  #Crear data frame
  
  dx_t=cbind(dx_t,dx_t_partial)
  
  
}

colnames(dx_t)=ticker


################################################################################
#Modelar ARIMA-GARCH
################################################################################


#-------------------------------------------------------------------------------
#Crear modelo para cada stock

uspec=rep(list(ugarchspec(distribution.model = "std")), ncol(dx_t))
uspec


#-------------------------------------------------------------------------------
#Modelar ARIMA-GARCH

fit.ARMA.GARCH=fit_ARMA_GARCH(dx_t, ugarchspec.list = uspec, verbose = FALSE)

#Resultado
fit.ARMA.GARCH

#Resultado
fits=fit.ARMA.GARCH$fit 
fits
summary(fits)

#-------------------------------------------------------------------------------
#Extraer los residuos

#"""
#Divide por el sigma para cada periodo
#"""

resi=lapply(fits, residuals, standardize =TRUE) 





for (i in 1:n_stocks){
  
  resi[[i]]=resi[[i]]*sqrt(fits[[i]]@fit$coef[4]/(1-fits[[i]]@fit$coef[5]-fits[[i]]@fit$coef[6]))
  
  
}

#Graficar, los dos primeros

plot(resi[[1]])
plot(resi[[2]])


#Gestimar varianzas las  dos primeras y cov

var(resi[[1]])
var(resi[[2]])
cov(resi[[1]],resi[[2]])



#Armar estructura de datos

v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)



################################################################################
#Modelar residuos
###############################################################################


#Estimacion de parametros

fit.results = fitnvmix(v_t, qmix =  "inverse.gamma", mix.param.bounds =  c(0.01, 10))

#Da los parametros estimados para cada accion

fit.results



`#-------------------------------------------------------------------------------
#Gotness of fit


qqplot_maha(fitnvmix_object = fit.results)


################################################################################
#ES igual peso
################################################################################



  
A=matrix(rep(1/n_stocks,n_stocks), ncol = 1)
  
rep()

  
ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
              loc = as.numeric(t(A)%*%fit.results$loc),
              scale = as.numeric(t(A)%*%fit.results$scale%*%A),
              nu=fit.results$nu)      
  
  
  
ES





################################################################################
#Optimizado
################################################################################




A=matrix(pso_finance$par, ncol = 1)


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results$loc),
            scale = as.numeric(t(A)%*%fit.results$scale%*%A),
            nu=fit.results$nu)      



ES

pso_finance$value
