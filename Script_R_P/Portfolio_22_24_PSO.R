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


################################################################################
#Inicio Ciclo While
################################################################################


ES_1_n=NULL
ES_Estimado=NULL
ES_pso=NULL



ii=101


while (ii<=120) {
  


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



#-------------------------------------------------------------------------------
#Modelar ARIMA-GARCH

fit.ARMA.GARCH=fit_ARMA_GARCH(dx_t, ugarchspec.list = uspec, verbose = FALSE)

#Resultado
fits=fit.ARMA.GARCH$fit 


#-------------------------------------------------------------------------------
#Extraer los residuos

#"""
#Divide por el sigma para cada periodo
#"""

resi=lapply(fits, residuals, standardize =TRUE) 





for (i in 1:n_stocks){
  
  resi[[i]]=resi[[i]]*sqrt(fits[[i]]@fit$coef[4]/(1-fits[[i]]@fit$coef[5]-fits[[i]]@fit$coef[6]))
  
  
}


v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)



################################################################################
#Modelar residuos
###############################################################################


#Estimacion de parametros

fit.results = fitnvmix(v_t, qmix =  "inverse.gamma", mix.param.bounds =  c(0.01, 10))



################################################################################
#Optimizacion: Inverse Gamma
################################################################################



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



#-------------------------------------------------------------------------------
#Modelar ARIMA-GARCH

fit.ARMA.GARCH=fit_ARMA_GARCH(dx_t, ugarchspec.list = uspec, verbose = FALSE)

#Resultado
fits=fit.ARMA.GARCH$fit 


#-------------------------------------------------------------------------------
#Extraer los residuos

#"""
#Divide por el sigma para cada periodo
#"""

resi=lapply(fits, residuals, standardize =TRUE) 





for (i in 1:n_stocks){
  
  resi[[i]]=resi[[i]]*sqrt(fits[[i]]@fit$coef[4]/(1-fits[[i]]@fit$coef[5]-fits[[i]]@fit$coef[6]))
  
  
}


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





################################################################################
#ES igual peso
################################################################################




A=matrix(rep(1/n_stocks,n_stocks), ncol = 1)




ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results$loc),
            scale = as.numeric(t(A)%*%fit.results$scale%*%A),
            nu=fit.results$nu)      



ES_1_n[ii]=ES





################################################################################
#Optimizado
################################################################################




A=matrix(pso_finance$par, ncol = 1)


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results$loc),
            scale = as.numeric(t(A)%*%fit.results$scale%*%A),
            nu=fit.results$nu)      



ES_pso[ii]=ES

ES_Estimado[ii]=pso_finance$value


ii=ii+1

}

mean(ES_1_n)
mean(ES_Estimado)
mean(ES_pso)


sd(ES_1_n)
sd(ES_pso)

t.test(ES_1_n,ES_pso)
var.test(ES_1_n,ES_pso)



#save(ES_1_n, file = "ES_1_n.Rdata")
#save(ES_Estimado, file = "ES_Estimado.Rdata")
#save(ES_pso, file = "ES_pso.Rdata")
#save(Pesos_2, file = "Pesos_2.Rdata")

#load("rate_1n.Rdata")
#load("rate_inG.Rdata")
