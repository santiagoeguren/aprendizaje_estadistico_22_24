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

#_------------------------------------------------------------------------------
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



ii=1


rate_inG=NULL
rate_1n=NULL


Pesos_1=NULL
Pesos_2=NULL



while (ii<=100){
  


#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2009-01-01'
f_final='2019-01-01'



#-------------------------------------------------------------------------------
#Definir cantidad de stock

n_stocks=2

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

#Dudo si lo divide por el sigma para cada periodo
resi=lapply(fits, residuals, standardize = TRUE) 

resi[[1]]=resi[[1]]*fits[[1]]@fit$coef[4]
resi[[2]]=resi[[2]]*fits[[2]]@fit$coef[4]


v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)


################################################################################
#Modelar residuos
###############################################################################



#Estimacion de parametros


fit.results=fitnvmix(v_t, qmix = "inverse.gamma", mix.param.bounds = c(0.1, 10))


################################################################################
#Optimizacion: Inverse Gamma
################################################################################



#-------------------------------------------------------------------------------
#Particle Swarm Op





fitness = function(x){
  
  
  
  A=matrix(rep(NA,n_stocks), ncol = 1)
  
  
  
  #Hay un error en la matreix A
  
  
  
  for (i in 1:n_stocks){
    
    A[i,1]=x[i]
    
  }
  
  
  #Calcular el retorno
  
  ret=0
  
  for (i in 1:n_stocks){
    
    ret=ret+A[i,1]*as.numeric(fits[[i]]@fit$coef[1])
    
  }
  
  #Estimar ES
  
  
  ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
              loc = as.numeric(t(A)%*%fit.results$loc),
              scale = as.numeric(t(A)%*%fit.results$scale%*%A),
              nu=fit.results$nu)    
  
  
  #Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 
  
  #fitness = ret/ES[1]
   fitness = ES[1]
  
  fitness = fitness + 1e9 * (round(sum(A),10)-1)^2
  
  
  
  
  return(fitness)
  
}



pso_finance = psoptim(par = rep(NA,n_stocks), fn = function(x){fitness(x)}, 
                      lower = rep(0.05,n_stocks), upper =rep(1,n_stocks), 
                      control = list(maxit = 10000, s = 100))



pso_finance



################################################################################
#Test
################################################################################



#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2019-01-01'
f_final='2020-01-01'


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


#Dudo si lo divide por el sigma para cada periodo
resi=lapply(fits, residuals, standardize = TRUE) 

resi[[1]]=resi[[1]]*fits[[1]]@fit$coef[4]
resi[[2]]=resi[[2]]*fits[[2]]@fit$coef[4]

v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)


################################################################################
#Modelar residuos
###############################################################################




#Estimacion de parametros

#Ejecutar este comando se demora

fit.results=fitnvmix(v_t, qmix = "inverse.gamma", mix.param.bounds = c(0.01, 10))

#fit.results

#-------------------------------------------------------------------------------
#Inverse gamma


A=matrix(pso_finance$par, ncol = 1)


Pesos_1[ii]=A[1,1]
Pesos_2[ii]=A[2,1]


#Calcular el retorno

ret=0

for (i in 1:n_stocks){
  
  ret=ret+A[i,1]*as.numeric(fits[[i]]@fit$coef[1])
  
}

#Estimar ES


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results$loc),
            scale = as.numeric(t(A)%*%fit.results$scale%*%A),
            nu=fit.results$nu)    


#Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 

#ret/ES[1]


 #rate_inG[ii]=ret/ES[1]
 rate_inG[ii]=ES[1]




#-------------------------------------------------------------------------------
#Igual peso





A=matrix(rep(1/n_stocks,n_stocks), ncol = 1)
# A=matrix(c(0.80,0.20), ncol = 1)




#Calcular el retorno

ret=0

for (i in 1:n_stocks){
  
  ret=ret+A[i,1]*as.numeric(fits[[i]]@fit$coef[1])
  
}

#Estimar ES


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results$loc),
            scale = as.numeric(t(A)%*%fit.results$scale%*%A),
            nu=fit.results$nu)    


#Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 




 #rate_1n[ii]=ret/ES[1]
 rate_1n[ii]=ES[1]


ii=ii+1

}














Pesos_1
Pesos_2

t.test(Pesos_1-Pesos_2)


summary(rate_inG)
summary(rate_1n)

#rate_1n=rate_1n[-51]
#rate_inG=rate_inG[-51]

var.test(rate_inG,rate_1n)


t.test(rate_inG,rate_1n)

plot(rate_1n,rate_inG)


summary(lm(rate_inG~0+rate_1n))







#save(rate_inG, file = "rate_inG.Rdata")
#save(rate_1n, file = "rate_1n.Rdata")
#save(Pesos_1, file = "Pesos_1.Rdata")
#save(Pesos_2, file = "Pesos_2.Rdata")

#load("rate_1n.Rdata")
#load("rate_inG.Rdata")
