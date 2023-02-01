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


#Divide por el sigma para cada periodo
resi=lapply(fits, residuals, standardize =TRUE) 




fits[[1]]@fit$coef[5]+fits[[1]]@fit$coef[6]
fits[[2]]@fit$coef[5]+fits[[2]]@fit$coef[6]



resi[[1]]=resi[[1]]*sqrt(fits[[1]]@fit$coef[4]/(1-fits[[1]]@fit$coef[5]-fits[[1]]@fit$coef[6]))
resi[[2]]=resi[[2]]*sqrt(fits[[2]]@fit$coef[4]/(1-fits[[2]]@fit$coef[5]-fits[[2]]@fit$coef[6]))




plot(resi[[1]])
plot(resi[[2]])



#Calcular la sd de la primer stock

#sd(resi[[1]])
#sd(resi[[2]])


var(resi[[1]])
var(resi[[2]])
cov(resi[[1]],resi[[2]])


v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)


################################################################################
#Modelar residuos
###############################################################################

#Hacer una lista con los modelos mixed


qmix_ =list(inverse.gamma = "inverse.gamma",
            inverse.burr = function(u, nu) (u^(-1/nu[2]) - 1)^(-1/nu[1])
            )
            

#Limites de parametros para estimar

m.p.b_ = list( 
              inverse.gamma = c(0.01, 10),
              inverse.burr = matrix(c(0.1, 0.1, 10, 10), ncol = 2)
              )


#Estimacion de parametros

#Ejecutar este comando se demora

fit.results = lapply(1:2, function(i) fitnvmix(v_t, qmix = qmix_[[i]], mix.param.bounds = m.p.b_[[i]]))

#Da los parametros estimados para cada accion

fit.results



#-------------------------------------------------------------------------------
#Gotness of fit

qq.results=lapply(1:2, function(i) qqplot_maha(fitnvmix_object = fit.results[[i]]))




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


Ratio_inv_Gamma=NULL


i=1

while (i<=length(all_weights[,1])) {
  


A=matrix(all_weights[i,], ncol = 1)
A


#Calcular el retorno

ret=0

for (e in 1:n_stocks){
  
  ret=ret+A[e,1]*as.numeric(fits[[e]]@fit$coef[1])
  
  }



#A[1,1]
#A[2,1]

#fits[[1]]@fit$coef[1]
#fits[[2]]@fit$coef[1]


#0.0004071588*0.504  + 0.0007463936  *0.496


  


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
         loc = as.numeric(t(A)%*%fit.results[[1]]$loc),
         scale = as.numeric(t(A)%*%fit.results[[1]]$scale%*%A),
         nu=fit.results[[1]]$nu)      



Ratio_inv_Gamma[i] = ES[1]

i=i+1

}


#Crear data frame

dat_inverse_Gamma=data.frame(Ratio_inv_Gamma=Ratio_inv_Gamma,all_weights=all_weights)



dat_inverse_Gamma=dat_inverse_Gamma[order(-dat_inverse_Gamma$Ratio_inv_Gamma),]

#Portfolio con mayor STARR
port_ratio_inv_Gamma=dat_inverse_Gamma[1,]
port_ratio_inv_Gamma





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
           loc = as.numeric(t(A)%*%fit.results[[1]]$loc),
           scale = as.numeric(t(A)%*%fit.results[[1]]$scale%*%A),
           nu=fit.results[[1]]$nu)    
  
  
  #Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 
  
  fitness = ES[1]
  
  fitness = fitness + 1e9 * (round(sum(A),10)-1)^2
  
  
  
  
  return(fitness)
  
}



pso_finance = psoptim(par = rep(NA,n_stocks), fn = function(x){fitness(x)}, 
                      lower = rep(0.05,n_stocks), upper =rep(1,n_stocks), 
                      control = list(maxit = 10000, s = 100))


pso_finance





################################################################################
#Optimizacion: Inverse Burr
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


Ratio_inv_Burr=NULL


i=1

while (i<=length(all_weights[,1])) {
  
  
  
  A=matrix(all_weights[i,], ncol = 1)
  A
  
  
  #Calcular el retorno
  
  ret=0
  
  for (e in 1:n_stocks){
    
    ret=ret+A[e,1]*as.numeric(fits[[e]]@fit$coef[1])
    
  }
  
  
  ES=ES_nvmix(0.05, qmix =  function(u, nu) (u^(-1/nu[2]) - 1)^(-1/nu[1]),
              loc = as.numeric(t(A)%*%fit.results[[2]]$loc),
              scale = as.numeric(t(A)%*%fit.results[[2]]$scale%*%A),
              nu = fit.results[[2]]$nu)         
  
  
  
  
  Ratio_inv_Burr[i] = ret/ES[1]
  
  i=i+1
  
}


#Crear data frame

dat_inverse_Burr=data.frame(Ratio_inv_Burr=Ratio_inv_Burr,all_weights=all_weights)



dat_inverse_Burr=dat_inverse_Burr[order(-dat_inverse_Burr$Ratio_inv_Burr),]

#Portfolio con mayor STARR
port_ratio_inv_Burr=dat_inverse_Burr[1,]
port_ratio_inv_Burr





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
  
  
  ES=ES_nvmix(0.05, qmix =  function(u, nu) (u^(-1/nu[2]) - 1)^(-1/nu[1]),
           loc = as.numeric(t(A)%*%fit.results[[2]]$loc),
           scale = as.numeric(t(A)%*%fit.results[[2]]$scale%*%A),
           nu = fit.results[[2]]$nu)     
  
  
  #Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 
  
  fitness = ret/ES[1]
  
  fitness = fitness - 1e9 * (round(sum(A),10)-1)^2
  
  
  
  
  return(fitness)
  
}



#pso_finance = psoptim(par = rep(NA,2), fn = function(x){-fitness(x)}, 
 #                     lower = c(0.2,0.2), upper = c(0.7,0.7), 
  #                    control = list(maxit = 10000, s = 100))


#pso_finance










################################################################################
#Test
################################################################################



#-------------------------------------------------------------------------------
#Cargar Fechas

#Primero año--->mes--->dia

f_init='2019-01-01'
f_final='2021-01-01'


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


#Dudo si lo divide por el sigma para cada periodo
resi=lapply(fits, residuals, standardize = TRUE) 

#Calcular la sd de la primer stock
resi[[1]]
sd(resi[[1]])


v_t=as.matrix(do.call(merge, resi))

#Cambia el nombre

colnames(v_t)=colnames(dx_t)
n = nrow(dx_t)


################################################################################
#Modelar residuos
###############################################################################

#Hacer una lista con los modelos mixed


qmix_ =list(inverse.gamma = "inverse.gamma",
            inverse.burr = function(u, nu) (u^(-1/nu[2]) - 1)^(-1/nu[1])
)


#Limites de parametros para estimar

m.p.b_ = list( 
  inverse.gamma = c(1, 8),
  inverse.burr = matrix(c(0.1, 0.1, 8, 8), ncol = 2)
)


#Estimacion de parametros

#Ejecutar este comando se demora

fit.results = lapply(1:2, function(i) fitnvmix(v_t, qmix = qmix_[[i]], mix.param.bounds = m.p.b_[[i]]))

#Da los parametros estimados para cada accion

fit.results



#-------------------------------------------------------------------------------
#Gotness of fit

qq.results=lapply(1:2, function(i) qqplot_maha(fitnvmix_object = fit.results[[i]]))



#-------------------------------------------------------------------------------
#Inverse gamma



  
  
A=matrix(pso_finance$par, ncol = 1)
  
  

  
#Calcular el retorno
  
  ret=0
  
  for (i in 1:n_stocks){
    
    ret=ret+A[i,1]*as.numeric(fits[[i]]@fit$coef[1])
    
  }
  
  #Estimar ES
  
  
  ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
              loc = as.numeric(t(A)%*%fit.results[[1]]$loc),
              scale = as.numeric(t(A)%*%fit.results[[1]]$scale%*%A),
              nu=fit.results[[1]]$nu)    
  
  
  #Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 
  
ret/ES[1]
  







#-------------------------------------------------------------------------------
#Igual peso





A=matrix(rep(1/n_stocks,n_stocks), ncol = 1)




#Calcular el retorno

ret=0

for (i in 1:n_stocks){
  
  ret=ret+A[i,1]*as.numeric(fits[[i]]@fit$coef[1])
  
}

#Estimar ES


ES=ES_nvmix(0.05, qmix =  "inverse.gamma",
            loc = as.numeric(t(A)%*%fit.results[[1]]$loc),
            scale = as.numeric(t(A)%*%fit.results[[1]]$scale%*%A),
            nu=fit.results[[1]]$nu)    


#Penalize Constraint Violation: Es para que las sumas de los pesos sea 1 

ret/ES[1]
















