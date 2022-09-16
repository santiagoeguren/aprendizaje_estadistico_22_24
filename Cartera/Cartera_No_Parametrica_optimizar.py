################################################################################
#Importar librerias
################################################################################

#Numpy
import numpy as np
#Pandas
import pandas as pd
#Graficos
import matplotlib.pyplot as plt
#matematica
import math as mt



################################################################################
#Transformar objetos de R a Python
################################################################################

v_t=r["v_t"]
x_barra=r["x_barra"]

print(v_t)



################################################################################
#Optimizar mediante monte carlo
################################################################################


#Definir número de interacciones

num_ports=50000


#Ejecutar interacciones

all_weights=np.zeros((num_ports,7))


#Generar pesos



for ind in range(num_ports):

     weights=np.array(np.random.random(7))
     weights=weights/np.sum(weights)         
   
     all_weights[ind:]=weights







#Crear data frame
df=pd.DataFrame(all_weights)

#Dar nombre al indice
df.index=np.arange(0, len(df.index), 1)

df



#Calcular el Sharpe ratio

retorno=np.zeros(len(df.index))
VaR=np.zeros(len(df.index))
CVaR=np.zeros(len(df.index))
sharpe_VaR=np.zeros(len(df.index))
sharpe_CVaR=np.zeros(len(df.index))






for ind in range(len(df.index)):


     retorno[ind]=np.sum(x_barra*df.iloc[ind])
     
     VaR[ind]=-1*(np.quantile(np.dot(v_t, df.iloc[ind]), 0.05) )
     
     retorno_cartera = np.sort(np.dot(v_t, df.iloc[ind]))
     localizador=mt.ceil((len(retorno_cartera)+1)*0.05)
     CVaR[ind]=-1*(retorno_cartera[0:(localizador-1)].mean())

     sharpe_VaR[ind]=retorno[ind]/ VaR[ind]
     sharpe_CVaR[ind]=retorno[ind]/ CVaR[ind]

   
#Imprimir
print("Mayor Sharpe: ", sharpe_VaR.max())
print("Lugar del mayor sharpe: ",sharpe_VaR.argmax()) 
print("Pesos del mayor sharpe: ",df.iloc[sharpe_VaR.argmax()])



#Imprimir
print("Mayor Sharpe: ", sharpe_CVaR.max())
print("Lugar del mayor sharpe: ",sharpe_CVaR.argmax()) 
print("Pesos del mayor sharpe: ",df.iloc[sharpe_CVaR.argmax()])






max_sr_ret=retorno[sharpe_VaR.argmax()]
max_sr_vol=VaR[sharpe_VaR.argmax()]
plt.figure(figsize=(12,8))
plt.scatter( VaR,retorno,c=sharpe_VaR,cmap="plasma")
plt.colorbar(label="Sharpe Ratio")
plt.xlabel("VaR")
plt.ylabel("Return")

plt.scatter(max_sr_vol,max_sr_ret,c="red",s=50,edgecolors="black")

# show plot
plt.show()









max_sr_ret=retorno[sharpe_CVaR.argmax()]
max_sr_vol=CVaR[sharpe_CVaR.argmax()]
plt.figure(figsize=(12,8))
plt.scatter( CVaR,retorno,c=sharpe_CVaR,cmap="plasma")
plt.colorbar(label="Sharpe Ratio")
plt.xlabel("CVaR")
plt.ylabel("Return")

plt.scatter(max_sr_vol,max_sr_ret,c="red",s=50,edgecolors="black")

# show plot
plt.show()


