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
#Financiera
import yfinance as yf
#pyfolio
#import pyfolio as pf


################################################################################
#Transformar objetos de R a Python
################################################################################

v_t=r["v_t"]
x_barra=r["x_barra"]
ticker=r["ticker"]


################################################################################
#Optimizar mediante monte carlo
################################################################################




#Definir número de interacciones

num_ports=50000


# Definir limites de los pesos
weight_min = 0.05
weight_max = 1
weight_size = len(ticker)




#Ejecutar interacciones

all_weights=np.zeros((num_ports,weight_size))


#Generar pesos



for ind in range (num_ports):
   
     weights=np.array(np.random.random(weight_size))
     weights= weights / np.sum(weights)
     
     all_weights[ind:]=weights



# Redondear valores
all_weights=all_weights.round(2)


#Crear data frame
df=pd.DataFrame(all_weights)

#Dar nombre al indice
df.index=np.arange(0, len(df.index), 1)

df


#-------------------------------------------------------------------------------
df = df[df[df.columns] <= weight_max]
df = df[df[df.columns] >= weight_min]
df =df.dropna()

df


################################################################################
#Calcular TARI
################################################################################



ER=np.zeros(len(df.index))
ETL=np.zeros(len(df.index))
STARR=np.zeros(len(df.index))



for ind in range(len(df.index)):

     ER[ind]=np.sum(x_barra*df.iloc[ind])
     
     retorno_cartera = np.sort(np.dot(v_t, df.iloc[ind]))
     localizador=mt.ceil((len(retorno_cartera)+1)*0.05)
     ETL[ind]=-1*(retorno_cartera[0:(localizador-1)].mean())
   
     STARR[ind]=ER[ind]/ ETL[ind]



#Imprimir
print("Mayor  STARR: ",  STARR.max())
print("Lugar del mayor  STARR: ",  STARR.argmax()) 
print("Pesos del mayor  STARR: ",df.iloc[ STARR.argmax()])





################################################################################
#Graficar frontera 
################################################################################


max_sr_ret=ER[ STARR.argmax()]
max_sr_vol=ETL[ STARR.argmax()]
plt.figure(figsize=(20,10))
plt.scatter( ETL,ER,c= STARR,cmap="plasma")
plt.colorbar(label= "STARR")
plt.xlabel("ETL")
plt.ylabel("ER")

plt.scatter(max_sr_vol,max_sr_ret,c="red",s=50,edgecolors="black")

# show plot
plt.show()












