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
#Transformar objetos de R a Python, leyendo desde un CSV
################################################################################

v_t = pd.read_csv(r'v_t.csv')
x_barra = pd.read_csv(r'x_barra.csv')
ticker = pd.read_csv(r'ticker.csv')


################################################################################
#Optimizar mediante monte carlo
################################################################################




#Definir número de interacciones

num_ports=100000


# Definir limites de los pesos
weight_min = 0.04
weight_max = 1
weight_size = len(ticker)


#Ejecutar interacciones

all_weights=np.zeros((num_ports,weight_size))


#Generar pesos

def between(value):
     if value.item() >= weight_min and value.item() <= weight_max:
          return value
     return 0

def generate_weights(size):
     """
     Generar array de 'size' numeros en un intervalo [low, high).
     """
     weights=np.array(np.random.random(size))
     weights= weights / np.sum(weights)
     return list(map(between, weights))

for ind in range(num_ports):
     all_weights[ind:]= generate_weights(weight_size)


# Redondear valores
all_weights=all_weights.round(2)


#Crear data frame
df=pd.DataFrame(all_weights)

#Dar nombre al indice
df.index=np.arange(0, len(df.index), 1)

df



################################################################################
#Calcular TARI
################################################################################

#"""
#ER: Expected Return
#ETL:Expected Tail loss
#TARI: Tail Adjusted Return Indicator
#""""

ER=np.zeros(len(df.index))
ETL=np.zeros(len(df.index))
TARI=np.zeros(len(df.index))



for ind in range(len(df.index)):

     ER[ind]=np.sum(x_barra*df.iloc[ind])
     
     retorno_cartera = np.sort(np.dot(v_t, df.iloc[ind]))
     localizador=mt.ceil((len(retorno_cartera)+1)*0.05)
     ETL[ind]=-1*(retorno_cartera[0:(localizador-1)].mean())
   
     TARI[ind]=ER[ind]/ ETL[ind]



#Imprimir
print("Mayor TARI: ", TARI.max())
print("Lugar del mayor TARI: ", TARI.argmax()) 
print("Pesos del mayor TARI: ",df.iloc[TARI.argmax()])



# 4: Stock: 0.051178229039275505
#5 : 0.0496541165033135
#6:  0.050267636912560795
#10: 0.04690070799141482
#20:  0.04172045097277336
#30: 0.054659



################################################################################
#Graficar frontera 
################################################################################


max_sr_ret=ER[TARI.argmax()]
max_sr_vol=ETL[TARI.argmax()]
plt.figure(figsize=(20,10))
plt.scatter( ETL,ER,c=TARI,cmap="plasma")
plt.colorbar(label="TARI")
plt.xlabel("ETL")
plt.ylabel("ER")

plt.scatter(max_sr_vol,max_sr_ret,c="red",s=50,edgecolors="black")

# show plot
plt.show()
















################################################################################
#test
################################################################################

#Descargar datos
stocks = yf.download(ticker,'2021-1-1','2022-1-1')['Adj Close']


# Plot all the close prices
((stocks.pct_change()+1).cumprod()).plot(figsize=(10, 7))

# Show the legend
plt.legend()

# Define the label for the title of the figure
plt.title("Returns", fontsize=16)

# Define the labels for x-axis and y-axis
plt.ylabel('Cumulative Returns', fontsize=14)
plt.xlabel('Year', fontsize=14)

# Plot the grid lines
plt.grid(which="major", color='k', linestyle='-.', linewidth=0.5)
plt.show()

#------------------------------------------------------------------------------
#Estimar retornos



pesos_m=[df.iloc[sharpe_CVaR.argmax()][0] , 
         df.iloc[sharpe_CVaR.argmax()][1], 
         df.iloc[sharpe_CVaR.argmax()][2],
         df.iloc[sharpe_CVaR.argmax()][3],
         df.iloc[sharpe_CVaR.argmax()][4],
         df.iloc[sharpe_CVaR.argmax()][5],
         ]  




pesos_m=[1/6, 1/6, 1/6, 1/6, 1/6, 1/6]


vv=np.dot((stocks.pct_change()+1).cumprod(), pesos_m)



t=range(1,(len(vv)+1))

plt.figure(figsize=(12,8))
plt.scatter(t,vv)
plt.show()



len(vv)
len(t)
