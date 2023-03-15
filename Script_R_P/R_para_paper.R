################################################################################
#Libreria
################################################################################


library(ggplot2)
library(npsm)


################################################################################
#Establacer directorio
################################################################################

getwd()
setwd("/home/santiago/aprendizaje_estadistico_22_24/Base_datos")


################################################################################
#Descargar datos
################################################################################

load("ES_1_n.Rdata")
load("ES_Estimado.Rdata")
load("ES_pso.Rdata")



#-------------------------------------------------------------------------------
#Armar data frame


ES=c(ES_1_n,ES_pso)
Type=as.factor(c(rep("Equal Weight",100),rep("Optimisazed",100)))

dat=data.frame(ES=ES,Type=Type)


g = ggplot(dat, aes(y=ES,x=Type)) 
g= g + geom_boxplot(aes(fill=Type)) 
g=g+geom_jitter(alpha = 0.5, color = "tomato")
g=g+ylab("ES")
g

#-------------------------------------------------------------------------------
#Comprobar normalidad

shapiro.test(ES_1_n)
shapiro.test(ES_pso)


#-------------------------------------------------------------------------------
var.test(ES_pso,ES_1_n)
var.test(ES_pso,ES_Estimado)


t.test(ES_pso,ES_1_n)
t.test(ES_pso,ES_Estimado)

#-------------------------------------------------------------------------------
#Comprar la dispercion


#--------------------------------------------------------------------------------------
#Comparar la dispercion - Test de Fligner y Killeen
#--------------------------------------------------------------------------------------


#H0: Var1 = Var2      vs       Var1 distinto de Var2


# Using fligner.test()
result = fligner.test(ES ~ Type)

# print the result
print(result)




#H0: Var1 = Var2      vs       Var1 distinto de Var2

fk.test(ES_pso,ES_1_n)









#--------------------------------------------------------------------------------------
#Comparar las mediana - Dispercion desigual - Fligner-Policello
#--------------------------------------------------------------------------------------

#H0: mediana1 = mediana2         vs          H1: mediana1 distinto de mediana2



fp.test(ES_pso,ES_1_n)


