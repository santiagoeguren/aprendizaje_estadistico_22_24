
library(cvar)





var_1=0.9
var_2=0.1
cov=0




corr=cov/(sqrt(var_1)*sqrt(var_2))
corr



w_1=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
w_2=1-w_1


var_port=NULL
VaRe=NULL
ES=NULL



i=1

while (i<=11) {
  
  var_port[i]=w_1[i]**2*var_1+w_2[i]**2*var_2+2*w_1[i]*w_2[i]*cov
  
  
  VaRe[i]=VaR(qnorm, 0.05, mean = 0, sd = sqrt(var_port[i]))
  
  
  ES[i]=ES(qnorm, 0.05, mean = 0, sd = sqrt(var_port[i]))
  
  
  
  i=i+1
  
 
  
  
}

dat=data.frame(w_1=w_1,w_2=w_2,var_port=var_port,VaRe=VaRe,ES=ES)



