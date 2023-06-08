library(tidyverse)
library(dplyr)
dados <- read.csv("premier2020_21.csv", dec=".")
dados <- mutate(dados,numero_total= FTHG + FTAG)
summary(dados)
attach(dados)

dataa=dados[1:70,]
TGa=dataa$numero_total
ajuste=lm(TGa~FTHGb+FTAGb,data=dataa)
summary(ajuste)
k=31
n=10
stake=100
lucro=c(0)
lucrorodada=c(0)
propacertorodada=c(0)
for (i in 1:k){
  dataa=dados[1:(60+10*i),]
  TGa=numero_total[1:(60+10*i)]
  ajuste=lm(TGa~FTHGb+FTAGb-1,data=dataa)
  gols_avg= (ajuste$coefficients[1]*dados$FTHGb[(60+10*i+1):(70+10*i)]+dados$FTAGb[(60+10*i+1):(70+10*i)]*ajuste$coefficients[2])
  lucro=c(0)
  cont=0
  m=0
  for (j in 1:n){ 
    if (((gols_avg[j]>2.5))){ #analisar as restrições
      m=m+1
      if (dados$numero_total[(60+10*i+j)]>2.5){ #comparar as dimensões, entrada a entrada
        lucro[j]=stake*(dados$OddO2.5[(60+10*i+j)]-1) 
        cont=cont+1
      }else{
        lucro[j]=-stake
        cont=cont
      }
    }else{
      m=m
      lucro[j]=0
      
      }
    
  
    
  }
  propacertorodada[i]=cont/m
  lucrorodada[i]=sum(lucro)
  
}

lucrorodada
propacertorodada  
lucroTemporada=sum(lucrorodada)
lucroTemporada
mean(propacertorodada,na.rm=T)
median(propacertorodada,na.rm=T)
lucroacumulado=cumsum(lucrorodada)
plot(lucroacumulado,col="blue",type="h")