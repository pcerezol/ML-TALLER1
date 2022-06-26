



#Punto 4
install.packages("officer", "flextable", "openxlsx", "huxtable")

library(officer)
library(flextable)
library(openxlsx)
library(huxtable)
library(dplyr)
###############Corremos el modelo
modelo2<-lm(log_Ing~female, data=GEIH_clean)
summary(modelo2)
tabla_reg <-huxreg(modelo2)
#huxreg(modelo2)

#############Bootstrap

SE2 <- function(GEIH_clean, index){
reg <- lm(log_Ing~female, data=GEIH_clean[index, ])
  coef(reg)
}
boot(data=GEIH_clean, SE2, R=1000)










###################
#La base únicamente contiene información de Bogotá
##############
#Creamos sets de data solo con hombres y mujeres
GEIH_mas<-subset(GEIH_clean, female==0)
GEIH_fem<-subset(GEIH_clean, female==1)

#regresión edad para hombres
modelo2_mas <- lm(log_Ing~age, data=GEIH_mas)
predict(modelo2_mas)
summary(modelo2_mas)
SE2_mas <- function(GEIH_mas, index){
  coef(lm(log_Ing~age, data=GEIH_mas), data=GEIH_mas, subset = index)
}
boot(data=GEIH_mas, SE2_mas, R=1000)

#regresión edad para mujeres
modelo2_fem <- lm(log_Ing~age, data=GEIH_fem)
summary(modelo2_fem)

SE2_fem <- function(GEIH_fem, index){
  coef(lm(log_Ing~age, data=GEIH_fem), data=GEIH_fem, subset = index)
}
boot(data=GEIH_fem, SE2_fem, R=1000)


###################
library("tidyverse")
library("dplyr")
grafica_sexo<-data.frame()
grafica_sexo<- GEIH_mas%>%{
mutate(Fem=  predict(lm(log_Ing~age, data=GEIH_fem)))
}

###################

#### se crea el valor predicho del ingreso total es decir el "y gorro"
library("ggplot2")

ggplot()+geom_point(data=GEIH_mas , aes(x=age, y=predict(modelo2_mas)), colours="#69b3a2")+
geom_point(data=GEIH_fem , aes(x=age, y=predict(modelo2_fem)))

##################33

##########4.5.4

modelo3<-lm(log_Ing~female+age+relab+formal+educ+p6426, data=GEIH_clean)
summary(modelo3)
huxreg(modelo3)