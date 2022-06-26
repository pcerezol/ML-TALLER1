#Punto 4
install.packages("officer")
install.packages("flextable")
#"flextable", "openxlsx", "huxtable"
library(huxtable)
library(dplyr)
library(officer)
library(flextable)
setwd("C:/Users/pcere/Dropbox/Machine Learning/ML-TALLER1/Views")
###############Corremos el modelo
modelo2<-lm(log_Ing~female, data=GEIH_clean)
summary(modelo2)
tabla_reg <-huxreg(modelo2)
quick_docx(tabla_reg, file = "tabla_regs.docx")


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

#### se crea el valor predicho del ingreso total es decir el "y gorro"



##########4.5.4

modelo3<-lm(log_Ing~female+age+relab+formal+educ+p6426, data=GEIH_clean)
summary(modelo3)
tabla_reg <-huxreg(modelo3)
quick_docx(tabla_reg, file = "tabla_reg41.docx")
huxreg(modelo3)