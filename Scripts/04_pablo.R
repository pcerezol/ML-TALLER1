#Punto 4
install.packages("officer")
install.packages("flextable")
#"flextable", "openxlsx", "huxtable"
library(huxtable)
library(dplyr)
library(officer)
library(flextable)
library(boot)
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
  reg_mas <- lm(log_Ing~age, data=GEIH_mas[index, ])
  coef(reg_mas)
}
boot(data=GEIH_mas, SE2_mas, R=1000)

#regresión edad para mujeres
modelo2_fem <- lm(log_Ing~age, data=GEIH_fem)
summary(modelo2_fem)

SE2_fem <- function(GEIH_fem, index){
  reg_fem <- lm(log_Ing~age, data=GEIH_fem[index, ])
  coef(reg_fem)
}
boot(data=GEIH_fem, SE2_fem, R=1000)

###############
#Creamos una tabla para comparar los dos modelos#
tabla_mas_fem <-huxreg("Hombres" = modelo2_mas,
                       "Mujeres" = modelo2_fem)
quick_docx(tabla_mas_fem, file = "tabla_mas_fem.docx")


###################

#### se crea el valor predicho del ingreso total es decir el "y gorro"



##########4.5.4

modelo3<-lm(log_Ing~female+age+relab+formal+educ+p6426, data=GEIH_clean)
summary(modelo3)
tabla_reg <-huxreg(modelo3)
quick_docx(tabla_reg, file = "tabla_reg41.docx")
huxreg(modelo3)


############ 4.5.5
#TML
#Comenzamos encontrando la matriz aniquiladora de los otros coeficientes
matriz1 <- as.matrix(subset(GEIH_clean, select = c( age, relab, formal, educ, p6426)))
matriz1T <- t(matriz1)
matrizXTX <- matriz1T %*% matriz1
matrizXTX_inv <- solve(matrizXTX)
matrizPx <- matriz1 %*% matrizXTX_inv %*% matriz1T
Identidad <- diag(16277)
matriz_aniq <-Identidad - matrizPx
#Hacemos la transformación a Y y variables de interés
NASH <- as.matrix(subset(GEIH_clean, select = c (log_Ing)))
Y_aniq <- matriz_aniq %*% NASH

summary(Y_aniq)
XB2 <- as.matrix(subset(GEIH_clean, select = c (female)))
XB2_aniq <- matriz_aniq %*% XB2
summary(XB2)

TMLmatriz <- cbind(Y_aniq, XB2)
TML <- as.data.frame((TMLmatriz))
