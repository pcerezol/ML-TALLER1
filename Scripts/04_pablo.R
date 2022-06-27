#####################
     #Punto 4#
#####################
install.packages("officer")
install.packages("flextable")
#"flextable", "openxlsx", "huxtable"
library(huxtable)
library(dplyr)
library(officer)
library(flextable)
library(boot)

#Definimos el escritorio para sacar tablas y gráficas#

setwd("C:/Users/pcere/Dropbox/Machine Learning/ML-TALLER1/Views")

#4.1 modelo simple log_Ing ~ female

modelo2 <- lm(log_Ing~female, data=GEIH_clean)
summary(modelo2)
tabla_reg <-huxreg(modelo2)
quick_docx(tabla_reg, file = "tabla_regs.docx")

#4.3 Estimate and plot the predicted age-earnings profile by gender. 

#La base únicamente contiene información de Bogotá

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

#Creamos una tabla para comparar los dos modelos
tabla_mas_fem <-huxreg("Hombres" = modelo2_mas,
                       "Mujeres" = modelo2_fem)
quick_docx(tabla_mas_fem, file = "tabla_mas_fem.docx")

#Sacamos la gráfica fachera de comparación
ggplot()+geom_point(data=GEIH_mas , aes(x=age, y=predict(modelo2_mas), color = "Hombres" ))+
  geom_point(data=GEIH_fem , aes(x=age, y=predict(modelo2_fem), color = "Mujeres"))+
  theme(panel.background = element_rect(fill = "white"))+
  labs(y= "Log(Ingreso Total)", x = "Edad")+
  theme(legend.title = element_blank())



#4############Bootstrap

SE2 <- function(GEIH_clean, index){
  reg <- lm(log_Ing~female, data=GEIH_clean[index, ])
  coef(reg)
}
boot(data=GEIH_clean, SE2, R=1000)

##########4.5
#4.5 (a) Regresión con múltiples controles
modelo3<-lm(log_Ing~female+age+relab+formal+educ+p6426, data=GEIH_clean)
summary(modelo3)
#sacamos la tabla
tabla_reg <-huxreg(modelo3)
quick_docx(tabla_reg, file = "tabla_reg41.docx")



#4.5 (b) Demostración del teorema FWL

#FWL con residuales
#Guardamos los resiguales de log_Inc en los controles
resi_dente <- residuals(lm(log_Ing ~ age+relab+formal+educ+p6426, GEIH_clean))

#Guardamos los resiguales de female en los controles
female_resi <- residuals(lm(female ~ relab+formal+educ+p6426, GEIH_clean))

#Corremos la regresión con transofrmaciones
FWL_funcional <- lm(resi_dente ~ female_resi, GEIH_clean)
summary(FWL_funcional)

#Sacamos la tabla
tabla_FWL <-huxreg("Modelo completo" = modelo3,
                   "FWL" = FWL_funcional)
quick_docx(tabla_FWL, file = "tabla_regFWL.docx")
