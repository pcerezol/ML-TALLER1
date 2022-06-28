###################################

#####    Talller 1        ######

# Script Bigdata & MAchine Learning


#Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan SebastiÃ¡n DurÃ¡n DurÃ¡n

######################################################


#Es posible que requieran instalar officer y flextables para exportar a word o excel
# install.packages("officer", "flextable", "openxlsx")
#quick_docx(tabla_regs, file = "out/tabla_regs.docx"


# 1.a Scrape the data at the website #

####################################################

#limpiamos---------------------------------------------
rm(list = ls())
#Cargamos los paquetes necesarios----------------------
library(tidyverse) #limpieza
library(rvest)     #scraping
library(dplyr)
library(readr, warn.conflicts = FALSE)
library(broom)
library(ggplot2) ###gráficas
library(officer)  #pasarlo a word
library(flextable) #tablas
library(huxtable) #regresiones de tablas
library(tableone) ##tablas
library(boot) ###bootstrap
library(skimr)  #estadísticas descriptivas
library(descr) ###tablas cruzadas
library(tableone) #descriptivas
library(lattice)
#Cargamos la data--------------------------------------
GEIH <- data.frame()
for (i in 1:10){
  url <-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  temp <- read_html(url)%>%
    html_table()  
  temp <- as.data.frame(temp)
  GEIH <- rbind(GEIH,temp)
}
#Fin del scraping

######################################################

# 2. Data cleaning #

######################################################

#2.1 Variables de interÃ©s------------------------------

#Dejamos mayores de edad y trabajadores

GEIH_ocupados<-data.frame()
GEIH_ocupados<-subset(GEIH, age>=18 & ocu==1 & ingtot>0)

#Creamos de variables de interes

GEIH_ocupados <- GEIH_ocupados %>%
  mutate(age2 = age^2,
         age_sex = age*sex,
         age_sex2 = age2*sex,
         log_Ing = log(ingtot),
         formal_sex = formal*sex,
         realb_sex = relab*sex
                  )


###codificación de nombre de variables

GEIH_ocupados <- GEIH_ocupados %>%
  rename(tiempo_tra = p6426, tipo_ocu  = relab, urbano = clase)  


#Creamos el df con las variables de interÃ©s

GEIH_clean <- subset(GEIH_ocupados, select = c ( Var.1, dominio, sex, ingtot, age, age2, 
                                                 age_sex, age_sex2, formal,
                                                 maxEducLevel, depto,
                                                 tiempo_tra, log_Ing, formal_sex,
                                                 realb_sex, urbano, tipo_ocu))
##### Variable Female 

GEIH_clean<-GEIH_clean%>%
  mutate(female=sex+1)
summary(GEIH_clean$female)
GEIH_clean['female'][GEIH_clean['female'] == 2] <- 0


####################################################################


GEIH_clean<-GEIH_clean%>%
  mutate(tiempo_tra2 = tiempo_tra^2)


#creamos tiempo de estudio de la persona de acuerdo con el
# nivel de educaciÃ³n alcanzado

###para remplazar los datos de una teniendo en cuenta otra variable 
#GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 3] <- 4

GEIH_clean <- GEIH_clean  %>% 
  
  mutate(educ=0,
         age_female=age*female,
         age_female2=age2*female
  )

GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 3] <- 4
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 4] <- 5
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 5] <- 10
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 6] <- 11
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 7] <- 15


#####se le agrega etiqueta a la variable female

levels(GEIH_clean$female) <- list("0" = "Hombre",
                                  "1" = "Mujer")

levels(GEIH_clean$educ) <- list("0" = "Ninguno",
                                "4" = "Primaria_inc",
                                "5" = "Primaria_com",
                                "10" = "Secundaria_inc",
                                "11" = "Secundaria_com",
                                "15" = "Educacion_sup")

#limpiamos NA
GEIH_clean[complete.cases(GEIH_clean),]

#### estadisticas descriptivas

####comando general para todas las variables

summary(GEIH_clean)

###saca tabla de estadísticas descriptivas

skim(GEIH_clean)


##vector de variables deriptivas

vardesc <- c("sex", "ingtot", "age","formal", "tipo_ocu", "urbano", "tiempo_tra", "educ")

tabla1 <- CreateTableOne(data = GEIH_clean, vars = vardesc)
tabla1


###################################################
############    Graficas      ######################

####### Grafica de barras para el genero

ggplot(GEIH_clean, aes(x=female)) + geom_bar(width=0.5, colour="red", fill="skyblue")
+ labs(x= female,y= Frecuencia)  + ylim(c(0,10000)) +  ggtitle("G?nero")  + theme_bw(base_size = 20) + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(1), 
            vjust=-0.5, 
            size=5.0) +   scale_fill_discrete(name = "female", labels = c("Mujer", "Hombre")) 

######Grafica para educacion

#### primero se crea una tabla con los valores 

table(GEIH_clean$educ)

###### luego se genera el grafico

#### GRafica de Educacion


factor(GEIH_clean$educ)

####Preguntar como se puede poner dos varibles en una misma gr?fica que me muestre por sexo esta variable de educaci?n

ggplot(GEIH_clean, aes(x= as.factor(educ), fill = sex )) + geom_bar(width=0.5, colour="red", fill="skyblue") + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0) 

### GR?fica de sexo
####preguntar como se puede cambiar los nombres de abajo

ggplot(GEIH_clean, aes(x= as.factor(sex))) + geom_bar(width=0.5, colour="red", fill="skyblue") + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0) 


##### tablas cruzadas y gráfico

crosstab(GEIH_clean$educ, GEIH_clean$sex, prop.c = TRUE)

#### saca estad?sticas descriptivas tambien

descr(GEIH_clean$ingtot)

####GR?fica de ingresos

### preguntar como se puede hacer un zoom

ggplot(GEIH_clean, aes(x = ingtot)) + geom_histogram()


##### para contar cuantos elementos tiene cada

data.frame(table(GEIH_clean$female))
data.frame(table(GEIH_clean$educ))

table(GEIH_clean$educ)



# 3.  Age-earnings profile

####################################################

##### se realiza el primer modelo

modelo1 <- lm(ingtot ~ age + age2, data = GEIH_clean) 

#### primer resumen de los valores que se generaron

summary(modelo1)

huxreg(modelo1)


#### predict
predict(modelo1)


#### se crea el valor predicho del ingreso total es decir el "y gorro"
library("ggplot2")
GEIH_clean <- GEIH_clean %>%
  mutate(ingtotpr = predict(modelo1))

ggplot(GEIH_clean, aes(x=age, y=predict(modelo1))) + geom_point() 
+ labs(x='Ingresos', y='Edad', title ='Grafico 1') + geom_point(col = "yellow", size = 0.5 ) 



#3.1 Peak age by bootstrap
install.packages("boot")
library(boot)

SE <- function(GEIH_clean, index){
  Pablo <- lm(ingtot~age+age2, data=GEIH_clean[index, ])
  coef(Pablo)
}
boot(data=GEIH_clean, SE, R=1000)
modelo1<-lm(ingtot~age+age2, GEIH_clean)
summary(modelo1)
#creamos una funciÃ³n para las estadÃ­sticas
#con las que haremos bootstrap

######################################################

# 4. The earnings #

######################################################

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
modelo3<-lm(log_Ing~female+age+tipo_ocu+formal+educ+tiempo_tra, data=GEIH_clean)
summary(modelo3)
#sacamos la tabla
tabla_reg <-huxreg(modelo3)
quick_docx(tabla_reg, file = "tabla_reg41.docx")



#4.5 (b) Demostración del teorema FWL

#FWL con residuales
#Guardamos los resiguales de log_Inc en los controles
resi_dente <- residuals(lm(log_Ing ~ age+tipo_ocu+formal+educ+tiempo_tra, GEIH_clean))

#Guardamos los resiguales de female en los controles
female_resi <- residuals(lm(female ~ age+tipo_ocu+formal+educ+tiempo_tra, GEIH_clean))

#Corremos la regresión con transofrmaciones
FWL_funcional <- lm(resi_dente ~ female_resi, GEIH_clean)
summary(FWL_funcional)

#Sacamos la tabla
tabla_FWL <-huxreg("Modelo completo" = modelo3,
                   "FWL" = FWL_funcional)
quick_docx(tabla_FWL, file = "tabla_regFWL.docx")






# 5. Predicting earnings#

######################################################
data(GEIH_clean)

set.seed(101010)

GEIH_clean <- GEIH_clean %>% mutate(holdout= as.logical(1:nrow(GEIH_clean) %in%
                                                          sample(nrow(GEIH_clean), nrow(GEIH_clean)*.3)))

test <- GEIH_clean[GEIH_clean$holdout==T,]
train <- GEIH_clean[GEIH_clean$holdout==F,]

###i. Estimate a model that only includes a constant. This will be the benchmark.

y <- rnorm(1000)

intercepto <- lm(y~1)


#ii. Estimate again your previous models

###primer modelo

modelo1 <- lm(ingtot ~ age + age2,
              data = GEIH_clean) 

GEIH_clean<-GEIH_clean%>%
  mutate(female=sex+1)
summary(GEIH_clean$female)
GEIH_clean['female'][GEIH_clean['female'] == 2] <- 0
modelo2<-lm(log_Ing~female, data=GEIH_clean)
GEIH_clean<-GEIH_clean%>%
  mutate(log_Ing=log10(ingtot))

modelo2 <- lm(log_Ing~female, data=GEIH_clean)

install.packages("huxreg")
library(huxtable)

regresiones <- huxreg(intercepto, modelo1, modelo2)

install.packages("officer")
install.packages("flextable")
library(officer)
library(flextable)

setwd("C:\Users\ASUS\OneDrive - Universidad de los Andes\Documentos\Respaldo\Escritorio")

quick_docx(regresiones, file = "tabla_regs.docx")


#iii. In the previous sections, the estimated models had different transformations of the dependent variable. At this point, explore other transformations of
#your independent variables also. For example, you can include polynomial
#terms of certain controls or interactions of these. Try at least five (5) models
#that are increasing in complexity.


modelo3 <-lm(ingtot~female+age+age2+relab+formal+educ+educ2+p6426+clase+ formal_sex + realb_sex+age_sex + age_sex2, data=GEIH_clean)

summary(modelo3)

#iv. Report and compare the average prediction error of all the models that
#you estimated before. Discuss the model with the lowest average prediction error.


modelo4 <-lm(ingtot~female+age+age2,  data=GEIH_clean )

summary(modelo4)

modelo5<- lm(ingtot~female+age+age2+ tipo_ocu,  data=GEIH_clean)

summary(modelo5)

modelo6 <- lm(ingtot~female+age+age2+tipo_ocu+formal, data=GEIH_clean)

summary(modelo6)

modelo7<- lm(ingtot~female+age+age2+tipo_ocu+formal+educ+educ2 + realb_sex, data=GEIH_clean )

summary(modelo7)
               
modelo8<- lm(ingtot~female+age+age2+tipo_ocu+formal+educ+educ2+ tiempo_tra+ tiempo_tra2 + formal_sex + realb_sex+age_sex + age_sex2, data=GEIH_clean)

summary(modelo8)

##########para modelo 4

data_errores <- data.frame(pred = predict(modelo4), actual = GEIH_clean$ingtot)

mean((data_errores$actual - data_errores$pred)^2)


###########para modelo 5

data_errores2 <- data.frame(pred = predict(modelo5), actual = GEIH_clean$ingtot)

mean((data_errores2$actual - data_errores2$pred)^2)


###########para modelo 6

data_errores3 <- data.frame(pred = predict(modelo6), actual = GEIH_clean$ingtot)

mean((data_errores3$actual - data_errores3$pred)^2)

###########para modelo 7


data_errores4 <- data.frame(pred = predict(modelo7), actual = GEIH_clean$ingtot)

mean((data_errores4$actual - data_errores4$pred)^2)

###########para modelo 8

data_errores5 <- data.frame(pred = predict(modelo8), actual = GEIH_clean$ingtot)

mean((data_errores5$actual - data_errores5$pred)^2)


##display leverage stats for each observation

summary(test)

modelo9 <- lm(ingtot~female+age+age2+tipo_ocu+formal+educ+educ2+ tiempo_tra+ tiempo_tra2 + formal_sex + realb_sex+age_sex + age_sex2, data=test)

summary(modelo9)

######calculate leverage for each observation in the model

hats <- as.data.frame(hatvalues(modelo9))

hats

head(hats)
tail(hats)



#### K-fold cross-validation

install.packages("caret")

library(caret)

ctrl <- trainControl(method = "cv", number = 5)


model14 <- train(ingtot~female+age+age2, data=test, method ="lm", trControl = ctrl)

model13 <- train(ingtot~female+age+age2+ tipo_ocu, data=test, method ="lm", trControl = ctrl)

model12 <- train(ingtot~female+age+age2+tipo_ocu+formal,data=test, method ="lm", trControl = ctrl)

model11 <- train(ingtot~female+age+age2+tipo_ocu+formal+educ+educ2 + realb_sex, data=test, method ="lm", trControl = ctrl)

model10 <- train(ingtot~female+age+age2+tipo_ocu+formal+educ+educ2+tiempo_tra+tiempo_tra2 + formal_sex + realb_sex+age_sex + age_sex2, data=test, method ="lm", trControl = ctrl)
  

print(model10)
print(model14)
print(model13)
print(model12)
print(model11)

##5.c LOOCV
#La idea es crear N samples de data eliminando la observación j del loop aprovechando la variable Var.1
#Luego, se guardan los predichos en una matriz para calcular el average mean square error
LOOCVmatrix <- c()
for (j in 1:16277){
  
  GEIH_LOOCV_j <- subset(GEIH_clean, Var=! j)

LOOCVmatrix[j] <-predict(modelo8, GEIH_LOOCV_j)

  
  }



































