######################################################

#Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebasti√°n Dur√°n Dur√°n

######################################################


#Es posible que requieran instalar officer y flextables para exportar a word o excel
# install.packages("officer", "flextable", "openxlsx")
#quick_docx(tabla_regs, file = "out/tabla_regs.docx"


# 1.a Scrape the data at the website #

####################################################

#limpiamos---------------------------------------------
rm(list = ls())
#Cargamos los paquetes necesarios----------------------
library(tidyverse)
library(rvest)
library(pacman)
library(dplyr)
library(readr, warn.conflicts = FALSE)
library(broom)
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

#2.1 Variables de inter√©s------------------------------

#Dejamos mayores de edad y trabajadores

GEIH_ocupados<-data.frame()
GEIH_ocupados<-subset(GEIH, age>=18 & ocu==1)

#Creamos interacciones de variables de inter√©s

GEIH_ocupados <- GEIH_ocupados %>%
  mutate(age2 = age^2,
         age_sex = age*sex,
         age_sex2 = age2*sex,
         log_Ing = log(ingtot),
         formal_sex = formal*sex,
         realb_sex = relab*sex,
  )

#Creamos el df con las variables de inter√©s

GEIH_clean <- subset(GEIH_ocupados, select = c ( Var.1, dominio, sex, ingtot, age, age2, 
                                                age_sex, age_sex2, formal,
                                                relab,maxEducLevel, depto,
                                                clase, p6426, log_Ing, formal_sex,
                                                realb_sex))


#creamos tiempo de estudio de la persona de acuerdo con el
# nivel de educaci√≥n alcanzado


###para remplazar los datos de una teniendo en cuenta otra variable GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 3] <- 4

GEIH_clean <- GEIH_clean  %>% 

  mutate(educ=0)

GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 3] <- 4
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 4] <- 5
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 5] <- 10
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 6] <- 11
GEIH_clean['educ'][GEIH_clean['maxEducLevel'] == 7] <- 15


#Creamos educ al cuadrado
  GEIH_clean <- GEIH_clean  %>% 
  mutate(educ2=educ^2)
  
summary(GEIH_clean$educ2)

#### estadÌsticas descriptivas


install.packages("tableone")
library(tableone)

vardesc <- c("sex","ingtot","age","formal","clase","educ","p6426", "relab")

GEIH_clean <- GEIH_clean %>%
  rename(tiempo_tra = p6426, tipo_ocu  = relab, urbano = clase)

summary(GEIH_clean)

skim(GEIH_clean)

install.packages("skim")
library(skimr)

tabla1 <- CreateTableOne(data = GEIH_clean, vars = vardesc)
tabla1

install.packages("writexl")
library(writexl)


#####se le agrega etiqueta a la variable female

levels(GEIH_clean$female) <- list("0" = "Hombre",
                                  "1" = "Mujer")

levels(GEIH_clean$educ) <- list("0" = "Ninguno",
                                "4" = "Primaria_inc",
                                "5" = "Primaria_com",
                                "10" = "Secundaria_inc",
                                "11" = "Secundaria_com",
                                "15" = "Educacion_sup")

###################################################
############    Gr·ficas      ######################

install.packages("ggplot2")
library(ggplot2)


####### Gr·fica de barras para el gÈnero

ggplot(GEIH_clean, aes(x=female)) + geom_bar(width=0.5, colour="red", fill="skyblue")
+ labs(x= female,y= Frecuencia)  + ylim(c(0,10000)) +  ggtitle("GÈnero")  + theme_bw(base_size = 20) + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(1), 
            vjust=-0.5, 
            size=5.0) +   scale_fill_discrete(name = "female", labels = c("Mujer", "Hombre")) 
 
######Gr·fica para educaciÛn

#### primero se crea una tabla con los valores 

table(GEIH_clean$educ)

###### luego se genera el gr·fico

#### GR·fica de EducaciÛn


factor(GEIH_clean$educ)

####Preguntar como se puede poner dos varibles en una misma gr·fica que me muestre por sexo esta variable de educaciÛn

ggplot(GEIH_clean, aes(x= as.factor(educ), fill = sex )) + geom_bar(width=0.5, colour="red", fill="skyblue") + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0) 

### GR·fica de sexo
####preguntar como se puede cambiar los nombres de abajo

ggplot(GEIH_clean, aes(x= as.factor(sex))) + geom_bar(width=0.5, colour="red", fill="skyblue") + 
  geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9), vjust=-0.5,  size=5.0) 

factor(GEIH_clean$educ)

grafica <- GEIH_clean


grafica <- add_labels(grafica$educ, labels = c( `Sin_educ`= 0, 
                                                `Primaria_inc` = 4,
                                                `Primaria_com` = 5,
                                                `Secundaria_inc`= 10,
                                                `Secundaria_com`= 11,
                                                `Terciaria` = 15))

grafica <- factor(c(0,4,5,10,11,15),labels=c("Ninguno", "Primaria_inc", "Primaria_com", "Secundaria_inc", "Secundaria_com", "Educacion_sup"))

levels(GEIH_clean$educ) <- list("0" = "Ninguno",
                                "4" = "Primaria_inc",
                                "5" = "Primaria_com",
                                "10" = "Secundaria_inc",
                                "11" = "Secundaria_com",
                                "15" = "Educacion_sup")

gr·fica['educ'][GEIH_clean['educ'] == 0] <- ""



##### tablas cruzadas

#######paquete de estadÌsticas descriptivas


install.packages("descr")
library(descr)

##### tablas cruzadas y gr·fico

  crosstab(GEIH_clean$educ, GEIH_clean$sex, prop.c = TRUE)

#### saca estadÌsticas descriptivas tambien
  
descr(GEIH_clean$ingtot)

####GR·fica de ingresos

### preguntar como se puede hacer un zoom

ggplot(GEIH_clean, aes(x = ingtot)) + geom_histogram()

 
 ##### para contar cuantos elementos tiene cada
 
 data.frame(table(GEIH_clean$female))
 data.frame(table(GEIH_clean$educ))
 
 table(GEIH_clean$educ)
 
####para sacar la tabla en excel

#write_xlsx(tabla1,"tabla1.xlsx")

#fin limpieza de la base




# 3.  Age-earnings profile

####################################################

##### se realiza el primer modelo

modelo1 <- lm(ingtot ~ age + age2,
              data = GEIH_clean) 
#### primer resumen de los valores que se generaron

summary(modelo1)
install.packages("huxtable")

library(huxtable)
huxreg(modelo1)

install.packages("ggplot2")
library(ggplot2)
predict(modelo1)


#### se crea el valor predicho del ingreso total es decir el "y gorro"

GEIH_clean <- GEIH_clean %>%
  mutate(ingtotpr = predict(modelo1))

ggplot(GEIH_clean, aes(x=age, y=predict(modelo1))) + geom_point() 
+ labs(x='Ingresos', y='Edad', title ='Gr·fico 1') + geom_point(col = "yellow", size = 0.5 ) 



#3.1 Peak age by bootstrap
install.packages("boot")
library(boot)
#creamos una funci√≥n para las estad√≠sticas
#con las que haremos bootstrap
SE <- function(GEIH_clean, index){
  coef(lm(ingtot))
                                                  

boot(data=GEIH_clean, SE, R=1000)


######################################################

# 4. The earnings #

######################################################
library(tidyverse)
GEIH_clean<-GEIH_clean%>%
  mutate(female=sex+1)
summary(GEIH_clean$female)
GEIH_clean['female'][GEIH_clean['female'] == 2] <- 0
modelo2<-lm(log_Ing~female, data=GEIH_clean)
GEIH_clean<-GEIH_clean%>%
  mutate(log_Ing=log10(ingtot))
######################################################

# 5. Predicting earnings#

######################################################
data(GEIH_clean)

set.seed{101010}

indexSet <- sample(2, nrow(GEIH_clean), replace = T, prob = c(0.7, 0.3))

train <- GEIH_clean[indexSet==1,]
test <- GEIH_clean[indexSet==2,]


######################################################



#fin limpieza de la base

