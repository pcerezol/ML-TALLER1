######################################################

#Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebastián Durán Durán

######################################################

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

#2.1 Variables de interés------------------------------

#Dejamos mayores de edad y trabajadores

GEIH_ocupados<-data.frame()
GEIH_ocupados<-subset(GEIH, age>=18 & ocu==1)

#Creamos interacciones de variables de interés

GEIH_ocupados <- GEIH_ocupados %>%
  mutate(age2 = age^2,
         age_sex = age*sex,
         age_sex2 = age2*sex,
         log_Ing = log(ingtot),
         formal_sex = formal*sex,
         realb_sex = relab*sex,
  )

#Creamos el df con las variables de interés

GEIH_clean <- subset(GEIH_ocupados, select = c (sex, ingtot, age, age2, 
                                                age_sex, age_sex2, formal,
                                                relab,maxEducLevel, depto,
                                                clase, p6426, log_Ing, formal_sex,
                                                realb_sex))

#creamos tiempo de estudio de la persona de acuerdo con el
# nivel de educación alcanzado

GEIH_clean <- GEIH_clean %>% 
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

#fin limpieza de la base







#3.1 Peak age by bootstrap
install.packages("boot")
library(boot)
#creamos una función para las estadísticas
#con las que haremos bootstrap
SE <- function(GEIH_clean, index,
               age_bar=mean(GEIH_clean$ingtot)){
  f<-lm(ingtot~age+age2,GEIH_clean,subset = index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  elastpt<-b2+2*b3*age_bar
  return(elastpt)
}
               
                                                  

boot(data=GEIH_clean, SE, R=1000)


######################################################

# 4. The earnings #

######################################################


######################################################

# 5. Predicting earnings#

######################################################