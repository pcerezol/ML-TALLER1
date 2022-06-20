######################################################

#Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebasti??n Dur??n Dur??n
######################################################

# 02. Data cleanning #
#Cargamos los paquetes necesarios----------------------
library(rio)
library(tidyverse)
library(e1071)
library(EnvStats)
library(tidymodels)
library(ggplot2)
library(scales)
library(ggpubr)
library(knitr)
library(kableExtra)
library(rvest)
library(pacman)
library(dplyr)

#Cargamos la data--------------------------------------
geih <- data.frame()
for (x in 1:10){
  url <-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
  temp <- read_html(url)%>%
  html_table()  
  geih <- temp
  GEIH <- as.data.frame(geih)
  }
    geih <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")%>%
    html_table()


#2.1 Variables de interés------------------------------
    #Dejamos mayores de edad y trabajadores
    GEIH_ocupados<-data.frame()
    GEIH_ocupados<-subset(GEIH, age>=18 & ocu==1)
    #dejamos variables de interés
    ##    GEIH_ocupados <- GEIH_ocupados %>%
    mutate(age2 = age^2,
           age_sex = age*sex,
           age_sex2 = age2*sex,
           ln_Ing = ln(ingtot),
           formal_sex = formal*sex,
           realb_sex = relab*sex,
           )
    
    GEIH_clean <- subset(GEIH_ocupados, select = c (sex, ingtot, age, age2, 
                                                    age_sex, age_sex2, formal,
                                                    relab,maxEducLevel, depto,
                                                    clase, p6426))
##Base para el punto 3

    GEIH_punto3 <- subset(GEIH_ocupados, select = c (age, age2, ingtot))

##Base para el punto 4

    ##Base para el punto 5
#2.2 Missing variables---------------------------------

#2.3 Tablas y gráficas---------------------------------