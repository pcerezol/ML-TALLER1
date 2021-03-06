#Dejamos la base limpia
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


GEIH_ocupados<-data.frame()
GEIH_ocupados<-subset(GEIH, age>=18 & ocu==1 & ingtot>0)

#Creamos interacciones de variables de interés

GEIH_ocupados <- GEIH_ocupados %>%
  mutate(age2 = age^2,
         log_Ing = log(ingtot),
         formal_sex = formal*sex,
         realb_sex = relab*sex,
         educ=0,
         female=sex+1
         
  )
GEIH_ocupados['educ'][GEIH_ocupados['maxEducLevel'] == 3] <- 4
GEIH_ocupados['educ'][GEIH_ocupados['maxEducLevel'] == 4] <- 5
GEIH_ocupados['educ'][GEIH_ocupados['maxEducLevel'] == 5] <- 10
GEIH_ocupados['educ'][GEIH_ocupados['maxEducLevel'] == 6] <- 11
GEIH_ocupados['educ'][GEIH_ocupados['maxEducLevel'] == 7] <- 15
GEIH_ocupados['female'][GEIH_ocupados['female'] == 2] <- 0

GEIH_ocupados <- GEIH_ocupados %>%
mutate(educ2=educ^2,
       age_female=age*female,
       age_female2=age2*female
       )

GEIH_clean <- subset(GEIH_ocupados, select = c ( dominio, ingtot, age, age2, 
                                                 age_female, age_female2, formal,
                                                 relab,maxEducLevel, depto,
                                                 clase, p6426, log_Ing, formal_sex,
                                                 realb_sex, educ, educ2,female ))
#limpiamos NA
GEIH_clean[complete.cases(GEIH_clean),]
#PUNTO 3 

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
#creamos una función para las estadísticas
#con las que haremos bootstrap


######################################################

# 4. The earnings #

#Creamos educ al cuadrado
GEIH_clean <- GEIH_clean  %>% 
  mutate(educ2=educ^2)

summary(GEIH_clean$educ2)

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
