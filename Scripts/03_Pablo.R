#PUNTO 3 

#3.1 Peak age by bootstrap
install.packages("boot")
library(boot)
#creamos una función para las estadísticas
#con las que haremos bootstrap
SE <- function(GEIH_clean, index){
  coef(lm(ingtot~age+age2, data=GEIH_clean, subset =  index))
}



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
