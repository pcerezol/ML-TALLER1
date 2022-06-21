#Punto 4
GEIH_clean <- filter(GEIH_clean, ingtot>0)
GEIH_clean<-GEIH_clean[complete.cases(GEIH_clean$ingtot),]
summary(GEIH_clean$log_Ing)
GEIH_clean<-GEIH_clean%>%
  mutate(log_Ing=log(ingtot))
modelo2<-lm(log_Ing~sex, data=GEIH_clean)
summary(modelo2)
summary(GEIH_clean$log_Ing)

#############Bootstrap
library(boot)
SE2 <- function(GEIH_clean, index){
  coef(lm(log_Ing~sex, data=GEIH_clean), data=GEIH_clean, subset = index)
}
boot(data=GEIH_clean, SE2, R=1200)