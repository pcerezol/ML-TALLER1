



#Punto 4

modelo2<-lm(log_Ing~female, data=GEIH_clean)
summary(modelo2)


#############Bootstrap

SE2 <- function(GEIH_clean, index){
  coef(lm(log_Ing~female, data=GEIH_clean), data=GEIH_clean, subset = index)
}
boot(data=GEIH_clean, SE2, R=1000)