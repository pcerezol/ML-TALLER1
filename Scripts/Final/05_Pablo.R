#####################
      #Punto 5#
#####################

######################################################

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


modelo3 <-lm(ingtot ~ female+age+age2+relab+formal+educ+educ2+p6426+clase+ formal_sex + realb_sex+age_sex + age_sex2, data=GEIH_clean)

#Creamos 5 modelos

modelo5_1 <- lm(ingtot ~ tiempo_tra + tiempo_tra2, GEIH_clean)

modelo5_2 <- lm(ingtot ~ educ + educ2 + tiempo_tra, GEIH_clean)

modelo5_3 <- lm(ingtot ~ female + female_educ + tiempo_tra + tiempo_tra2, GEIH_clean)

modelo5_4 <- lm(ingtot ~ educ + educ2 + tiempo_tra + tiempo_tra2, GEIH_clean)

modelo5_5 <- lm(ingtot ~ female + formal_female + educ + educ2 + tiempo_tra + tiempo_tra2 + tipo_ocu, GEIH_clean)

#iv. Report and compare the average prediction error of all the models that
#you estimated before. Discuss the model with the lowest average prediction error.

######################################################


##########################################









