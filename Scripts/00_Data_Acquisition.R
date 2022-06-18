######################################################

        #Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebasti??n Dur??n Dur??n
######################################################
#Por la fecha, el scraping se hizo siguiendo esta p??gina: https://datanalytics.com/libro_r/web-scraping.html
# 00. Data Acquisition #

#Cargamos los paquetes necesarios----------------------
install.packages("rvest")
install.packages("dplyr")
library(dplyr)
library(rvest) # Para scraping
library(pacman)
library(tidyverse)
p_load(rio) # Librer??a para importar datos 
p_load(tidyverse) # Librer??a para limpiar datos
p_load(e1071) # Tiene la funci??n para calcular skewness
p_load(EnvStats) # Transformaci??n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer??a para visualizar datos
p_load(scales) # Formato de los ejes en las gr??ficas
p_load(ggpubr) # Combinar gr??ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(rvest)


#Limpiamos todo pos si las moscas ---------------------
rm(list = ls())

#Comenzamos el Scrapping-------------------------------

#####################################################

# 1.a Scrape the data at the website #

####################################################

#cargamos el url--------------------------------------
url_scraping <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html"

geih <- data.frame()
for (i in 1:10) {
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i,".html")
  temp <- read_html(url) %>% 
    html_table()
  geih <- rbin(geih, temp)
  }

geih <- data.frame()

  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
  temp <- read_html(url) %>% 
    html_table()
  geih <- rbin(geih, temp)


for (i in 1:10){
temp <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"))%>%
html_table()
geih <- data.frame()
geih <- temp
rbind.data.frame(geih)
}

  geih <- data.frame() 
  for (i in 1:2){
    temp <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"))%>%
    html_table()
    geih <- temp
    rbind(geih)
  }
  summary(geih)
  skim(geih)
  