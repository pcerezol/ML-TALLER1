######################################################

        #Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebasti??n Dur??n Dur??n
######################################################
#Por la fecha, el scraping se hizo siguiendo esta p??gina: https://datanalytics.com/libro_r/web-scraping.html
# 00. Data Acquisition #

#Cargamos los paquetes necesarios----------------------
install.packages("rvest")
library(rvest) # Para scraping
library(pacman)
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

tmp <- read_html(url_scraping)

tmp <- html_node(tmp, "table")
length(tmp)
sapply(tmp, class)

aurelio <- 2+2








