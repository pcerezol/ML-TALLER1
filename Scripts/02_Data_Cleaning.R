######################################################

#Problem Set 1: Predicting Income#

# Authors: Pablo Cerezo Lesmes & Juan Sebasti??n Dur??n Dur??n
######################################################
#Por la fecha, el scraping se hizo siguiendo esta p??gina: https://datanalytics.com/libro_r/web-scraping.html
# 02. Data cleanning #
rm(list = ls())
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
    temp <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"))%>%
    html_table()
    geih <- temp


#2.1 Variables de interés------------------------------

#2.2 Missing variables---------------------------------

#2.3 Tablas y gráficas---------------------------------