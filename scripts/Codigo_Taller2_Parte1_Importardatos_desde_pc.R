#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 2: Predicting Poverty                      #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 22/02/2023 


# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx, rio)


# 1.1 Directorio de trabajo 

#Iván
#setwd("D:/2023/ANDES/Big data/Taller2/Taller2_Big_Data")
setwd("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 2")

# 1.2 Importar datos 


train_personas <- read.csv("data_ignore/train_personas.csv")
train_hogares <- read.csv("data_ignore/train_hogares.csv")
test_personas <- read.csv("data_ignore/test_personas.csv")
test_hogares <- read.csv("data_ignore/test_hogares.csv")
#sample_submission <- read.csv("data_ignore/sample_submission.csv")

