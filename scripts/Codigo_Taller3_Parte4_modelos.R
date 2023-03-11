#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Making Money with ML?                     #
# 
#
# Código Mapas totales
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 7/03/2023 

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(tidyverse, rstudioapi, rio, leaflet, rgeos, tmaptools, sf, stargazer,osmdata, plotly)

test_final <- read.csv("~/MAESTRIA/Taller 3 Big Data/test_final.csv")
train_final <- read.csv("~/MAESTRIA/Taller 3 Big Data/train_final.csv")


#Partir la base de Train en 2: t_train y t_test
p_load(caret)

inTrain <- createDataPartition(
  y = train_final$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


t_train <- train_final[ inTrain,]
t_test  <- train_final[-inTrain,]

# train_final$a <- ifelse(train_final$surface_covered != 0, 1, 0)
# 
# prop.table(table(train_final$a, exclude = NULL))
# 
# train_final <- train_final[,-41]

