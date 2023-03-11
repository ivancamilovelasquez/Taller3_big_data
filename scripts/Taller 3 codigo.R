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

#Cambiar nombre de columnas ----
colnames(test_final)[28] <- "dist_min_train_parque"
colnames(test_final)[29] <- "dist_min_train_fitness"
colnames(test_final)[30] <- "dist_min_train_fitnesse"
colnames(test_final)[31] <- "dist_min_train_playground"
colnames(test_final)[33] <- "dist_min_train_sportc"
colnames(test_final)[34] <- "dist_hospital_centre_train"
colnames(test_final)[35] <- "dist_clinic_centre_train"
colnames(test_final)[36] <- "dist_busstation_centre_train"
colnames(test_final)[37] <- "dist_police_centre_train" 
colnames(test_final)[38] <- "dist_pub_centre_train"
colnames(test_final)[39] <- "dist_school_centre_train"
colnames(test_final)[40] <- "dist_cinema_centre_train"
colnames(test_final)[41] <- "dist_restaurante_centre_train"

train_final$garaje <- as.factor(train_final$garaje)
train_final$piscina <-  as.factor(train_final$piscina)
train_final$terraza <- as.factor(train_final$terraza)
train_final$campestre <- as.factor(train_final$campestre) 

test_final$garaje <- as.factor(test_final$garaje)
test_final$piscina <-  as.factor(test_final$piscina)
test_final$terraza <- as.factor(test_final$terraza)
test_final$campestre <- as.factor(test_final$campestre) 

#Partir la base de Train en 2: t_train y t_test
p_load(caret)

set.seed(1234)

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

