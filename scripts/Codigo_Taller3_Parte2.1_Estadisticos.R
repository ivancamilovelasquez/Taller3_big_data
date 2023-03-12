#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Making Money with ML?                   #
# 
#
# Descriptivos
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 12/03/2023 

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(readr,openxlsx, tidyverse, rstudioapi, rio, leaflet, rgeos, tmaptools, sf, stargazer,osmdata, plotly)

# - Importar base

test_final <- read_csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 3/test_final.csv")

train_final <- read_csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 3/train_final.csv")

#Cambiar nombre de columnas 

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

train_final$garaje <- as.numeric(train_final$garaje)
train_final$piscina <-  as.numeric(train_final$piscina)
train_final$terraza <- as.numeric(train_final$terraza)
train_final$campestre <- as.numeric(train_final$campestre) 


test_final$garaje <- as.numeric(test_final$garaje)
test_final$piscina <-  as.numeric(test_final$piscina)
test_final$terraza <- as.numeric(test_final$terraza)
test_final$campestre <- as.numeric(test_final$campestre) 


# Bases descriptivas 

# Train 

Base_descriptivas <- train_final[c("price","bedrooms", "apto_total", "garaje",
                             "piscina", "terraza", 
                             "campestre","dist_min_train_parque", 
                             "dist_min_train_fitness", "dist_min_train_fitnesse",
                             "dist_min_train_playground", "dist_hospital_centre_train",
                             "dist_clinic_centre_train", "dist_police_centre_train",
                             "dist_school_centre_train", "dist_restaurante_centre_train",
                             "dist_cinema_centre_train")]

estadisticas_todos <- data.frame(sapply(Base_descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 3/Estadisticos_todos.xlsx")


# Test 

Base_descriptivas <- test_final[c("price","bedrooms", "apto_total", "garaje",
                                   "piscina", "terraza", 
                                   "campestre","dist_min_train_parque", 
                                   "dist_min_train_fitness", "dist_min_train_fitnesse",
                                   "dist_min_train_playground", "dist_hospital_centre_train",
                                   "dist_clinic_centre_train", "dist_police_centre_train",
                                   "dist_school_centre_train", "dist_restaurante_centre_train",
                                   "dist_cinema_centre_train")]

estadisticas_todos <- data.frame(sapply(Base_descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 3/Estadisticos_todos_test.xlsx")

