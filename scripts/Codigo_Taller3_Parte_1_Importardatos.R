#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 3: Making Money with ML?                     #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 12/03/2023 


# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

library(pacman)
p_load(tidyverse, ggplot2, openxlsx, scales, skimr, stringi, SnowballC)


# 1. Definir directorio 

#Iván
setwd("D:/2023/ANDES/Big data/Taller3")


# 1.2 Importar datos 

test  <- read.csv("data_ignore/test.csv")
train <- read.csv("data_ignore/train.csv")



# 1.3 Explorar Missings

skim(train)

# Datos faltantes

tabla_missings <- apply(train, 2, function(x) sum(is.na(x)))
tabla_missings <- table(tabla_missings)
tabla_missings

# Tenemos 10 columnas con o cero missings. una columna con 9 missings
# 1 una columna con 30079 missings
names(train)[which(names(tabla_missings) == 30079 )]
# variables con muchos missings ; surface_total, surface_covered, 
#rooms, bathrooms 


skim(test)
# variables con muchos missings ; surface_total, surface_covered, 
#rooms, bathrooms


# 1.3 Completar informacion con expresiones regulares

names(train)[c("title")] <- "titulo"
names(test)[c("title")] <-  "titulo"

objetos <- c("train", "test")
for (obj in objetos) {

#Eliminar tildes 
data$title_modificado <- stri_trans_general(str = data$title , 
                                             id = "Latin-ASCII")


#Quitar comas, guiones y otros caracteres especiales o signos de puntuación
data$title_modificado <- gsub('[^A-Za-z0-9 ]+', ' ', data$title_modificado)


# Todo en minuscula
data$title_modificado <- tolower(data$title_modificado)

# Un solo espacio
data$title_modificado <- gsub('\\s+', ' ', data$title_modificado)


# Variable casa y apartamento

data$apartamento_m <- grepl("\\bapartamento\\b", data$title_modificado)
data$aparta_estudio <- grepl("\\bapartaestudio\\b", data$title_modificado)
data$apto <- grepl("\\bapto\\b", data$title_modificado)
data$duplex <- grepl("\\duplex\\b", data$title_modificado)
data$penthouse <- grepl("\\penthouse\\b", data$title_modificado)
data$pent <- grepl("\\pent\\b", data$title_modificado)
data$ph <- grepl("\\ph\\b", data$title_modificado)
data$ap <- grepl("\\ap\\b", data$title_modificado)
data$aparta <- grepl("\\aparta\\b", data$title_modificado)


data$apartamento_m <- ifelse(data$apartamento_m, 1, 0)
data$aparta_estudio <- ifelse(data$aparta_estudio, 1, 0)
data$apto <- ifelse(data$apto, 1, 0)
data$duplex <- ifelse(data$duplex, 1, 0)
data$penthouse <- ifelse(data$penthouse, 1, 0)
data$pent <- ifelse(data$pent, 1, 0)
data$ph <- ifelse(data$ph, 1, 0)
data$ap <- ifelse(data$ap, 1, 0)
data$aparta <- ifelse(data$aparta, 1, 0)


data$apto_total <- ifelse(data$apartamento_m == 1 | data$aparta_estudio == 1 | data$apto == 1 | data$duplex==1 | data$penthouse ==1 |data$pent==1 | data$ph ==1 | data$ap ==1 |data$aparta, 1, 0)
data <- subset(data, select = -c(apartamento_m, aparta_estudio, apto, 
                                   duplex,penthouse, pent,ph, ap, aparta))




data$casa <- grepl("\\bcasa\\b", data$title_modificado)
data$casa <- ifelse(data$casa, 1, 0)


}



agdfggsdgsdg












