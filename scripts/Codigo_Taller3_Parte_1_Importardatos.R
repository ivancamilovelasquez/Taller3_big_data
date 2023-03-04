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
p_load(tidyverse, ggplot2, openxlsx, scales, skimr, stringi, SnowballC, stringr)


# 1. Definir directorio 

#Iván
setwd("D:/2023/ANDES/Big data/Taller3")
#setwd("C:/Users/Ivan/Documents/Documento 2023/Andes/Big data/Taller3_big_data")


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

colnames(train)[colnames(train) == "title"] <- "titulo"
colnames(test)[colnames(test) == "title"] <- "titulo"


objetos <- c("train", "test")
for (obj in objetos) {
  
  
data <- get(obj)    

 
#1.3.1 Eliminar tildes 
data$title_modificado <- stri_trans_general(str = data$titulo , 
                                              id = "Latin-ASCII")
 

 
#1.3.2 Quitar comas, guiones y otros caracteres especiales o signos de puntuación
data$title_modificado <- gsub('[^A-Za-z0-9 ]+', ' ', data$title_modificado)


#1.3.3 Todo en minuscula
data$title_modificado <- tolower(data$title_modificado)

#1.3.4 Un solo espacio
data$title_modificado <- gsub('\\s+', ' ', data$title_modificado)


#1.3.5 crear las variables casa y apartamento

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


#1.3.6 crear la variable metros cuadrados

#Expresiones regulares de metros cuadrados 
data$numero_antes_m2 <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*m2)")
data$numero_despues_m2 <- str_extract(data$description, "(?<=m2\\s)\\d{2,3}")
data$numero_antes_metros <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*metros)")
data$numero_despues_metros <- str_extract(data$description, "(?<=metros\\s)\\d{2,3}")
data$numero_antes_mt <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*mt)")
data$numero_despues_mt <- str_extract(data$description, "(?<=mt\\s)\\d{2,3}")
data$numero_antes_mts <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*mts)")
data$numero_despues_mts <- str_extract(data$description, "(?<=mts\\s)\\d{2,3}")
data$numero_antes_mt2 <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*mt2)")
data$numero_despues_mt2 <- str_extract(data$description, "(?<=mt2\\s)\\d{2,3}")
data$numero_antes_mts2 <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*mts2)")
data$numero_despues_mts2 <- str_extract(data$description, "(?<=mts2\\s)\\d{2,3}")
data$numero_antes_metros_c2 <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*metros cuadrados)")
data$numero_despues_metros_c2 <- str_extract(data$description, "(?<=metros cuadrados\\s)\\d{2,3}")


data$maximo_metros <- pmax(data$numero_antes_m2, data$numero_despues_m2, data$numero_antes_metros, data$numero_despues_metros,
                            data$numero_antes_mt, data$numero_despues_mt, data$numero_antes_mts , data$numero_despues_mts, 
                            data$numero_antes_mt2, data$numero_despues_mt2, data$numero_antes_mts2, data$numero_despues_mts2, data$numero_antes_metros_c2,
                            data$numero_despues_metros_c2, na.rm = TRUE)


data <- subset(data, select = -c(numero_antes_m2,numero_despues_m2, numero_antes_metros, numero_despues_metros, 
                                   numero_antes_mt, numero_despues_mt, numero_antes_mts, numero_despues_mts, numero_antes_mt2,
                                   numero_despues_mt2, numero_antes_mts2, numero_despues_mts2, numero_antes_metros_c2,  numero_despues_metros_c2))


#1.3.7 crear la variable número de garajes

data$numero_antes_garaje <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*garaje)")
data$numero_despues_garaje <- str_extract(data$description, "(?<=garaje\\s)\\d{2,3}")
data$numero_antes_garajes <- str_extract(data$description, "\\b\\d{2,3}(?=\\s*garajes)")
data$numero_despues_garajes <- str_extract(data$description, "(?<=garajes\\s)\\d{2,3}")

data$numero_garajes <- pmax(data$numero_antes_garaje, data$numero_despues_garaje, data$numero_antes_garajes, 
                             data$numero_despues_garajes, na.rm = TRUE)


data <- subset(data, select = -c(numero_antes_garaje, numero_despues_garaje, 
                                   numero_antes_garajes, numero_despues_garajes))


#1.3.8 crear las variables garaje, piscina, terraza, campestre

data$garaje <- grepl("garaje", data$description)
data$piscina <- grepl("piscina", data$description)
data$terraza <- grepl("terraza", data$description)
data$campestre <- grepl("campestre", data$description)


assign(obj, data)
rm(data)

}








