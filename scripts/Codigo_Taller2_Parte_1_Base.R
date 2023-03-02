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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx)

# 1. Importar los Datos de la Web

# 1.1 Definir Url

url_train_p <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle-v2/46984/5036080/compressed/train_personas.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1677290721&Signature=HITHF%2B3UbKAP1e8kgxE3W1aOhHsJWkO3G9OEIE4XJQn%2FvgoRWo0yjubjcbh1VokRtGJBwfEgpMgZs8XvCzLQ%2FM80zgwy%2BxlX8hQAmkqQ6G6FSQtlID3FXa8tAzrlLOmMLL05PKaSy93OdsLNcHsWFoMn2RRV9KwRLSx6JnG5ishNUZj7j9DbnWhldJO4h0KQJz%2BEHHYrY7enTnIDCuvqXwbmGsxbgzWYcPfiY30vfJRffwQq3XMBr6Vg1fb%2BHzC0TsV5wBX5KJh%2Ff1LshQmfYAd3kB5lQGaSdEpnP4G8zCIZ5718V755WNtvGGlIIGTdR6CuEYN%2F8qhL67a1rp3vJw%3D%3D&response-content-disposition=attachment%3B+filename%3Dtrain_personas.csv.zip"
url_train_h <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle-v2/46984/5036080/compressed/train_hogares.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1677290715&Signature=fHQsUVHrQEfMb%2FEE4eN8PYl3CK22nDkPRdtNiZ3QOE7pYRGK2ZIJR5ve7ZAlsupVaamX68poFFHTKQM4Nm1q47pD7Op1u8lJHFQowDaT8OKi5OZJxnWIe3FWRu7adgjRFF%2BlZLrsIuYN6A2sa40A5WJkoraWn3%2B5SOQlUsRV7%2FSLDKxfAQyJWJft63Sf0Y5Fjqu%2FVj0zSIWgHZxWtcud91btPKfHa1kJ94%2BMA3bwd9pE6OjsQ88PcnmlgsLo6vYnnFnjflsYfFGQkWU7cRszeDfMPKoDE75gPqk7YVBvE%2BuX%2BWi%2Ft6KCFQaYbniyr%2FjheMICaGh%2F5PxkOPDdCix6qA%3D%3D&response-content-disposition=attachment%3B+filename%3Dtrain_hogares.csv.zip"

url_test_p <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle-v2/46984/5036080/compressed/test_personas.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1677290712&Signature=SOnS0G9WMlLH%2BW%2FdHmugz%2BPwFq5KcbUll2TsI0Iq2gDqv5JGU87h0sm%2BjcPZfzRurSowbwASOoQ%2FxEVSBIVJ2BqnzqVbSYciMjHvBkVNhC3237OHV25MEg1QOW5snnkDvUOE4pSxEbVVDm2jJIOnWYuRiBZDFbpnv%2BBl%2FbVYcXn5PAoHhMlErtdy5z8zAJqxnG3LmZbzWt1N45yvlVUDL7C%2BZ8xzeLX8Jaj%2BsQUbuNJRigHp2DeciBhfIHMxAom3qFsyEAsQDEldaaFORARE54942Nk8qMZSEKi6eK0CAIK%2FGBRN3XFQWJB1kEcrAM4QGcUNuN9K%2B%2FOq5uZU3b1gGQ%3D%3D&response-content-disposition=attachment%3B+filename%3Dtest_personas.csv.zip"
url_test_h <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle-v2/46984/5036080/compressed/test_hogares.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1677290709&Signature=g8IhiAk4LxSKrpGpTxJMVeaW3WfBL7I1wib7vqOMqfZl3bxXTO3RJbnyrDI19xFZbvZfIGy61VOYA4et1RqtAkm5HwbgK1expVrJQZ6U8EQGFJkFD2d3wOjb35wUC2zNeTNAE8IYf0oiz6SeEgWH9RSSRkFRFeEK17dM5kpAkc%2Fk5k5iHiBYwBxbFLzlouat38w3WeIACyTND2NgTaFvFeqn6UTdAOfGOWwacO0H32z9ztkTEeRF1Y%2FVgzlStsIezPR9BwLqHPZaxEN5hTc7nfdbIn0kpSWJ1vFmtobpLH%2BKrkvynk6u8GPwPMr1ovvvy%2BJhrHqe2ToO7CH6O0VjEQ%3D%3D&response-content-disposition=attachment%3B+filename%3Dtest_hogares.csv.zip"

url_sample_h <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle-v2/46984/5036080/compressed/sample_submission.csv.zip?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1677290706&Signature=EMonKmzcGrf8xnnxEpjEdQriSCQhutG7z%2B9qX7qj%2BuTI1nGtXVKbky0lj14mlWrgc3%2B1UiUENI4%2FHYKRSZloq7jWc9XjVDaVkMGPzomm4Ifu5X8ErGgemu%2F2usX85m4GBV0NG7dU10kPd%2Fnxzml84luDdtRma2eauvpMhRLdyUpHD%2F7GTrybU53U9u4aufKao5DIKsxQGoHNX%2BeJjezvJNeguiK03PIK1tjPV2I7XjiX7R9nf%2BGnYycHuCLRFBwaiQJ5uuTsUt%2ByRSAt%2FzlWnIhNzE0%2B4XSZZFmbSIxTbjX8W%2BpIJD2zS6GWKDbNABxyW6UigqZGv3b9fBBMIb6N9w%3D%3D&response-content-disposition=attachment%3B+filename%3Dsample_submission.csv.zip"

# 1.2 Especificar Directorio

directorio_destino <- "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/Problem Sets"

# 1.3 Descargar Archivos de la web

archivo_comprimido1 <- file.path(directorio_destino, "train_personas.csv.zip")
archivo_comprimido2 <- file.path(directorio_destino, "train_hogares.csv.zip")
archivo_comprimido3 <- file.path(directorio_destino, "test_personas.csv.zip")
archivo_comprimido4 <- file.path(directorio_destino, "test_hogares.csv.zip")
archivo_comprimido5 <- file.path(directorio_destino, "sample_submission.csv.zip")

download.file(url_train_p, destfile = archivo_comprimido1, mode = "wb")
download.file(url_train_h, destfile = archivo_comprimido2, mode = "wb")
download.file(url_test_p, destfile = archivo_comprimido3, mode = "wb")
download.file(url_test_h, destfile = archivo_comprimido4, mode = "wb")
download.file(url_sample_h, destfile = archivo_comprimido5, mode = "wb")

# 1.4 Descomprimir el archivo

unzip(archivo_comprimido1, exdir = directorio_destino)
unzip(archivo_comprimido2, exdir = directorio_destino)
unzip(archivo_comprimido3, exdir = directorio_destino)
unzip(archivo_comprimido4, exdir = directorio_destino)
unzip(archivo_comprimido5, exdir = directorio_destino)

# 1.5 Importar el archivo 

archivo_descomprimido1 <- file.path(directorio_destino, "train_personas.csv")
archivo_descomprimido2 <- file.path(directorio_destino, "train_hogares.csv")
archivo_descomprimido3 <- file.path(directorio_destino, "test_personas.csv")
archivo_descomprimido4 <- file.path(directorio_destino, "test_hogares.csv")
archivo_descomprimido5 <- file.path(directorio_destino, "sample_submission.csv")

# 1.6 Crear los Data Frames

train_personas <- read.csv(archivo_descomprimido1)
train_hogares <- read.csv(archivo_descomprimido2)
test_personas <- read.csv(archivo_descomprimido3)
test_hogares <- read.csv(archivo_descomprimido4)
sample_submission <- read.csv(archivo_descomprimido5)

# 2. Unión de las bases 

train <- left_join(train_personas,train_hogares)
test <- left_join(test_personas,test_hogares)

# Crear variables faltantes 

test$Pobre <- NA
test$Ingtot <- NA
test$Ingtotug <- NA

# 3. Limpieza de la base 

objetos <- c("train", "test")

for (obj in objetos) {
  
  data <- get(obj)
  
  # - 3.1 Train
  
  # - Edad (Sólo mayores de 18 años)
  
  data <- rename(data, c("edad" = "P6040"))
  data$edad_2 <- data$edad^2
  
  # - Género
  
  data$mujer <- ifelse(data$P6020 == 2, 1, 0)
  data$mujer[data$P6020 == 1] <- 0
  
  # - Estudiante
  
  data$estudiante <- ifelse(data$P6240 == 3, 1, 0)
  data$estudiante[data$P6240 != 3] <- 0
  data$estudiante[data$P6240 == "."] <- 0
  data$estudiante[is.na(data$estudiante)] <- 0
  
  # - Busca trabajo
  
  data$busca_trabajo <- ifelse(data$P6240 == 2, 1, 0)
  data$busca_trabajo[data$P6240 != 2] <- 0
  data$busca_trabajo[data$P6240 == "."] <- 0
  data$busca_trabajo[is.na(data$busca_trabajo)] <- 0
  
  # - Amo(a) de casa
  
  data$amo_casa <- ifelse(data$P6240 == 4, 1, 0)
  data$amo_casa[data$P6240 != 4] <- 0
  data$amo_casa[data$P6240 == "."] <- 0
  data$amo_casa[is.na(data$amo_casa)] <- 0
  
  # - Hijos en el hogar
  
  data$hijos_hogar <- ifelse(data$P6050 == 3, 1, 0)
  data$hijos_hogar[data$P6050 != 3] <- 0
  data$hijos_hogar[data$P6050 == "."] <- 0
  data$hijos_hogar[is.na(data$hijos_hogar)] <- 0
  
  # - Primaria
  
  data$primaria <- ifelse(data$P6210 == 1, 1, 0)
  data$primaria[data$P6210 == "."] <- 0
  data$primaria[is.na(data$primaria)] <- 0
  
  # - Secundaria
  
  data$secundaria <- ifelse(data$P6210 == 4, 1, 0)
  data$secundaria[data$P6210 == "."] <- 0
  data$secundaria[is.na(data$secundaria)] <- 0
  
  # - Media
  
  data$media <- ifelse(data$P6210 == 5, 1, 0)
  data$media[data$P6210 == "."] <- 0
  data$media[is.na(data$media)] <- 0
  
  # - Superior
  
  data$superior <- ifelse(data$P6210 == 6, 1, 0)
  data$superior[data$P6210 == "."] <- 0
  data$superior[is.na(data$superior)] <- 0
  
  
  # - Experiencia trabajo actual
  
  data <- rename(data, c("exp_trab_actual" = "P6426"))
  
  # - Horas de trabajo a la semana
  
  data <- rename(data, c("horas_trab_usual" = "P6800"))
  
  
  # - Ciudad
  
  data <- rename(data, c("ciudad" = "Dominio"))
  
  # - Número de Cuartos
  
  data <- rename(data, c("numero_cuartos" = "P5010"))
  
  # - Número de personas
  
  data <- rename(data, c("numero_personas" = "Nper"))
  
  # - Imputación de experiencia
  
  data$exp_trab_actual <- ifelse(data$edad < 18 & 
                                   is.na(data$exp_trab_actual), 0, 
                                 data$exp_trab_actual)
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(mean_exp = mean(exp_trab_actual, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual) & data$edad >= 18, 
                                     mean_exp, data$exp_trab_actual))
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(variable = ifelse(all(is.na(exp_trab_actual)), 0, 
                             exp_trab_actual)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual), 
                                     variable, data$exp_trab_actual))
  
  # Imputación Horas 
  
  data$horas_trab_usual <- ifelse(data$edad < 18 & 
                                    is.na(data$horas_trab_usual), 0, 
                                  data$horas_trab_usual)
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(mean_h = mean(horas_trab_usual, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(horas_trab_usual = if_else(is.na(horas_trab_usual) & data$edad >= 18, 
                                      mean_h, data$horas_trab_usual))
  
  data <- data %>% 
    group_by(id) %>% 
    mutate(variable = ifelse(all(is.na(horas_trab_usual)), 0, 
                             horas_trab_usual)) %>% 
    ungroup() %>% 
    mutate(horas_trab_usual = if_else(is.na(horas_trab_usual), 
                                      variable, data$horas_trab_usual))
  
  
  
  data <- subset(data, select = c("id", "Orden", "Clase",
                                  "ciudad", "edad", "edad_2", "mujer", 
                                  "estudiante", "busca_trabajo", "amo_casa",
                                  "hijos_hogar", "primaria", "secundaria",
                                  "media", "superior", "Ingtot",
                                  "Ingtotug", "exp_trab_actual",
                                  "horas_trab_usual", "Pobre", "numero_personas"))
  
  data$num_menores <- as.numeric(data$edad < 18)
  
  # Pasar la base únicamente a hogares
  
  data <- data %>% group_by(id) %>%
    summarize(edad = mean(edad),
              edad_2 = mean(edad_2),
              mujer = mean(mujer),
              estudiante = mean(estudiante),
              busca_trabajo = mean(busca_trabajo),
              amo_casa =mean(amo_casa),
              hijos_hogar = mean(amo_casa),
              primaria = mean(primaria),
              secundaria = mean(secundaria),
              media = mean(media),
              superior = mean(superior),
              Ingtot = sum(Ingtot),
              Ingtotug = mean(Ingtotug),
              exp_trab_actual = mean(exp_trab_actual),
              horas_trab_usual = mean(horas_trab_usual),
              Pobre = mean(Pobre),
              numero_personas = sum(numero_personas),
              num_menores = sum(num_menores),
              ciudad = first(ciudad))
  
  assign(obj, data)
  rm(data)
  
}
