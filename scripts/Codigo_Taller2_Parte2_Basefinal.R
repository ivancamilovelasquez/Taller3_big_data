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
# 
#  Código : Limpieza de datos - Base final 



# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx)



# 1. Unión de las bases 

train <- left_join(train_personas,train_hogares)
test <- left_join(test_personas,test_hogares)

# Crear variables faltantes 

test$Pobre <- NA
test$Ingtot <- NA
test$Ingtotug <- NA

# 2. Limpieza de la base 

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