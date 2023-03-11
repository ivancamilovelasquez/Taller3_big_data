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

# Ver cuantos datso faltantes hay en la variable de superficie 
filtro <- is.na(train_final$surface_covered)
sum(filtro)
filtro2 <- is.na(train_final$surface_total)
sum(filtro2)


#Partir la base de Train en 2: t_train y t_test ----
p_load(caret)
set.seed(1234)

inTrain <- createDataPartition(
  y = train_final$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


t_train <- train_final[ inTrain,]
t_test  <- train_final[-inTrain,]

filtro <- is.na(t_train$surface_covered)
sum(filtro)
filtro2 <- is.na(t_train$surface_total)
sum(filtro2)

# Hay muchos datos faltantes de estas variables claves
# Grafico de distribucion de las areas de las propiedades 

p <- ggplot(train_final, aes(x = area_maxima)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Metros cuadrados", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

ggplotly(p)

# Eslacar los datos----
variables_numericas <- c("dist_min_train_parque", "dist_min_train_fitness", "area_parque", 
                         "dist_hospital_centre_train","dist_busstation_centre_train", "dist_police_centre_train", 
                         "dist_school_centre_train", "dist_restaurante_centre_train")

escalador <- preProcess(train_final[, variables_numericas],
                        method = c("center", "scale"))

train_final[, variables_numericas] <- predict(escalador, train_final[, variables_numericas])
test_final[, variables_numericas] <- predict(escalador, test_final[, variables_numericas])


escalador1 <- preProcess(t_train[, variables_numericas],
                        method = c("center", "scale"))

t_train[, variables_numericas] <- predict(escalador, t_train[, variables_numericas])
t_test[, variables_numericas] <- predict(escalador, t_test[, variables_numericas])


# train_final$a <- ifelse(train_final$surface_covered != 0, 1, 0)
# 
# prop.table(table(train_final$a, exclude = NULL))
# 
# train_final <- train_final[,-41]

p_load(MLmetrics)

#Modelo 1: Regresión lineal ----
cv1 <- trainControl(number = 5, method = "cv")

mod1 <- train(price ~ bedrooms + garaje + as.factor(year) + casa  + dist_min_train_parque + dist_min_train_fitness 
              + area_parque + dist_hospital_centre_train + dist_busstation_centre_train, 
              data = t_train, 
              method = "lm",
              trControl = cv1
)
mod1

## Métricas modelo 1
y_hat_outsample1 = predict(mod1, newdata = t_test)
MAE(y_pred = y_hat_outsample1, y_true = t_test$price)
MAPE(y_pred = y_hat_outsample1, y_true = t_test$price)
RMSE(y_pred = y_hat_outsample1, y_true = t_test$price)

##Modelo 2: Arbol de decision ----
cv <- trainControl(number = 10, method = "cv")
# especificamos la grilla de los alphas
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

mod2 <- train(price~as.factor(year) + bedrooms + as.factor(casa)  + dist_min_train_parque + dist_min_train_fitness + area_parque + 
               dist_hospital_centre_train + dist_busstation_centre_train + dist_police_centre_train + 
               campestre + piscina + terraza + dist_school_centre_train + dist_restaurante_centre_train,
              data = t_train, 
              method = "rpart", 
              trControl = cv,
              tuneGrid = grid)
mod2

## Métricas modelo 2
y_hat_outsample2 = predict(mod2, newdata = t_test)
MAE(y_pred = y_hat_outsample2, y_true = t_test$price)
MAPE(y_pred = y_hat_outsample2, y_true = t_test$price)
RMSE(y_pred = y_hat_outsample2, y_true = t_test$price)

#Modelo 3: GBM ----
grid_gbm<-expand.grid(n.trees=1000,interaction.depth=5,shrinkage=0.01,n.minobsinnode = 20)
mod3 <- train(price ~ bedrooms + as.factor(casa)  + dist_min_train_parque + dist_min_train_fitness + area_parque + 
                dist_hospital_centre_train + dist_busstation_centre_train + dist_police_centre_train + garaje +
                campestre + piscina + terraza + dist_school_centre_train + dist_restaurante_centre_train,
              data = t_train, 
              method = "gbm", 
              trControl = cv1,
              metric = "RSME",
              tuneGrid = grid_gbm
)

mod3

## Métricas modelo 3
y_hat_outsample3 = predict(mod3, newdata = t_test)
MAE(y_pred = y_hat_outsample3, y_true = t_test$price)
MAPE(y_pred = y_hat_outsample3, y_true = t_test$price)
RMSE(y_pred = y_hat_outsample3, y_true = t_test$price)

#Modelo 4: Ramdon Forest
cv <- trainControl(method = "cv",number = 10,search = "grid")
tunegrid_rf <- expand.grid(mtry = 5, 
                           min.node.size = 10,
                           splitrule = "variance")

mod4 <- train(price ~ bedrooms + as.factor(casa)  + dist_min_train_parque + dist_min_train_fitness + area_parque + 
                dist_hospital_centre_train + dist_busstation_centre_train + dist_police_centre_train + garaje +
                campestre + piscina + terraza + dist_school_centre_train + dist_restaurante_centre_train +
                dist_cinema_centre_train + dist_pub_centre_train + dist_min_train_playground,
              data = t_train, 
              method = "ranger", 
              trControl = cv,
              metric = 'MAE', 
              tuneGrid = tunegrid_rf)

mod4
y_hat_outsample4 = predict(mod4, newdata = t_test)
MAE(y_pred = y_hat_outsample4, y_true = t_test$price)
MAPE(y_pred = y_hat_outsample4, y_true = t_test$price)
RMSE(y_pred = y_hat_outsample4, y_true = t_test$price)




# El mejor modelo es ...

modelo_final = mod4
y_predict <- predict(modelo_final, newdata = test_final)

predictmod1 <- data.frame(
  property_id = test_final$property_id,
  price = y_predict     
)
predictmod1
write.csv(predictmod1, "~/MAESTRIA/Taller 3 Big Data/predictmod1.csv", row.names =  FALSE)
