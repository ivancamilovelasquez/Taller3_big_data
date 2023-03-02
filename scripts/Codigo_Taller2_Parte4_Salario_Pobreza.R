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

p_load(caret, h2o, tidyverse)

train2 <- train 
test2 <- test 

trainIndex <- createDataPartition(train2$Pobre, p = 0.7)
trainIndex <- trainIndex$Resample1

ttrain <- train2[trainIndex, ]
ttest <- train2[-trainIndex, ]

## Modelos para Predecir los Salarios

# Modelo 1: Regresión Lineal 

cv <- trainControl(number = 5, method = "cv")
mod1 <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual, 
              data = ttrain, 
              method = "lm",
              trControl = cv
)
mod1

# Metricas modelo 1
p_load(MLmetrics)

y_hat_outsample1 = predict(mod1, newdata = ttest)

MAE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug)
MedianAE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug)
MedianAPE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug)
R2_Score(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug)
RMSE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug)

#Modelo 2: GBM 
grid_gbm<-expand.grid(n.trees=1000,interaction.depth=3,shrinkage=0.01,n.minobsinnode = 30)
mod2 <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual,
              data = ttrain, 
              method = "gbm", 
              trControl = cv,
              metric = "RSME",
              tuneGrid = grid_gbm
)

mod2

#Metricas modelo 2
y_hat_outsample2 = predict(mod2, newdata = ttest)

MAE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug)
MedianAE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug)
MedianAPE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug)
R2_Score(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug)
RMSE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug)



# Modelo 3: Arbol de decision 
mod3 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                media+superior+exp_trab_actual,
              data = ttrain, 
              method = "rpart", 
              trControl = cv)

library(rattle)
fancyRpartPlot(mod3$finalModel)

#Metricas modelo 3
y_hat_outsample3 = predict(mod3, newdata = ttest)

MAE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug)
MedianAE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug)
MedianAPE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug)
R2_Score(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug)
RMSE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug)

# Modelo 4: Random forest y una grilla para tunear 
tunegrid_rf <- expand.grid(mtry = 3, 
                           min.node.size = 100,
                           splitrule = "variance")

mod4 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                media+superior+exp_trab_actual,
              data = ttrain, 
              method = "ranger", 
              trControl = cv,
              metric = 'RMSE', 
              tuneGrid = tunegrid_rf)

mod4

#Metricas modelo 4
y_hat_outsample4 = predict(mod4, newdata = ttest)

MAE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
MedianAE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
MedianAPE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
R2_Score(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
RMSE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)


# Modelo 5: Regresion lineal (diferentes controles)
mod5 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                media+superior+exp_trab_actual+horas_trab_usual+busca_trabajo, 
              preProcess=NULL,
              data = train2, 
              method = "lm",
              trControl = cv,
              metric = 'RMSE')
mod5

#Metricas modelo 5
y_hat_outsample5 = predict(mod5, newdata = ttest)

MAE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug)
MedianAE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug)
MedianAPE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug)
R2_Score(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug)
RMSE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug)




#Tablas para comparar modelos outsample
ResultadosOutsample <- data.frame(Modelo=c("Regresión Linear 1","Regresión Linear 2","Random Forest GBM","Arbol de Decisión","Random Forest"), 
                         MAE = c(MAE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug),
                                 MAE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug),
                                 MAE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug),
                                 MAE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug),
                                 MAE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
                                 ),
                         MedianAE = c(MedianAE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug),
                                      MedianAE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug),
                                      MedianAE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug),
                                      MedianAE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug),
                                      MedianAE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
                         ),
                         MedianAPE = c(MedianAPE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug),
                                       MedianAPE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug),
                                       MedianAPE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug),
                                       MedianAPE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug),
                                       MedianAPE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
                         ),
                         R2_Score = c(R2_Score(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug),
                                       R2_Score(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug),
                                       R2_Score(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug),
                                       R2_Score(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug),
                                       R2_Score(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
                         ),
                         RMSE = c(RMSE(y_pred = y_hat_outsample1, y_true = ttest$Ingtotug),
                                       RMSE(y_pred = y_hat_outsample5, y_true = ttest$Ingtotug),
                                       RMSE(y_pred = y_hat_outsample2, y_true = ttest$Ingtotug),
                                       RMSE(y_pred = y_hat_outsample3, y_true = ttest$Ingtotug),
                                       RMSE(y_pred = y_hat_outsample4, y_true = ttest$Ingtotug)
                         )
)
ResultadosOutsample
##Basado en esto, escogeremos el modelo de Random Forest para predecir la pobreza

test2$Ingtotug_pred <- predict(mod4, newdata = test2)
ttrain$Ingtotug_pred <- predict(mod4, newdata = ttrain)
ttest$Ingtotug_pred <- predict(mod4, newdata = ttest)

