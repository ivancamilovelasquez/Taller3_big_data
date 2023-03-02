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


# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, caret, boot, openxlsx, rio, rpart)
p_load(rpart.plot, Metrics, AER, rattle, ipred, randomForest, ipred, glmnet)


# Fijar una semilla 
set.seed(10119)


# Dividir los datos de train 

index <- createDataPartition(train$Pobre, p = 0.7, list = FALSE)
train_train <- train[index, ]
train_test <- train[-index, ]



# Modelo 1 : Arbol simple 

m1p1 <- rpart(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
              amo_casa + hijos_hogar + primaria + secundaria + media +
              superior + exp_trab_actual + horas_trab_usual, 
              data    = train_train,
              method = "class")

predictions <- predict(m1p1, newdata = train_test, type = "class")
correct_predictions <- sum(predictions == train_test$Pobre)
accuracy_1 <- correct_predictions / nrow(train_test)
print(accuracy_1)
# Accuracy de 0.82




# Modelo 2 : Arbol  bagging

m2p2 <- bagging(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                      amo_casa + hijos_hogar + primaria + secundaria + media +
                      superior + exp_trab_actual + horas_trab_usual,
                      data  = train_train, nbagg = 500)

m2p2 <- predict(m2p2,newdata = train_test, type="class")

predictions_2 <- predict(m1p1, newdata = train_test, type = "class")
correct_predictions <- sum(predictions_2 == train_test$Pobre)
accuracy_2 <- correct_predictions / nrow(train_test)
print(accuracy_2)
# Acurracy de 0.831




# Modelo 3 : Random Forest 

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE)

m3p3 <- train(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                    amo_casa + hijos_hogar + primaria + secundaria + media +
                    superior + exp_trab_actual + horas_trab_usual, 
                  data = train_train, method = "rf", trControl = ctrl,
                  tuneGrid = expand.grid(mtry = 1:ncol(train_train)))

predictions <- predict(m3p3, newdata = train_test)
correct_predictions <- sum(predictions == train_test$Pobre)
accuracy_3 <- correct_predictions / nrow(train_test)
print(accuracy_3)



# Modelo 4:  Random Forest con  expand.grid

train_train <- train_train %>% 
  mutate_all(funs(factor))
train_train <- train_train %>%
  mutate_all(funs(make.names(as.character(.))))

tunegrid_rf <- expand.grid(mtry = c(3, 5, 8), 
                           min.node.size = c(500, 1000,20000, 30000),
                           splitrule = "gini")

m4p4 <- train(Pobre ~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                amo_casa + hijos_hogar + primaria + secundaria + media +
                superior + exp_trab_actual + horas_trab_usual,
              data = train_train,
              method = "rf", 
              trControl = trainControl(method = "cv", number = 10, classProbs = TRUE),
              tuneGrid = tunegrid_rf,
              metric = 'Accuracy')


## Modelos lineales 

# Logit

m1_log1 <- glm(Pobre ~ edad + edad_2 + mujer + estudiante + busca_trabajo +
               amo_casa + hijos_hogar + primaria + secundaria + media +
               superior + exp_trab_actual + horas_trab_usual,
               data= train_train,
               family=binomial(link="logit"))

train_train$y_hat_1 <- predict(m1_log1, newdata=train_train , type="response")
rule=0.5
train_test$y_hat_1 <- predict(m1_log1, newdata=train_test , type="response")
train_test$pobre_prob1 = ifelse(train_test$y_hat_1>rule,1,0)
accuracy_logit1 <- mean(train_test$pobre_prob1 == train_test$Pobre)
accuracy_logit1

#Predicciones de la Pobreza desde el Random Forest

test2$Ingtotug_pred <- predict(mod4, newdata = test2)
ttrain$Ingtotug_pred <- predict(mod4, newdata = ttrain)
ttest$Ingtotug_pred <- predict(mod4, newdata = ttest)

#Modelo 1: Logit 
p_load(MLmetrics)

pob1 <- train(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                 primaria+secundaria+media+superior+exp_trab_actual,
               data = ttrain,
               method = "glmnet",
               family = "binomial")
pob1

y_hat_outsample_p1 = predict(pob1, newdata = ttest)

Recall(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre)
F1_Score(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre)
Precision(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre)


#Modelo 2: Upsample Lasso
# UP-Sampling
glimpse(ttrain)
ttrain$Ingtotug_pred <- predict(mod4, newdata = ttrain)
set.seed(2905)
ttrain$Pobre <- as.factor(ttrain$Pobre)
class(ttrain$Pobre)
upSampledTrain <- upSample(x = ttrain,
                           y = ttrain$Pobre,
                           yname = "Pobre")
dim(ttrain)

dim(upSampledTrain)

table(upSampledTrain$Pobre) 

lambda_grid <- 10^seq(-4, 0.01, length = 10)

pob2 <- train(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                                  primaria+secundaria+media+superior+exp_trab_actual, 
                                data = upSampledTrain, 
                                method = "glmnet",
                                trControl = cv,
                                family = "binomial", 
                                metric = "Accuracy",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                                preProcess = c("center", "scale")
)
pob2

y_hat_outsample_p2 = predict(pob2, newdata = ttest)

Recall(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre)
F1_Score(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre)
Precision(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre)

#Modelo 3: Logit Caret

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = F,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(2905)
pob3 <- train(as.factor(Pobre)~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                primaria+secundaria+media+superior+exp_trab_actual, 
              data = ttrain, 
              method = "glm",
              trControl = ctrl_def,
              family = "binomial", 
              preProcess =  c("center", "scale"))

pob3

y_hat_outsample_p3 = predict(pob3, newdata = ttest)

Recall(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre)
F1_Score(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre)
Precision(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre)


#Modelo 4: Logit Ridge
lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300

set.seed(2905)
pob4 <- train(as.factor(Pobre)~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                             primaria+secundaria+media+superior+exp_trab_actual, 
                           data = ttrain, 
                           method = "glmnet",
                           trControl = ctrl,
                           family = "binomial", 
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                           preProcess = c("center", "scale")
)
pob4

y_hat_outsample_p4 = predict(pob4, newdata = ttest)

Recall(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre)
F1_Score(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre)
Precision(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre)

#Modelo 5: Logit Lasso Smote
p_load("smotefamily")
glimpse(train2)

predictors<-c("edad","edad_2","mujer","estudiante","busca_trabajo",
              "amo_casa","hijos_hogar","primaria","secundaria","media",
              "superior", "exp_trab_actual","horas_trab_usual",
              "Ingtotug_pred")
head(ttrain[predictors])

smote_output = SMOTE(X = ttrain[predictors],
                     target = ttrain$Pobre)
smote_data = smote_output$data
table(ttrain$Pobre)
table(smote_data$class)

set.seed(2905)
pob5<- train(class~edad+edad_2+mujer+estudiante+busca_trabajo+
               amo_casa+hijos_hogar+primaria+secundaria+media+
               superior+exp_trab_actual+horas_trab_usual
             +Ingtotug_pred,
             data = smote_data, 
             method = "glmnet",
             trControl = ctrl,
             family = "binomial", 
             metric = "Accuracy",
             tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid), 
             preProcess = c("center", "scale")
)
pob5

y_hat_outsample_p5 = predict(pob5, newdata = ttest)

Recall(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre)
F1_Score(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre)
Precision(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre)


#Modelo 6: LDA
pob6 = train(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                  primaria+secundaria+media+superior+exp_trab_actual, 
                data=ttrain, 
                method="lda",
                trControl = ctrl,
                metric = 'Accuracy')

pob6

y_hat_outsample_p6 = predict(pob6, newdata = ttest)

Recall(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
Accuracy(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
R2_Score(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
RMSE(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)


#Tablas para comparar modelos outsample
ResultadosOutsampleP <- data.frame(Modelo=c("Logit","Logit Lasso Upsampling","Logit Caret","Logit Ridge","SMOTE","LDA"), 
                                  Recall = c(Recall(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre),
                                             Recall(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre),
                                             Recall(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre),
                                             Recall(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre),
                                             Recall(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre),
                                             Recall(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
                                  ),
                                  Accuracy = c(Accuracy(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre),
                                               Accuracy(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre),
                                               Accuracy(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre),
                                               Accuracy(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre),
                                               Accuracy(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre),
                                               Accuracy(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
                                  ),
                                  F1_Score = c( F1_Score(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre),
                                                F1_Score(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre),
                                                F1_Score(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre),
                                                F1_Score(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre),
                                                F1_Score(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre),
                                                F1_Score(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
                                  ),
                                  Precision = c(Precision(y_pred = y_hat_outsample_p1, y_true = ttest$Pobre),
                                                Precision(y_pred = y_hat_outsample_p2, y_true = ttest$Pobre),
                                                Precision(y_pred = y_hat_outsample_p3, y_true = ttest$Pobre),
                                                Precision(y_pred = y_hat_outsample_p4, y_true = ttest$Pobre),
                                                Precision(y_pred = y_hat_outsample_p5, y_true = ttest$Pobre),
                                                Precision(y_pred = y_hat_outsample_p6, y_true = ttest$Pobre)
                                  )
)

ResultadosOutsampleP

#Basados en esto, el mejor modelo será el de Logit Caret

test2$Pobre_pred <- predict(pob3, newdata = test2)
predictTest_mod3 <- data.frame(
  id = test2$id,
  pobre = test2$Pobre_pred    
)
