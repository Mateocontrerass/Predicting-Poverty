
rm(list = ls())
cat("\f")

require(pacman)
p_load(tidyverse,dplyr,here,skimr,tidyr,gamlr,modelsummary,caret,
       rio,knitr, kableExtra, rstudioapi,tidymodels,janitor,MLmetrics,
       rattle,doParallel, ROCR, themis, install = TRUE)

## Carlos

#-------------------------------------------------------------------------------

### Logit - 

#-------------------------------------------------------------------------------

### train

## Depurar base

set.seed(666)

training <- data_rf_train
rm(data_rf_train)

colnames(training)

sub_set_training <- subset (training, select = -c(id, Li, Lp, ing, Npersug, P5010, P6050_3,
                                                  P6050_4, P6050_5, P6050_6, P6050_7, 
                                                  P6050_8, P6050_9, P6426, P5130,
                                                  P6210s1_0, P6210s1_1, P6210s1_10, P6210s1_11,
                                                  P6210s1_12, P6210s1_13, P6210s1_14, P6210s1_15,
                                                  P6210s1_2, P6210s1_4, P6210s1_5, P6210s1_6, P6210s1_7,
                                                  P6210s1_8, P6210s1_9, P6210s1_99, P7505_1))

training <- subset(training, select = -c(id, Li, Lp, ing))


## Modelo

logit_training <- glm(Pobre_1 ~.,
                      data = training, family = binomial(link="logit")) ## Esimar modelo logit

summary(logit_training)

Pobre_1_logit_training <- predict(logit_training,
                                  newdata = training,
                                  type = "response") # Estimación de predicciones de Pobre_1za training

colnames(training)

ggplot(data=training , mapping=aes(Pobre_1,Pobre_1_logit_training)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## se observa que 0.35 puede servir para evitar subestimar Pobre_1za


## se generan 2 reglas, la de bayes (p=0.5), y una regla basada en la distribución de las predicciones del modelo (p=0.35)

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat1_training <- ifelse(Pobre_1_logit_training>regla1,1,0) ## Prediccion de Pobre_1za


ggplot(training, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(training, aes(x = Pobre_1_hat1_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r1 <- confusionMatrix(data = factor(Pobre_1_hat1_training),
                                        reference = factor(training$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r1 <- cm_logit_training_r1$table

skim(training$Pobre_1)


acc_training_r1 <- Accuracy(y_pred = Pobre_1_hat1_training, y_true = training$Pobre_1)
pre_training_r1 <- Precision(y_pred = Pobre_1_hat1_training, y_true = training$Pobre_1, positive = 1)
rec_training_r1 <- Recall(y_pred = Pobre_1_hat1_training, y_true = training$Pobre_1, positive = 1)
f1_training_r1 <- F1_Score(y_pred = Pobre_1_hat1_training, y_true = training$Pobre_1, positive = 1)
spf_training_r1 <- Specificity(y_pred = Pobre_1_hat1_training, y_true = training$Pobre_1, positive = 1)
FPR_training_r1 <- cm_logit_training_r1[2,1]/sum(cm_logit_training_r1[2,])
FNR_training_r1 <- cm_logit_training_r1[1,2]/sum(cm_logit_training_r1[1,])

metricas_training_r1 <- data.frame(Modelo = "Logit - Regla de Bayes",
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Dentro de muestra",
                                  "Accuracy" = acc_training_r1,
                                  "Precision" = pre_training_r1,
                                  "Recall" = rec_training_r1,
                                  "F1" = f1_training_r1,
                                  "Specificity" = spf_training_r1,
                                  "FPR" = FPR_training_r1,
                                  "FNR" = FNR_training_r1)

#Regla 2
regla2 <- 0.35 # Se define regla de predicción 

Pobre_1_hat2_training <- ifelse(Pobre_1_logit_training>regla2,1,0) ## Prediccion de Pobre_1za


ggplot(training, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(training, aes(x = Pobre_1_hat2_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r2 <- confusionMatrix(data = factor(Pobre_1_hat2_training),
                                        reference = factor(training$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r2 <- cm_logit_training_r2$table

skim(training$Pobre_1)


acc_training_r2 <- Accuracy(y_pred = Pobre_1_hat2_training, y_true = training$Pobre_1)
pre_training_r2 <- Precision(y_pred = Pobre_1_hat2_training, y_true = training$Pobre_1, positive = 1)
rec_training_r2 <- Recall(y_pred = Pobre_1_hat2_training, y_true = training$Pobre_1, positive = 1)
f1_training_r2 <- F1_Score(y_pred = Pobre_1_hat2_training, y_true = training$Pobre_1, positive = 1)
spf_training_r2 <- Specificity(y_pred = Pobre_1_hat2_training, y_true = training$Pobre_1, positive = 1)
FPR_training_r2 <- cm_logit_training_r2[2,1]/sum(cm_logit_training_r2[2,])
FNR_training_r2 <- cm_logit_training_r2[1,2]/sum(cm_logit_training_r2[1,])

metricas_training_r2 <- data.frame(Modelo = "Logit - Gráfica",
                                   "Muestreo" = NA, 
                                   "Evaluación" = "Dentro de muestra",
                                   "Accuracy" = acc_training_r2,
                                   "Precision" = pre_training_r2,
                                   "Recall" = rec_training_r2,
                                   "F1" = f1_training_r2,
                                   "Specificity" = spf_training_r2,
                                   "FPR" = FPR_training_r2,
                                   "FNR" = FNR_training_r2)


## Curva de ROC

library("ROCR")

predicciones_r1 <- prediction(Pobre_1_hat1_training, training$Pobre_1)
ROC_r1 <- performance(predicciones_r1,"tpr","fpr")
plot(ROC_r1, main = "ROC curve", col="red")
abline(a = 0, b = 1)


predicciones_r2 <- prediction(Pobre_1_hat2_training, training$Pobre_1)
ROC_r2 <- performance(predicciones_r2,"tpr","fpr")
plot(ROC_r1, main = "ROC curve", col="red")
plot(ROC_r2, main = "ROC curve", col="blue" , add=T)
abline(a = 0, b = 1)

auc_ROC_r1 <- performance(predicciones_r1, measure = "auc")
auc_ROC_r1@y.values[[1]]

auc_ROC_r2 <- performance(predicciones_r2, measure = "auc")
auc_ROC_r2@y.values[[1]]



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Evauluating

skim(evaluating)

colnames(evaluating)

sub_set_evaluating <- subset (evaluating, select = -c(id, Li, Lp, ing, Npersug, P5010, P6050_3,
                                                      P6050_4, P6050_5, P6050_6, P6050_7, 
                                                      P6050_8, P6050_9, P6426, P5130,
                                                      P6210s1_0, P6210s1_1, P6210s1_10, P6210s1_11,
                                                      P6210s1_12, P6210s1_13, P6210s1_14, P6210s1_15,
                                                      P6210s1_2, P6210s1_4, P6210s1_5, P6210s1_6, P6210s1_7,
                                                      P6210s1_8, P6210s1_9, P6210s1_99, P7505_1))

evaluating <- subset(evaluating, select = -c(id, Li, Lp, ing))

colnames(evaluating)


logit_evaluate <- glm(Pobre_1 ~.,
                      data = evaluating, family = binomial(link="logit")) ## Estimar modelo logit en evaluate

tidy(logit_evaluate)
summary(logit_evaluate)

Pobre_1_logit_evaluating <- predict(logit_evaluate,
                                    newdata = evaluating,
                                    type = "response") # Estimación de predicciones de Pobre_1za
summary(Pobre_1_logit_evaluating)

ggplot(data=evaluating , mapping=aes(Pobre_1,Pobre_1_logit_evaluating)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## parece que 0.35 es el valor para evitar subestimar a los Pobre_1s


## Se prueban las mismas 2 reglas usadas en la muestra training

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat1_evaluating <- ifelse(Pobre_1_logit_evaluating>regla1,1,0) ## Prediccion de Pobre_1za


ggplot(evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(evaluating, aes(x = Pobre_1_hat1_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r1 <- confusionMatrix(data = factor(Pobre_1_hat1_evaluating),
                                        reference = factor(evaluating$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_evaluating_r1 <- cm_logit_evaluating_r1$table

skim(evaluating$Pobre_1)


acc_evaluating_r1 <- Accuracy(y_pred = Pobre_1_hat1_evaluating, y_true = evaluating$Pobre_1)
pre_evaluating_r1 <- Precision(y_pred = Pobre_1_hat1_evaluating, y_true = evaluating$Pobre_1, positive = 1)
rec_evaluating_r1 <- Recall(y_pred = Pobre_1_hat1_evaluating, y_true = evaluating$Pobre_1, positive = 1)
f1_evaluating_r1 <- F1_Score(y_pred = Pobre_1_hat1_evaluating, y_true = evaluating$Pobre_1, positive = 1)
spf_evaluating_r1 <- Specificity(y_pred = Pobre_1_hat1_evaluating, y_true = evaluating$Pobre_1, positive = 1)
FPR_evaluating_r1 <- cm_logit_evaluating_r1[2,1]/sum(cm_logit_evaluating_r1[2,])
FNR_evaluating_r1 <- cm_logit_evaluating_r1[1,2]/sum(cm_logit_evaluating_r1[1,])

metricas_evaluating_r1 <- data.frame(Modelo = "Logit - Regla de Bayes",
                                   "Muestreo" = NA, 
                                   "Evaluación" = "Fuera de muestra",
                                   "Accuracy" = acc_evaluating_r1,
                                   "Precision" = pre_evaluating_r1,
                                   "Recall" = rec_evaluating_r1,
                                   "F1" = f1_evaluating_r1,
                                   "Specificity" = spf_evaluating_r1,
                                   "FPR" = FPR_evaluating_r1,
                                   "FNR" = FNR_evaluating_r1)

#Regla 2
regla2 <- 0.35 # Se define regla de predicción 

Pobre_1_hat2_evaluating <- ifelse(Pobre_1_logit_evaluating>regla2,1,0) ## Prediccion de Pobre_1za


ggplot(evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(evaluating, aes(x = Pobre_1_hat2_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r2 <- confusionMatrix(data = factor(Pobre_1_hat2_evaluating),
                                          reference = factor(evaluating$Pobre_1),
                                          mode = "sens_spec", positive = "1")

cm_logit_evaluating_r2 <- cm_logit_evaluating_r2$table

skim(evaluating$Pobre_1)


acc_evaluating_r2 <- Accuracy(y_pred = Pobre_1_hat2_evaluating, y_true = evaluating$Pobre_1)
pre_evaluating_r2 <- Precision(y_pred = Pobre_1_hat2_evaluating, y_true = evaluating$Pobre_1, positive = 1)
rec_evaluating_r2 <- Recall(y_pred = Pobre_1_hat2_evaluating, y_true = evaluating$Pobre_1, positive = 1)
f1_evaluating_r2 <- F1_Score(y_pred = Pobre_1_hat2_evaluating, y_true = evaluating$Pobre_1, positive = 1)
spf_evaluating_r2 <- Specificity(y_pred = Pobre_1_hat2_evaluating, y_true = evaluating$Pobre_1, positive = 1)
FPR_evaluating_r2 <- cm_logit_evaluating_r2[2,1]/sum(cm_logit_evaluating_r2[2,])
FNR_evaluating_r2 <- cm_logit_evaluating_r2[1,2]/sum(cm_logit_evaluating_r2[1,])

metricas_evaluating_r2 <- data.frame(Modelo = "Logit - Gráfica",
                                   "Muestreo" = NA, 
                                   "Evaluación" = "Fuera de muestra",
                                   "Accuracy" = acc_evaluating_r2,
                                   "Precision" = pre_evaluating_r2,
                                   "Recall" = rec_evaluating_r2,
                                   "F1" = f1_evaluating_r2,
                                   "Specificity" = spf_evaluating_r2,
                                   "FPR" = FPR_evaluating_r2,
                                   "FNR" = FNR_evaluating_r2)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

### Metodos para solución de imbalance

## Remuestreo - Oversampling

library(themis)

prop.table(table(training$Pobre_1 <- factor(training$Pobre_1))) ## Particios 75/25

train_oversampling <- recipe(Pobre_1 ~ ., data = training) %>%
  themis::step_smote(Pobre_1, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_oversampling$Pobre_1))

a <- nrow(training)
b <- nrow(train_oversampling)
b-a ## se crearon 188665 obsesrvaciones nuevas


colnames(train_oversampling)

logit_oversampling <- glm(Pobre_1 ~.,
                          data = train_oversampling, family = binomial(link="logit")) ## Estimar modelo logit en evaluate

tidy(logit_oversampling)
summary(logit_oversampling)

Pobre_1_logit_oversampling <- predict(logit_oversampling,
                                      newdata = train_oversampling,
                                      type = "response") # Estimación de predicciones de Pobre_1za
summary(Pobre_1_logit_oversampling)

ggplot(data=train_oversampling , mapping=aes(Pobre_1,Pobre_1_logit_oversampling)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## la regra de bayes parece funcionar para predecir pobreza


regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat_oversamping <- ifelse(Pobre_1_logit_oversampling>regla1,1,0) ## Prediccion de Pobreza


ggplot(train_oversampling, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(train_oversampling, aes(x = Pobre_1_hat_oversamping)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_oversampling <- confusionMatrix(data = factor(Pobre_1_hat_oversamping),
                                         reference = factor(train_oversampling$Pobre_1),
                                         mode = "sens_spec", positive = "1")

cm_logit_oversampling <- cm_logit_oversampling$table

skim(train_oversampling$Pobre_1)


acc_oversampling <- Accuracy(y_pred = Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1)
pre_oversampling <- Precision(y_pred = Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
rec_oversampling <- Recall(y_pred = Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
f1_oversampling <- F1_Score(y_pred = Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
spf_oversampling <- Specificity(y_pred = Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
FPR_oversampling <- cm_logit_oversampling[2,1]/sum(cm_logit_oversampling[2,])
FNR_oversampling <- cm_logit_oversampling[1,2]/sum(cm_logit_oversampling[1,])

metricas_oversampling <- data.frame(Modelo = "Logit - correccion de imbalance",
                                     "Muestreo" = "Oversampling", 
                                     "Evaluación" = "Dentro de muestra",
                                     "Accuracy" = acc_oversampling,
                                     "Precision" = pre_oversampling,
                                     "Recall" = rec_oversampling,
                                     "F1" = f1_oversampling,
                                     "Specificity" = spf_oversampling,
                                     "FPR" = FPR_oversampling,
                                     "FNR" = FNR_oversampling)

## Curva de ROC

predicciones_oversampling <- prediction(Pobre_1_hat_oversamping, train_oversampling$Pobre_1)
ROC_oversampling <- performance(predicciones_oversampling,"tpr","fpr")
plot(ROC_oversampling, main = "ROC curve", col="red")
abline(a = 0, b = 1)

auc_ROC_r2 <- performance(predicciones_r2, measure = "auc")
auc_ROC_r2@y.values[[1]]



### Comparación con muestra evaluate


Pobre_1_logit_oversampling_evaluate <- predict(logit_oversampling,
                                               newdata = evaluating,
                                               type = "response") # Estimación de predicciones de Pobre_1za

regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat_oversamping_evaluate <- ifelse(Pobre_1_logit_oversampling_evaluate>regla1,1,0) ## Prediccion de Pobre_1za

cm_logit_oversampling_evaluate <- confusionMatrix(data = factor(Pobre_1_hat_oversamping_evaluate),
                                                  reference = factor(evaluating$Pobre_1),
                                                  mode = "sens_spec", positive = "1")

cm_logit_oversampling_evaluate <- cm_logit_oversampling_evaluate$table

skim(evaluating$Pobre_1)


acc_oversampling_evaluate <- Accuracy(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1)
pre_oversampling_evaluate <- Precision(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
rec_oversampling_evaluate <- Recall(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
f1_oversampling_evaluate <- F1_Score(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
spf_oversampling_evaluate <- Specificity(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
FPR_oversampling_evaluate <- cm_logit_oversampling_evaluate[2,1]/sum(cm_logit_oversampling_evaluate[2,])
FNR_oversampling_evaluate <- cm_logit_oversampling_evaluate[1,2]/sum(cm_logit_oversampling_evaluate[1,])

metricas_oversampling_evaluate <- data.frame(Modelo = "Logit - correcion imbalance",
                                            "Muestreo" = "Oversampling", 
                                            "Evaluación" = "Fuera de muestra",
                                            "Accuracy" = acc_oversampling_evaluate,
                                            "Precision" = pre_oversampling_evaluate,
                                            "Recall" = rec_oversampling_evaluate,
                                            "F1" = f1_oversampling_evaluate,
                                            "Specificity" = spf_oversampling_evaluate,
                                            "FPR" = FPR_oversampling_evaluate,
                                            "FNR" = FNR_oversampling_evaluate)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Remuestreo - Undersampling



prop.table(table(training$Pobre_1 <- factor(training$Pobre_1))) ## Particios 75/25

train_undersampling <- recipe(Pobre_1 ~ ., data = training) %>%
  themis::step_downsample(Pobre_1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_undersampling$Pobre_1))

a <- nrow(training)
b <- nrow(train_undersampling)
a-b ## se eliminaron 188665 obsesrvaciones nuevas


colnames(train_undersampling)

logit_undersampling <- glm(Pobre_1 ~.,
                          data = train_undersampling, family = binomial(link="logit")) ## Estimar modelo logit en evaluate

tidy(logit_undersampling)
summary(logit_undersampling)

Pobre_1_logit_undersampling <- predict(logit_undersampling,
                                      newdata = train_undersampling,
                                      type = "response") # Estimación de predicciones de Pobre_1za
summary(Pobre_1_logit_undersampling)

ggplot(data=train_undersampling , mapping=aes(Pobre_1,Pobre_1_logit_undersampling)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## la regra de bayes parece funcionar para predecir pobreza


regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat_oversamping <- ifelse(Pobre_1_logit_undersampling>regla1,1,0) ## Prediccion de Pobreza


ggplot(train_undersampling, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(train_undersampling, aes(x = Pobre_1_hat_oversamping)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_undersampling <- confusionMatrix(data = factor(Pobre_1_hat_oversamping),
                                         reference = factor(train_undersampling$Pobre_1),
                                         mode = "sens_spec", positive = "1")

cm_logit_undersampling <- cm_logit_undersampling$table

skim(train_undersampling$Pobre_1)


acc_undersampling <- Accuracy(y_pred = Pobre_1_hat_oversamping, y_true = train_undersampling$Pobre_1)
pre_undersampling <- Precision(y_pred = Pobre_1_hat_oversamping, y_true = train_undersampling$Pobre_1, positive = 1)
rec_undersampling <- Recall(y_pred = Pobre_1_hat_oversamping, y_true = train_undersampling$Pobre_1, positive = 1)
f1_undersampling <- F1_Score(y_pred = Pobre_1_hat_oversamping, y_true = train_undersampling$Pobre_1, positive = 1)
spf_undersampling <- Specificity(y_pred = Pobre_1_hat_oversamping, y_true = train_undersampling$Pobre_1, positive = 1)
FPR_undersampling <- cm_logit_undersampling[2,1]/sum(cm_logit_undersampling[2,])
FNR_undersampling <- cm_logit_undersampling[1,2]/sum(cm_logit_undersampling[1,])

metricas_undersampling <- data.frame(Modelo = "Logit - correccion de imbalance",
                                    "Muestreo" = "undersampling", 
                                    "Evaluación" = "Dentro de muestra",
                                    "Accuracy" = acc_undersampling,
                                    "Precision" = pre_undersampling,
                                    "Recall" = rec_undersampling,
                                    "F1" = f1_undersampling,
                                    "Specificity" = spf_undersampling,
                                    "FPR" = FPR_undersampling,
                                    "FNR" = FNR_undersampling)

## Curva de ROC

predicciones_undersampling <- prediction(Pobre_1_hat_oversamping, train_undersampling$Pobre_1)
ROC_undersampling <- performance(predicciones_undersampling,"tpr","fpr")
plot(ROC_undersampling, main = "ROC curve", col="red")
abline(a = 0, b = 1)

auc_ROC_r2 <- performance(predicciones_r2, measure = "auc")
auc_ROC_r2@y.values[[1]]



### Comparación con muestra evaluate


Pobre_1_logit_undersampling_evaluate <- predict(logit_undersampling,
                                               newdata = evaluating,
                                               type = "response") # Estimación de predicciones de Pobre_1za

regla1 <- 0.5 # Se define regla de Bayes

Pobre_1_hat_oversamping_evaluate <- ifelse(Pobre_1_logit_undersampling_evaluate>regla1,1,0) ## Prediccion de Pobre_1za

cm_logit_undersampling_evaluate <- confusionMatrix(data = factor(Pobre_1_hat_oversamping_evaluate),
                                                  reference = factor(evaluating$Pobre_1),
                                                  mode = "sens_spec", positive = "1")

cm_logit_undersampling_evaluate <- cm_logit_undersampling_evaluate$table

skim(evaluating$Pobre_1)


acc_undersampling_evaluate <- Accuracy(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1)
pre_undersampling_evaluate <- Precision(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
rec_undersampling_evaluate <- Recall(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
f1_undersampling_evaluate <- F1_Score(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
spf_undersampling_evaluate <- Specificity(y_pred = Pobre_1_hat_oversamping_evaluate, y_true = evaluating$Pobre_1, positive = 1)
FPR_undersampling_evaluate <- cm_logit_undersampling_evaluate[2,1]/sum(cm_logit_undersampling_evaluate[2,])
FNR_undersampling_evaluate <- cm_logit_undersampling_evaluate[1,2]/sum(cm_logit_undersampling_evaluate[1,])

metricas_undersampling_evaluate <- data.frame(Modelo = "Logit - correcion imbalance",
                                             "Muestreo" = "undersampling", 
                                             "Evaluación" = "Fuera de muestra",
                                             "Accuracy" = acc_undersampling_evaluate,
                                             "Precision" = pre_undersampling_evaluate,
                                             "Recall" = rec_undersampling_evaluate,
                                             "F1" = f1_undersampling_evaluate,
                                             "Specificity" = spf_undersampling_evaluate,
                                             "FPR" = FPR_undersampling_evaluate,
                                             "FNR" = FNR_undersampling_evaluate)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## Remuestreo - Umbral de decisión optimo

# Esto no se debería hacer sobre la base de testeo pero se hace solo a modo ilustrativo

prubea <- seq(0.1, 0.9, length.out = 100)
opt_t <- data.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(Pobre_1_logit_evaluating > t)
  f1_t <- F1_Score(y_true = test$infielTRUE, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]

ggplot(opt_t, aes(x = t, y = F1)) +
  geom_point(size = 0.7) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = mejor_t, linetype = "dashed", 
             color = "red") +
  labs(x = "Threshold")





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

## SMOTE, Tuning model (hiperparametros), lasso

# Tabla de metricas

metricas <- bind_rows(metricas_training_r1, metricas_evaluating_r1, metricas_training_r2,
                      metricas_evaluating_r2, metricas_oversampling, metricas_oversampling_evaluate,
                      metricas_undersampling, metricas_undersampling_evaluate)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


## Linear discriminant analysis









