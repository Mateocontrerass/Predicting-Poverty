
rm(list = ls())

require(pacman)
p_load(tidyverse,dplyr,here,skimr,tidyr,gamlr,modelsummary,caret,
       rio,knitr, kableExtra, rstudioapi,tidymodels,janitor,MLmetrics,
       rattle,doParallel, themis, install = TRUE)

## Carlos

#-------------------------------------------------------------------------------

### Logit - 

#-------------------------------------------------------------------------------

### train

## Depurar base

set.seed(777) ## fijar semilla divina

training <- data_rf_train
rm(data_rf_train)

colnames(training)

#sub_set_training <- subset(training, select = -c(id, Li, Lp, P6050, P6210s1, P6426, 
#                                                P6020, P5010, P5130, Npersug, ing))

sub_set_training <- subset (training, select = -c(id, Li, Lp, ing, Npersug, P5010, P6050_3,
                                                      P6050_4, P6050_5, P6050_6, P6050_7, 
                                                      P6050_8, P6050_9, P6426, P5130,
                                                      P6210s1_0, P6210s1_1, P6210s1_10, P6210s1_11,
                                                      P6210s1_12, P6210s1_13, P6210s1_14, P6210s1_15,
                                                      P6210s1_2, P6210s1_4, P6210s1_5, P6210s1_6, P6210s1_7,
                                                      P6210s1_8, P6210s1_9, P6210s1_99, P7505_1))

#"colnames(sub_set_training)

#skim(sub_set_training)

#library(fastDummies)

#sub_set_training <- dummy_cols(sub_set_training,
#                              select_columns = c("Clase", "P6090", "P6100", "P6210", 
#                                                  "P6240", "P7505", "Depto", "P5090"))


#sub_set_training <- subset(sub_set_training, select = -c(Clase, P6090, P6100, P6210, 
#                                                        P6240, P7505, Depto, P5090))

#sub_set_training <- subset(sub_set_training, select = -c(Clase_1, P6090_1, P6100_1, P6210_3, 
#                                                         P6210_6, P6240_1, P6240_6, P7505_1, 
#                                                         Depto_05, Depto_76, P5090_1, P5090_3))

#colnames(sub_set_training)"


## Modelo

logit_training <- glm(Pobre_1 ~.,
                      data = sub_set_training, family = binomial(link="logit")) ## Esimar modelo logit

summary(logit_training)

sub_set_training$Pobre_1_logit_training <- predict(logit_training,
                                                 newdata = sub_set_training,
                                                 type = "response") # Estimación de predicciones de Pobre_1za training

summary(sub_set_training$Pobre_1_logit_training)

ggplot(data=sub_set_training , mapping=aes(Pobre_1,Pobre_1_logit_training)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## se observa que 0.35 puede servir para evitar subestimar Pobre_1za


## se generan 2 reglas, la de bayes (p=0.5), y una regla basada en la distribución de las predicciones del modelo (p=0.35)

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

sub_set_training$Pobre_1_hat1_training <- ifelse(sub_set_training$Pobre_1_logit_training>regla1,1,0) ## Prediccion de Pobre_1za


ggplot(sub_set_training, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(sub_set_training, aes(x = Pobre_1_hat1_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r1 <- confusionMatrix(data = factor(sub_set_training$Pobre_1_hat1_training),
                                        reference = factor(sub_set_training$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r1 <- cm_logit_training_r1$table

skim(sub_set_training$Pobre_1)


acc_training_r1 <- Accuracy(y_pred = sub_set_training$Pobre_1_hat1_training, y_true = sub_set_training$Pobre_1)
pre_training_r1 <- Precision(y_pred = sub_set_training$Pobre_1_hat1_training, y_true = sub_set_training$Pobre_1, positive = 1)
rec_training_r1 <- Recall(y_pred = sub_set_training$Pobre_1_hat1_training, y_true = sub_set_training$Pobre_1, positive = 1)
f1_training_r1 <- F1_Score(y_pred = sub_set_training$Pobre_1_hat1_training, y_true = sub_set_training$Pobre_1, positive = 1)
spf_training_r1 <- Specificity(y_pred = sub_set_training$Pobre_1_hat1_training, y_true = sub_set_training$Pobre_1, positive = 1)
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

sub_set_training$Pobre_1_hat2_training <- ifelse(sub_set_training$Pobre_1_logit_training>regla2,1,0) ## Prediccion de Pobre_1za


ggplot(sub_set_training, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(sub_set_training, aes(x = Pobre_1_hat2_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r2 <- confusionMatrix(data = factor(sub_set_training$Pobre_1_hat2_training),
                                        reference = factor(sub_set_training$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r2 <- cm_logit_training_r2$table

skim(sub_set_training$Pobre_1)


acc_training_r2 <- Accuracy(y_pred = sub_set_training$Pobre_1_hat2_training, y_true = sub_set_training$Pobre_1)
pre_training_r2 <- Precision(y_pred = sub_set_training$Pobre_1_hat2_training, y_true = sub_set_training$Pobre_1, positive = 1)
rec_training_r2 <- Recall(y_pred = sub_set_training$Pobre_1_hat2_training, y_true = sub_set_training$Pobre_1, positive = 1)
f1_training_r2 <- F1_Score(y_pred = sub_set_training$Pobre_1_hat2_training, y_true = sub_set_training$Pobre_1, positive = 1)
spf_training_r2 <- Specificity(y_pred = sub_set_training$Pobre_1_hat2_training, y_true = sub_set_training$Pobre_1, positive = 1)
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

predicciones_r1 <- prediction(sub_set_training$Pobre_1_hat1_training, sub_set_training$Pobre_1)
ROC_r1 <- performance(predicciones_r1,"tpr","fpr")
plot(ROC_r1, main = "ROC curve", col="red")
abline(a = 0, b = 1)


predicciones_r2 <- prediction(sub_set_training$Pobre_1_hat2_training, sub_set_training$Pobre_1)
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


sub_set_evaluating <- subset (evaluating, select = -c(id, Li, Lp, ing, Npersug, P5010, P6050_3,
                                                      P6050_4, P6050_5, P6050_6, P6050_7, 
                                                      P6050_8, P6050_9, P6426, P5130,
                                                      P6210s1_0, P6210s1_1, P6210s1_10, P6210s1_11,
                                                      P6210s1_12, P6210s1_13, P6210s1_14, P6210s1_15,
                                                      P6210s1_2, P6210s1_4, P6210s1_5, P6210s1_6, P6210s1_7,
                                                      P6210s1_8, P6210s1_9, P6210s1_99, P7505_1))
colnames(sub_set_evaluating)


logit_evaluate <- glm(Pobre_1 ~.,
                      data = sub_set_evaluating, family = binomial(link="logit")) ## Estimar modelo logit en evaluate

tidy(logit_evaluate)
summary(logit_evaluate)

sub_set_evaluating$Pobre_1_logit_evaluating <- predict(logit_evaluate,
                                                     newdata = sub_set_evaluating,
                                                     type = "response") # Estimación de predicciones de Pobre_1za
summary(sub_set_evaluating$Pobre_1_logit_evaluating)

ggplot(data=sub_set_evaluating , mapping=aes(Pobre_1,Pobre_1_logit_evaluating)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## parece que 0.35 es el valor para evitar subestimar a los Pobre_1s


## Se prueban las mismas 2 reglas usadas en la muestra training

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

sub_set_evaluating$Pobre_1_hat1_evaluating <- ifelse(sub_set_evaluating$Pobre_1_logit_evaluating>regla1,1,0) ## Prediccion de Pobre_1za


ggplot(sub_set_evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(sub_set_evaluating, aes(x = Pobre_1_hat1_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r1 <- confusionMatrix(data = factor(sub_set_evaluating$Pobre_1_hat1_evaluating),
                                        reference = factor(sub_set_evaluating$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_evaluating_r1 <- cm_logit_evaluating_r1$table

skim(sub_set_evaluating$Pobre_1)


acc_evaluating_r1 <- Accuracy(y_pred = sub_set_evaluating$Pobre_1_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1)
pre_evaluating_r1 <- Precision(y_pred = sub_set_evaluating$Pobre_1_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
rec_evaluating_r1 <- Recall(y_pred = sub_set_evaluating$Pobre_1_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
f1_evaluating_r1 <- F1_Score(y_pred = sub_set_evaluating$Pobre_1_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
spf_evaluating_r1 <- Specificity(y_pred = sub_set_evaluating$Pobre_1_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
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

sub_set_evaluating$Pobre_1_hat2_evaluating <- ifelse(sub_set_evaluating$Pobre_1_logit_evaluating>regla2,1,0) ## Prediccion de Pobre_1za


ggplot(sub_set_evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(sub_set_evaluating, aes(x = Pobre_1_hat2_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r2 <- confusionMatrix(data = factor(sub_set_evaluating$Pobre_1_hat2_evaluating),
                                        reference = factor(sub_set_evaluating$Pobre_1),
                                        mode = "sens_spec", positive = "1")

cm_logit_evaluating_r2 <- cm_logit_evaluating_r2$table

skim(sub_set_evaluating$Pobre_1)


acc_evaluating_r2 <- Accuracy(y_pred = sub_set_evaluating$Pobre_1_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1)
pre_evaluating_r2 <- Precision(y_pred = sub_set_evaluating$Pobre_1_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
rec_evaluating_r2 <- Recall(y_pred = sub_set_evaluating$Pobre_1_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
f1_evaluating_r2 <- F1_Score(y_pred = sub_set_evaluating$Pobre_1_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
spf_evaluating_r2 <- Specificity(y_pred = sub_set_evaluating$Pobre_1_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
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

prop.table(table(sub_set_training$Pobre_1 <- factor(sub_set_training$Pobre_1))) ## Particios 75/25

train_oversampling <- recipe(Pobre_1 ~ ., data = sub_set_training) %>%
  themis::step_smote(Pobre_1, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_oversampling$Pobre_1))

a <- nrow(sub_set_training)
b <- nrow(train_oversampling)
b-a ## se crearon 188665 obsesrvaciones nuevas

train_oversampling <- subset(train_oversampling, select = -c(Pobre_1_logit_training,
                                                             Pobre_1_hat1_training,
                                                             Pobre_1_hat2_training))

logit_oversampled <- glm(Pobre_1 ~.,
                         data = train_oversampling, family = binomial(link="logit")) ## Estimar modelo logit en evaluate

tidy(logit_oversampled)
summary(logit_oversampled)

train_oversampling$Pobre_1_logit_oversampled <- predict(logit_oversampled,
                                                        newdata = train_oversampling,
                                                        type = "response") # Estimación de predicciones de Pobre_1za
summary(train_oversampling$Pobre_1_logit_oversampled)

ggplot(data=train_oversampling , mapping=aes(Pobre_1,Pobre_1_logit_oversampled)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## la regra de bayes parece funcionar para predecir pobreza


regla1 <- 0.5 # Se define regla de Bayes

train_oversampling$Pobre_1_hat_oversamping <- ifelse(train_oversampling$Pobre_1_logit_oversampled>regla1,1,0) ## Prediccion de Pobreza


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

cm_logit_oversampling <- confusionMatrix(data = factor(train_oversampling$Pobre_1_hat_oversamping),
                                          reference = factor(train_oversampling$Pobre_1),
                                          mode = "sens_spec", positive = "1")

cm_logit_oversampling <- cm_logit_oversampling$table

skim(train_oversampling$Pobre_1)


acc_oversampling <- Accuracy(y_pred = train_oversampling$Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1)
pre_oversampling <- Precision(y_pred = train_oversampling$Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
rec_oversampling <- Recall(y_pred = train_oversampling$Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
f1_oversampling <- F1_Score(y_pred = train_oversampling$Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
spf_oversampling <- Specificity(y_pred = train_oversampling$Pobre_1_hat_oversamping, y_true = train_oversampling$Pobre_1, positive = 1)
FPR_oversampling <- cm_logit_oversampling[2,1]/sum(cm_logit_oversampling[2,])
FNR_oversampling <- cm_logit_oversampling[1,2]/sum(cm_logit_oversampling[1,])

metricas_oversampling <- data.frame(Modelo = "Logit - Regla de Bayes",
                                     "Muestreo" = "Oversampling", 
                                     "Evaluación" = "Dentro de muestra",
                                     "Accuracy" = acc_oversampling,
                                     "Precision" = pre_oversampling,
                                     "Recall" = rec_oversampling,
                                     "F1" = f1_oversampling,
                                     "Specificity" = spf_oversampling,
                                     "FPR" = FPR_oversampling,
                                     "FNR" = FNR_oversampling)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

metricas <- bind_rows(metricas_training_r1, metricas_training_r2, metricas_evaluating_r1, metricas_evaluating_r2,
                      metricas_oversampling)
metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------





## Linear discriminant analysis



