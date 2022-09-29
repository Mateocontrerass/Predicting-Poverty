## Problem Set 2 : Predicting Poverty.
# Big data and Machine Learning

# Mateo Contreras
# Carlos Ayala
# Federico Meneses

# Punto 1

#------------------------------------------------------------------------------
# Limpiar el espacio de trabajo

#Enviroment
rm(list=ls())
#Console
cat("\f")

#------------------------------------------------------------------------------
# Cargar paquetes
require(pacman)
p_load(tidyverse,dplyr,here,skimr,tidyr,gamlr,modelsummary,caret,
       rio,knitr, kableExtra, rstudioapi,tidymodels,janitor,MLmetrics,
       rattle,doParallel, install = TRUE)
library(tidyverse)




#------------------------------------------------------------------------------
# Cargar las bases de datos


path_code <- dirname(getActiveDocumentContext()$path)
setwd(path_code)
getwd()
  #Con este set de comandos se pone el directorio solo de cada persone


hogares <- readRDS("data/train_hogares.Rds")
personas <- readRDS("data/train_personas.Rds")



#------------------------------------------------------------------------------
#  Analísis exploratorio de ambas bases por separado.
colnames(hogares)
  #skim(hogares)

colnames(personas)
  # id presente en ambas tablas


#------------------------------------------------------------------------------
# Unir las bases
base_completa <-personas %>%  left_join(hogares)
  #Unión de ambas bases.


base_completa<-subset(base_completa,select=c(-Dominio,-Orden,-Fex_c,-Fex_dpto,-Npobres,-Nindigentes,
                                             -P7510s1,
                                             -P7510s2,-P7510s3,-P7510s5,-P7510s6,
                                             -P7510s7,-P7500s2,-P7500s3,-Oficio,-Des,-Pet,-Oc,-Ina))

  #Dropeamos dominio que es lo mismo que Depto


rm(hogares,personas)
  #Borramos las bases que no utilizamos

objeto<-skim(base_completa)
  #Guardamos el skim para sacar el complete_rate
  


ing<-base_completa$Ingtot
  #por algun motivo, a pesar de tener un buen complete rate, al eliminar los
  #complete rate<0.5 se va el ingreso, por lo que se guarda y se adiciona 
  #posteriormente.



indices_chiquitos<-which(objeto$complete_rate>.5)



  #Indices de complete rate>0.5
base_completa<-subset(base_completa,select=c(indices_chiquitos))

base_completa<-cbind(base_completa,ing)
  #reoincorporamos la variable de ingreso



#------------------------------------------------------------------------------

# Borramos las variables que no están en ambas bases de datos (base_completa vs train)


hogares <- readRDS("data/test_hogares.Rds")
personas <- readRDS("data/test_personas.Rds")


  #Cargamos las bases test

test <-personas %>%  left_join(hogares) 
  #Pegamos las bases train

test<-subset(test,select=c(-Dominio,-Orden,-Fex_c,-Fex_dpto))
  #Dominio no sirve porque es igual a depto


rm(hogares,personas)
  #Borramos las bases individuales
#------------------------------------------------------------------------------




columnas_test<-c(names(test))

columnas_base<-c(names(base_completa))

  #Sacar las columnas presentes en en los DF


  columnas_total<-intersect(columnas_base,columnas_test)
  #Intersección entre variables comunes
  
  columnas_total
  
  remover <- c("Pobre","ing")
  
  columnas<-append(columnas_total,remover)
  #Agregamos pobre a la base total
  
  test<-subset(test,select=c(columnas_total))
  #Para que test tenga las mismas variables

  base_completa<-subset(base_completa,select=(columnas))
  #Depuramos la base para que tenga las mismas columnas que test.
  #Si no se tienen las mismas features falla el modelo.
  
#------------------------------------------------------------------------------
  
  
  
  variables_categoricas <- c("Depto","P6020","P6050","P6090","P6100",
                             "P6210","P6210s1","P6240","P7505",
                             "P5090","Clase","Pobre")
  
  
  
  base_completa[,variables_categoricas]<-lapply(base_completa[,variables_categoricas],factor)
  
  variables_categoricas <- c("Depto","P6020","P6050","P6090","P6100",
                             "P6210","P6210s1","P6240","P7505",
                             "P5090","Clase")
  
  
  test[,variables_categoricas]<-lapply(test[,variables_categoricas],factor)  
  
  #------------------------------------------------------------------------------
  
library(tidytable)
tr_cat<-subset(base_completa,select=c("Depto","P6020","P6050","P6090","P6100",
                                      "P6210","P6210s1","P6240","P7505",
                                      "P5090","Clase","Pobre"))

tr_cat<-get_dummies(tr_cat,drop_first = T,dummify_na=F)

tr_cat<-subset(tr_cat,select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                                -P6210,-P6210s1,-P6240,-P7505,
                                -P5090,-Clase,-Pobre))

te_cat<-subset(test,select=c("Depto","P6020","P6050","P6090","P6100",
                                            "P6210","P6210s1","P6240","P7505",
                                            "P5090","Clase"))

te_cat<-get_dummies(te_cat,drop_first = T,dummify_na=F)


te_cat<-subset(te_cat,select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                                -P6210,-P6210s1,-P6240,-P7505,
                                -P5090,-Clase))


colnames(te_cat)
colnames(tr_cat)
detach("package:tidytable", unload = TRUE)

#Reiniciar si el codigo falla porque tidytable cambia parte de la sintaxis :))))



#------------------------------------------------------------------------------

base_completa1<-cbind(base_completa,tr_cat)

base_completa1<-subset(base_completa1, select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                               -P6210,-P6210s1,-P6240,-P7505,
                               -P5090,-Clase,-Pobre))

test1<-cbind(test,te_cat)

test1<-subset(test1,select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                                             -P6210,-P6210s1,-P6240,-P7505,
                                             -P5090,-Clase))

columnas_test<-c(names(test1))

columnas_base<-c(names(base_completa1))

#Sacar las columnas presentes en en los DF


columnas_total<-intersect(columnas_base,columnas_test)
#Intersección entre variables comunes

columnas_total

remover <- c("Pobre_1","ing")

columnas<-append(columnas_total,remover)
#Agregamos pobre a la base total

test1<-subset(test1,select=c(columnas_total))
#Para que test tenga las mismas variables

base_completa1<-subset(base_completa1,select=(columnas))
#Depuramos la base para que tenga las mismas columnas que test.
#Si no se tienen las mismas features falla el modelo.


test<-test1
base_completa<-base_completa1

rm(tr_cat,te_cat,objeto,base_completa1,test1)
#------------------------------------------------------------------------------
  #Factor para variables de df test
  
  
  #p6020: sexo
  #p6040:años cumplidos
  #p6050: parentesco con el jefe del hogar: factor
  #p6090: afiliado o cotizante a SS
  #p6100: regimen afiliado: subs coti ecopetrol : factor
  #p6210 : rango nivel educativo más alto : factor
  # ":s1 : grado escolar aprobado : discrita numerica
  #p6240: actividad que ocupó tiempo : factor
  # oficio : factor
  #p7500s2: recibió pago por pensiones? : factor
  # "s3: más pagos: factor.
  # p7505: recibió dinero de algun lado? :factor : si/no
  # p7510s1: recibió dinero otrade residentes : factor
  # " s2: dinero de fuera del pais: factor
  # " s3: dinero de instituciones: factor
  # " s5: dinero de activos con banco: factor
  # " s6: dinero cesantias: factor
  # " s7: otros : factor
  # Pet : edad trabajar : factor
  # Oc: Ocupado : factor
  #Fex_c : factor de expansión: no se que es #######
  #fex_dpto: factor expan... tampoco se
  #p5000: numero de cuartos: numerico
  #p5010: en cuantos cuartos duermen las personas: numerico
  #p5090: vivienda arriendo o propia: factor
  #p5130: cuanto pagaría de arriendo: numerico
  #nper: personass en hogar: numerico
  #npersug: #per unidad gasto: numerico
  #Li: linea indigencia: no utilizar
  #linea pobreza:LP : no utilizar
  
  


  
  
  
  
  save(test,file="data/test")
  

  
#------------------------------------------------------------------------------


# Generando muestra train y evaluate 

prop.table(table(base_completa$Pobre))
#74% personas no pobres y 25% de personas pobres.

set.seed(666)

split1 <- createDataPartition(base_completa$Pobre , p = 0.7)[[1]]

training = base_completa[split1,]
#Base de entrenamiento con 70% de los datos

evaluating = base_completa[-split1,]
#Base de evaluación de hiperparametros

#No hace falta hacer la partición para la muestra test dado que el ejercicio ya la da.

rm(base_completa)
rm(columnas,columnas_test,remover,split1,variables_categoricas)
  #Para mantener el espacio de trabajo limpio

save(evaluating,file="data/evaluating")


#------------------------------------------------------------------------------

#Checkear particiones

prop.table(table(training$Pobre))
prop.table(table(evaluating$Pobre))
  #Todo parece estar en orden.


#------------------------------------------------------------------------------

# Analisis descriptivo

# Objetivo: Tratar NAN y convertir valores a factor si es necesario.

  #skim(training)

  #Observaciones preliminares:
   
    # Variables str:

      # ID: No util. No introducir a los modelos. 
      # Clase: 1 Urbano 2 Rural. Considero que es util.
      # Dominio: indicador de cabecera municipal. Depende del modelo podria ser util.
      # Depto: departamento. Util.





  # Sugiero descartar las variables que tienen una tasa de completado <0.5 
  # porque considero que pueden hacer mucho más ruido del que aportan dado que 
  # me gustaria imputar los NAN a ver que tal. 


#------------------------------------------------------------------------------
  #Imputación de datos

  #install.packages("mixgb")               
  library("mixgb")

  identificadores<-subset(training,select=c("id","Li","Lp"))
  
  train_set<-subset(training,select=c(-id,-Li,-Lp))
  #Esto para guardar estas variables que no pueden entrar en el mixgb
  
  imputed_test<-mixgb(data=train_set,verbose=TRUE,m=1)
  #Imputación de los datos
  
  
  data_imputada<-imputed_test[[1]]
  #Saco el primer dataframe

  
  data_rf_train<-cbind(identificadores,data_imputada)
  #Pego los identificadores
  
  
  save(data_rf_train,file="data/data_imputada2")

  
#------------------------------------------------------------------------------
  
  #Base train imputada  
  load("data/data_imputada2")
  
  training<-data_rf_train
  
  colnames(data_rf_train)
  
  #Base test
  load("data\\test")
  

  #Base evaluación
  load("data/evaluating")
  


  #Por algún motivo, el objeto se guarda como : "data_rf_train"
  
  


#------------------------------------------------------------------------------

  # NOTAS IMPORTANTES

  # No utilizar las siguientes variables para la estimación: 
  # ID / Li / Lp 




#------------------------------------------------------------------------------

# Implementación de modelos

#Elastic net Federico
#install.packages("glmnet")
library(glmnet)
## Lasso
x<- select(training, c(-Pobre, -id))
y <- training$Pobre

modelo_lasso <- glmnet(
  x,
  y,
  alpha = 1,
  nlambda = 300,
  standardize = FALSE
)

# Analicemos c?mo cambian los coeficientes para diferentes lambdas
regularizacion <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")

newx<-data.matrix(select(evaluating,c(-id, -Pobre)))
predicciones_lasso <- predict(modelo_lasso, 
                              newx )
lambdas_lasso <- modelo_lasso$lambda

# Cada predicci?n se va a evaluar
y_test = evaluating$Pobre
resultados_lasso <- data.frame()
for (i in 1:length(lambdas_lasso)) {
  l <- lambdas_lasso[i]
  y_hat_out2 <- predicciones_lasso[, i]
  r22 <- R2_Score(y_pred = y_hat_out2, y_true = y_test)
  rmse2 <- RMSE(y_pred = y_hat_out2, y_true = y_test)
  resultado <- data.frame(Modelo = "Lasso",
                          Muestra = "Fuera",
                          Lambda = l,
                          R2_Score = r22, 
                          RMSE = rmse2)
  resultados_lasso <- bind_rows(resultados_lasso, resultado)
}

ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)



#Ridge


#------------------------------------------------------------------------------

  #LM para ingreso

train_pred<-subset(data_rf_train,select=c(-id,-Li,-Lp,-Pobre))


reg_lin<-lm(ing~.,data=train_pred)
summary(reg_lin)


y_out_lm<-predict(reg_lin,evaluating)


#------------------------------------------------------------------------------

# Random Forest (Mateo)





















  # Modelo basico de clasificación sin imputación para un arbol
  
modelo1 <- decision_tree(mode="classification") 
  #Establecimiento del modelo

training$Pobre<-factor(training$Pobre)
evaluating$Pobre<-factor(evaluating$Pobre)


modelo1_fit <- fit(modelo1, Pobre ~ . -id -Li -Lp, data = training)

modelo1_fit

training$Pobre
testing$pobre


  # Desempeño

y_hat_insample <- predict(modelo1_fit, training)$.pred_class
y_hat_outsample <- predict(modelo1_fit, evaluating)$.pred_class


training$Pobre

acc_in <- Accuracy(y_true = training$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = training$Pobre, y_pred = y_hat_insample)
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample)
recall_in <- round(100*recall_in, 2)

f1_in <- F1_Score(y_true = training$Pobre, y_pred = y_hat_insample)

f1_in <- round(100*f1_in, 2)


evaluating$Pobre

acc_out <- Accuracy(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
recall_out <- round(100*recall_out, 2)

f1_out <- F1_Score(y_true = evaluating$Pobre, y_pred = y_hat_outsample)

f1_out <- round(100*f1_out, 2)

resultados <- data.frame(Modelo = "Modelo 1", Base = c("Train", "Test"), 
                         Accuracy = c(acc_in, acc_out), 
                         Precision = c(pre_in, pre_out),
                         Recall = c(recall_in, recall_out),
                         F1 = c(f1_in, f1_out))

kbl(resultados) %>%
  kable_styling(full_width = T)


  # Modelo basico de clasificación con imputación
  

modelo1_fit <- fit(modelo1, Pobre ~ . -id -Li -Lp, data = data_rf_train)

modelo1_fit


# Desempeño

y_hat_insample <- predict(modelo1_fit, data_rf_train)$.pred_class
y_hat_outsample <- predict(modelo1_fit, evaluating)$.pred_class


training$Pobre

acc_in <- Accuracy(y_true = data_rf_train$Pobre, y_pred = y_hat_insample)
acc_in <- round(100*acc_in, 2)
pre_in <- Precision(y_true = data_rf_train$Pobre, y_pred = y_hat_insample)
pre_in <- round(100*pre_in, 2)
recall_in <- Recall(y_true = training$Pobre, y_pred = y_hat_insample)
recall_in <- round(100*recall_in, 2)

f1_in <- F1_Score(y_true = data_rf_train$Pobre, y_pred = y_hat_insample)

f1_in <- round(100*f1_in, 2)


evaluating$Pobre

acc_out <- Accuracy(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
acc_out <- round(100*acc_out, 2)
pre_out <- Precision(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
pre_out <- round(100*pre_out, 2)
recall_out <- Recall(y_true = evaluating$Pobre, y_pred = y_hat_outsample)
recall_out <- round(100*recall_out, 2)

f1_out <- F1_Score(y_true = evaluating$Pobre, y_pred = y_hat_outsample)

f1_out <- round(100*f1_out, 2)

resultados2 <- data.frame(Modelo = "Modelo 1 imp", Base = c("Train", "Test"), 
                         Accuracy = c(acc_in, acc_out), 
                         Precision = c(pre_in, pre_out),
                         Recall = c(recall_in, recall_out),
                         F1 = c(f1_in, f1_out))

kbl(resultados) %>%
  kable_styling(full_width = T)

resultados <- rbind(resultados, resultados2)
resultados

#Peor resultado out of sample para imputados :(

#------------------------------------------------------------------------------

  # Random Forest para clasificación

install.packages("randomForest")
library("randomForest")

#Base de datos para clasificación:

train_clas<-subset(data_rf_train,select=c(-id,-Li,-Lp,-ing))


#Demasiado pesado, voy a partir la base otra vez

split1 <- createDataPartition(train_clas$Pobre , p = 0.1)[[1]]

train_pequeña<- train_clas[split1,]



ctrl<-trainControl(method="cv",number=2,verbose=TRUE,savePredictions=T,
                   summaryFunction = twoClassSummary )


forest<-train(Pobre~. , data=train_pequeña,method="rf",
              trControl=ctrl,
              family="binomial",
              metric="Sens")


ttest<-subset(test,select=c(-id,-Li,-Lp))

y_out<-predict(forest,ttest)


#------------------------------------------------------------------------------

  # Random Forest para predicción de ingreso


#Base para predicción

train_pred<-subset(data_rf_train,select=c(-id,-Li,-Lp,-Pobre))

split1 <- createDataPartition(train_pred$Pobre , p = 0.1)[[1]]

train_pequeña_pred<- train_clas[split1,]

forest<-train(ing~. , data=train_pequeña_pred,method="rf",
              trControl=ctrl,
              family="binomial",metric="Sens")



#------------------------------------------------------------------------------



library(tidytable)  

base_completa<-get_dummies(base_completa,cols=c("Depto","P6020","P6050","P6090","P6100",
                                                "P6210","P6210s1","P6240","P7505",
                                                "P5090","Clase","Pobre"), drop_first = T)

base_completa<-subset(base_completa,select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                                             -P6210,-P6210s1,-P6240,-P7505,
                                             -P5090,-Clase,-Pobre))


test<-get_dummies(test,cols=c("Depto","P6020","P6050","P6090","P6100",
                              "P6210","P6210s1","P6240","P7505",
                              "P5090","Clase"), drop_first = T)

test<-subset(test,select=c(-Depto,-P6020,-P6050,-P6090,-P6100,
                           -P6210,-P6210s1,-P6240,-P7505,
                           -P5090,-Clase))


compare_df_cols(test,base_completa)
detach("package:tidytable", unload = TRUE)






