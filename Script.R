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
install.packages("mixgb")
library("mixgb")

set.seed(666)



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
  
  
identificadores<-subset(test,select=c("id","Li","Lp"))

train_set<-subset(test,select=c(-id,-Li,-Lp))
#Esto para guardar estas variables que no pueden entrar en el mixgb

imputed_test<-mixgb(data=train_set,verbose=TRUE,m=1)
#Imputación de los datos


test<-imputed_test[[1]]
#Saco el primer dataframe


test<-cbind(identificadores,test)


  
  
  
  save(test,file="data/test")
  

  
#------------------------------------------------------------------------------


# Generando muestra train y evaluate 

prop.table(table(base_completa$Pobre))
#74% personas no pobres y 25% de personas pobres.


split1 <- createDataPartition(base_completa$Pobre , p = 0.7)[[1]]

training = base_completa[split1,]
#Base de entrenamiento con 70% de los datos

evaluating = base_completa[-split1,]
#Base de evaluación de hiperparametros

#No hace falta hacer la partición para la muestra test dado que el ejercicio ya la da.

rm(base_completa)
rm(columnas,columnas_test,remover,split1,variables_categoricas)
  #Para mantener el espacio de trabajo limpio



identificadores<-subset(evaluating,select=c("id","Li","Lp"))

ev_set<-subset(evaluating,select=c(-id,-Li,-Lp))
#Esto para guardar estas variables que no pueden entrar en el mixgb

imputed_ev<-mixgb(data=ev_set,verbose=TRUE,m=1)
#Imputación de los datos


evaluating<-imputed_ev[[1]]
#Saco el primer dataframe


evaluating<-cbind(identificadores,evaluating)




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

  identificadores<-subset(training,select=c("id","Li","Lp"))
  
  train_set<-subset(training,select=c(-id,-Li,-Lp))
  #Esto para guardar estas variables que no pueden entrar en el mixgb
  
  imputed_test<-mixgb(data=train_set,verbose=TRUE,m=1)
  #Imputación de los datos
  
  
  data_imputada<-imputed_test[[1]]
  #Saco el primer dataframe

  
  training<-cbind(identificadores,data_imputada)
  #Pego los identificadores
  
  
  save(data_rf_train,file="data/entrenamiento")

  
#------------------------------------------------------------------------------
  
  #Base train imputada  
  load("data/entrenamiento")
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

  #install.packages("glmnet")
  library(glmnet)
  ## Lasso
    x<- subset(training, select = c(-Li,-Lp,-id,-Pobre_1, -ing))
    y <- training$ing
    
    modelo_lasso <- glmnet(
      x,
      y,
      alpha = 1,
      nlambda = ,
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
    
    newx<-data.matrix(subset(evaluating, select = c(-Li,-Lp,-id,-Pobre_1,-ing)))
    predicciones_lasso <- predict(modelo_lasso, 
                                  newx )
    lambdas_lasso <- modelo_lasso$lambda
    
    # Cada predicci?n se va a evaluar
    y_test = evaluating$ing
    resultados_lasso <- data.frame()
    for (i in 1:length(lambdas_lasso)) {
      l <- lambdas_lasso[i]
      y_hat_out2 <- predicciones_lasso[, i]
      r22 <- R2_Score(y_pred = y_hat_out2, y_true = evaluating$ing)
      rmse2 <- RMSE(y_pred = y_hat_out2, y_true = evaluating$ing)
      resultado <- data.frame(Modelo = "Lasso",
                              Muestra = "Fuera",
                              Lambda = l,
                              R2_Score = r22, 
                              RMSE = rmse2)
      resultados_lasso <- bind_rows(resultados_lasso, resultado)
    }
    
    #Revisamos los posibles MSE
    
    ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    filtro <- resultados_lasso$RMSE == min(resultados_lasso$RMSE)
    mejor_lambda_lasso <- resultados_lasso[filtro, "Lambda"]
    
    # Guardamos el mejor Lasso
    #predicción con la base de datos training
    newxt<-data.matrix(subset(training, select = c(-Li,-Lp,-id,-Pobre_1, -ing)))
    y_hat_in2 <- predict.glmnet(modelo_lasso,
                                newxt,
                                s = mejor_lambda_lasso)
    
    #predicción con la base de datos test
    y_hat_out2 <- predict.glmnet(modelo_lasso,
                                 newx,
                                 s = mejor_lambda_lasso)
    #Evaluamos
    
    pobre_hat_lasso<-ifelse(y_hat_out2<evaluating$Lp,1,0)
    plot(pobre_hat_lasso)
    
    pobre_hat_lasso<-factor(pobre_hat_lasso)
    Pobre_1<-factor(evaluating$Pobre_1)
    
    cm_lasso<-confusionMatrix(pobre_hat_lasso,Pobre_1)
    
    
    resultados<-data.frame(Modelo="Lasso",Base="Predicción",
                           Accuracy=cm_lasso$overall[1],
                           Sensitivity=cm_lasso$byClass[1],
                           Specificity=cm_lasso$byClass[2])
    
    
    
    resultados
  
  #Ridge 
  modelo_ridge <- glmnet(
    x ,
    y ,
    alpha = 0,
    nlambda = ,
    standardize = FALSE
  )
  
  # Analicemos cómo cambian los coeficientes para diferentes lambdas
  regularizacion2 <- modelo_ridge$beta %>% 
    as.matrix() %>%
    t() %>% 
    as_tibble() %>%
    mutate(lambda = modelo_ridge$lambda)
  
  regularizacion2 <- regularizacion2 %>%
    pivot_longer(
      cols = !lambda, 
      names_to = "predictor",
      values_to = "coeficientes"
    )
  #Encontrar mejor lambda
  regularizacion2 %>%
    ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
    geom_line() +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10",
                                    scales::math_format(10^.x))
    ) +
    labs(title = "Coeficientes del modelo en función de la regularización (Ridge)", x = "Lambda", y = "Coeficientes") +
    theme_bw() +
    theme(legend.position="bottom")
  
  predicciones_ridge <- predict(modelo_ridge, 
                                newx)
  lambdas_ridge <- modelo_ridge$lambda
  
  # Cada predicción se va a evaluar
  resultados_ridge <- data.frame()
  for (i in 1:length(lambdas_ridge)) {
    l <- lambdas_ridge[i]
    y_hat_out3 <- predicciones_ridge[, i]
    r23 <- R2_Score(y_pred = y_hat_out3, y_true = evaluating$ing)
    rmse3 <- RMSE(y_pred = y_hat_out3, y_true = evaluating$ing)
    resultado <- data.frame(Modelo = "Ridge",
                            Muestra = "Fuera",
                            Lambda = l,
                            R2_Score = r23, 
                            RMSE = rmse3)
    resultados_ridge <- bind_rows(resultados_ridge, resultado)
  }
  
  ggplot(resultados_ridge, aes(x = Lambda, y = RMSE)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)
  
  filtro <- resultados_ridge$RMSE == min(resultados_ridge$RMSE)
  mejor_lambda_ridge <- resultados_ridge[filtro, "Lambda"]
  
  #evaluamos los resultados
  resultados_ridge_1 <-predict(modelo_ridge, newxt, s = mejor_lambda_ridge)
  
  resultados_ridge_2 <-predict(modelo_ridge, newx, s = mejor_lambda_ridge)
  
  pobre_hat_r<-ifelse(resultados_ridge_2<evaluating$Lp,1,0)
  plot(pobre_hat_r)
  
  pobre_hat_r<-factor(pobre_hat_r)
  Pobre_1<-factor(evaluating$Pobre_1)
  
  cm_ridge<-confusionMatrix(evaluating$pobre_hat_r,evaluating$Pobre_1)
  
  
  resultados<-data.frame(Modelo="ridge",Base="Predicción",
                         Accuracy=cm_ridge$overall[1],
                         Sensitivity=cm_ridge$byClass[1],
                         Specificity=cm_ridge$byClass[2])
  resultados
  
  #Elastic net 
  set.seed(666)
  modelo_elastic<-cv.glmnet(
    x=newxt,
    y,
    lambda = NULL,
    type.measure = c("default", "mse", "deviance", "class", "auc", "mae", "C"),
    nfolds = 10,
    foldid = NULL,
    alignment = c("lambda", "fraction"),
    grouped = TRUE,
    keep = FALSE,
    parallel = FALSE,
    gamma = c(0, 0.25, 0.5, 0.75, 1),
   
  )
  
  plot(modelo_elastic)
  coef(modelo_elastic)
  
  predicciones_elastic <- predict(modelo_elastic, 
                                newx)
  lambdas_elastic <- modelo_elastic$lambda
  
  
  resultados_elastic_1 <-predict(modelo_elastic, newxt, s = "lambda.min")

  resultados_elastic_2 <-predict(modelo_elastic, newx, s = "lambda.min")
  
  pobre_hat_elastic<-ifelse(resultados_elastic_2<evaluating$Lp,1,0)
  plot(pobre_hat_elastic)
  
  pobre_hat_elastic<-factor(pobre_hat_elastic)
  Pobre_1<-factor(Pobre_1)
  
  cm_elastic<-confusionMatrix(pobre_hat_elastic,Pobre_1)
  
  
  resultados<-data.frame(Modelo="elastic net",Base="Predicción",
                         Accuracy=cm_elastic$overall[1],
                         Sensitivity=cm_elastic$byClass[1],
                         Specificity=cm_elastic$byClass[2])
  resultados
#------------------------------------------------------------------------------

  #LM para ingreso

train_pred<-subset(training,select=c(-id,-Li,-Lp,-Pobre_1))


reg_lin<-lm(ing~.,data=train_pred)
summary(reg_lin)


y_out_lm<-predict(reg_lin,evaluating)



#===============================================================================

## Carlos

#-------------------------------------------------------------------------------

### Logit - 

#-------------------------------------------------------------------------------

load("data/data_imputada2")
load("data/evaluating")

### train

## Depurar base

set.seed(777) ## fijar semilla divina

training <- data_rf_train
rm(data_rf_train)

colnames(training)

sub_set_training <- subset(training, select = -c(id, Li, Lp, P6050, P6210s1, P6426, 
                                                 P6020, P5010, P5130, Npersug, ing))

colnames(sub_set_training)

skim(sub_set_training)

library(fastDummies)

sub_set_training <- dummy_cols(sub_set_training,
                               select_columns = c("Clase", "P6090", "P6100", "P6210", 
                                                  "P6240", "P7505", "Depto", "P5090"))

#Modelo fuera de muestra
evaluating$ing_hat<-predict(reg_lin,evaluating)

sub_set_training <- subset(sub_set_training, select = -c(Clase, P6090, P6100, P6210, 
                                                         P6240, P7505, Depto, P5090))

sub_set_training <- subset(sub_set_training, select = -c(Clase_1, P6090_1, P6100_1, P6210_3, 
                                                         P6210_6, P6240_1, P6240_6, P7505_1, 
                                                         Depto_05, Depto_76, P5090_1, P5090_3))

colnames(sub_set_training)


## Modelo

logit_training <- glm(Pobre ~.,
                      data = sub_set_training, family = binomial(link="logit")) ## Esimar modelo logit

summary(logit_training)

sub_set_training$Pobre_logit_training <- predict(logit_training,
                                                 newdata = sub_set_training,
                                                 type = "response") # Estimación de predicciones de pobreza training

summary(sub_set_training$Pobre_logit_training)

ggplot(data=sub_set_training , mapping=aes(Pobre,Pobre_logit_training)) + 
  geom_boxplot(aes(fill=as.factor(Pobre))) + theme_test() ## se observa que 0.35 puede servir para evitar subestimar pobreza


## se generan 2 reglas, la de bayes (p=0.5), y una regla basada en la distribución de las predicciones del modelo (p=0.35)

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

sub_set_training$Pobre_hat1_training <- ifelse(sub_set_training$Pobre_logit_training>regla1,1,0) ## Prediccion de pobreza


ggplot(sub_set_training, aes(x = Pobre)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre?",
       x = "",
       y = "Distribución")

ggplot(sub_set_training, aes(x = Pobre_hat1_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r1 <- confusionMatrix(data = factor(sub_set_training$Pobre_hat1_training),
                                        reference = factor(sub_set_training$Pobre),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r1 <- cm_logit_training_r1$table

skim(sub_set_training$Pobre)


acc_training_r1 <- Accuracy(y_pred = sub_set_training$Pobre_hat1_training, y_true = sub_set_training$Pobre)
pre_training_r1 <- Precision(y_pred = sub_set_training$Pobre_hat1_training, y_true = sub_set_training$Pobre, positive = 1)
rec_training_r1 <- Recall(y_pred = sub_set_training$Pobre_hat1_training, y_true = sub_set_training$Pobre, positive = 1)
f1_training_r1 <- F1_Score(y_pred = sub_set_training$Pobre_hat1_training, y_true = sub_set_training$Pobre, positive = 1)
spf_training_r1 <- Specificity(y_pred = sub_set_training$Pobre_hat1_training, y_true = sub_set_training$Pobre, positive = 1)
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

sub_set_training$Pobre_hat2_training <- ifelse(sub_set_training$Pobre_logit_training>regla2,1,0) ## Prediccion de pobreza


ggplot(sub_set_training, aes(x = Pobre)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre?",
       x = "",
       y = "Distribución")

ggplot(sub_set_training, aes(x = Pobre_hat2_training)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_training_r2 <- confusionMatrix(data = factor(sub_set_training$Pobre_hat2_training),
                                        reference = factor(sub_set_training$Pobre),
                                        mode = "sens_spec", positive = "1")

cm_logit_training_r2 <- cm_logit_training_r2$table

skim(sub_set_training$Pobre)


acc_training_r2 <- Accuracy(y_pred = sub_set_training$Pobre_hat2_training, y_true = sub_set_training$Pobre)
pre_training_r2 <- Precision(y_pred = sub_set_training$Pobre_hat2_training, y_true = sub_set_training$Pobre, positive = 1)
rec_training_r2 <- Recall(y_pred = sub_set_training$Pobre_hat2_training, y_true = sub_set_training$Pobre, positive = 1)
f1_training_r2 <- F1_Score(y_pred = sub_set_training$Pobre_hat2_training, y_true = sub_set_training$Pobre, positive = 1)
spf_training_r2 <- Specificity(y_pred = sub_set_training$Pobre_hat2_training, y_true = sub_set_training$Pobre, positive = 1)
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

predicciones_r1 <- prediction(sub_set_training$Pobre_hat1_training, sub_set_training$Pobre)
ROC_r1 <- performance(predicciones_r1,"tpr","fpr")
plot(ROC_r1, main = "ROC curve", col="red")
abline(a = 0, b = 1)


predicciones_r2 <- prediction(sub_set_training$Pobre_hat2_training, sub_set_training$Pobre)
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

## Test

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

sub_set_evaluating$Pobre_logit_evaluating <- predict(logit_evaluate,
                                                     newdata = sub_set_evaluating,
                                                     type = "response") # Estimación de predicciones de pobreza
summary(sub_set_evaluating$Pobre_logit_evaluating)

ggplot(data=sub_set_evaluating , mapping=aes(Pobre_1,Pobre_logit_evaluating)) + 
  geom_boxplot(aes(fill=as.factor(Pobre_1))) + theme_test() ## parece que 0.35 es el valor para evitar subestimar a los pobres


## Se prueban las mismas 2 reglas usadas en la muestra training

#Regla 1
regla1 <- 0.5 # Se define regla de Bayes

sub_set_evaluating$Pobre_hat1_evaluating <- ifelse(sub_set_evaluating$Pobre_logit_evaluating>regla1,1,0) ## Prediccion de pobreza


ggplot(sub_set_evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre?",
       x = "",
       y = "Distribución")

ggplot(sub_set_evaluating, aes(x = Pobre_hat1_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r1 <- confusionMatrix(data = factor(sub_set_evaluating$Pobre_hat1_evaluating),
                                          reference = factor(sub_set_evaluating$Pobre_1),
                                          mode = "sens_spec", positive = "1")

cm_logit_evaluating_r1 <- cm_logit_evaluating_r1$table

skim(sub_set_evaluating$Pobre_1)


acc_evaluating_r1 <- Accuracy(y_pred = sub_set_evaluating$Pobre_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1)
pre_evaluating_r1 <- Precision(y_pred = sub_set_evaluating$Pobre_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
rec_evaluating_r1 <- Recall(y_pred = sub_set_evaluating$Pobre_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
f1_evaluating_r1 <- F1_Score(y_pred = sub_set_evaluating$Pobre_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
spf_evaluating_r1 <- Specificity(y_pred = sub_set_evaluating$Pobre_hat1_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
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

sub_set_evaluating$Pobre_hat2_evaluating <- ifelse(sub_set_evaluating$Pobre_logit_evaluating>regla2,1,0) ## Prediccion de pobreza


ggplot(sub_set_evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre?",
       x = "",
       y = "Distribución")

ggplot(sub_set_evaluating, aes(x = Pobre_hat2_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_evaluating_r2 <- confusionMatrix(data = factor(sub_set_evaluating$Pobre_hat2_evaluating),
                                          reference = factor(sub_set_evaluating$Pobre_1),
                                          mode = "sens_spec", positive = "1")

cm_logit_evaluating_r2 <- cm_logit_evaluating_r2$table

skim(sub_set_evaluating$Pobre_1)


acc_evaluating_r2 <- Accuracy(y_pred = sub_set_evaluating$Pobre_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1)
pre_evaluating_r2 <- Precision(y_pred = sub_set_evaluating$Pobre_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
rec_evaluating_r2 <- Recall(y_pred = sub_set_evaluating$Pobre_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
f1_evaluating_r2 <- F1_Score(y_pred = sub_set_evaluating$Pobre_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
spf_evaluating_r2 <- Specificity(y_pred = sub_set_evaluating$Pobre_hat2_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
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

#------------------------------------------------------------------------------



  # Clasificación


regla <- 0.35 # Se define regla de predicción 

sub_set_evaluating$Pobre_hat_evaluating <- ifelse(sub_set_evaluating$Pobre_logit_evaluating>regla,1,0) ## Prediccion de pobreza


ggplot(sub_set_evaluating, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre?",
       x = "",
       y = "Distribución")

ggplot(sub_set_evaluating, aes(x = Pobre_hat_evaluating)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona pobre? - Predicción",
       x = "",
       y = "Distribución")


cm_logit_evaluating <- confusionMatrix(data = factor(sub_set_evaluating$Pobre_hat_evaluating),
                                       reference = factor(sub_set_evaluating$Pobre_1), 
                                       mode = "sens_spec", positive = "1")

cm_logit_evaluating <- cm_logit_evaluating$table

skim(sub_set_evaluating$Pobre_1)


acc_evaluating <- Accuracy(y_pred = sub_set_evaluating$Pobre_hat_evaluating, y_true = sub_set_evaluating$Pobre_1)
pre_evaluating <- Precision(y_pred = sub_set_evaluating$Pobre_hat_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
rec_evaluating <- Recall(y_pred = sub_set_evaluating$Pobre_hat_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
f1_evaluating <- F1_Score(y_pred = sub_set_evaluating$Pobre_hat_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
spf_evaluating <- Specificity(y_pred = sub_set_evaluating$Pobre_hat_evaluating, y_true = sub_set_evaluating$Pobre_1, positive = 1)
FPR_evaluating <- cm_logit_evaluating[2,1]/sum(cm_logit_evaluating[2,])
FNR_evaluating <- cm_logit_evaluating[1,2]/sum(cm_logit_evaluating[1,])

metricas_evaluating <- data.frame(Modelo = "Logit",
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_evaluating,
                                  "Precision" = pre_evaluating,
                                  "Recall" = rec_evaluating,
                                  "F1" = f1_evaluating,
                                  "Specificity" = spf_evaluating,
                                  "FPR" = FPR_evaluating,
                                  "FNR" = FNR_evaluating)



#-------------------------------------------------------------------------------

metricas <- bind_rows(metricas_training_r1, metricas_training_r2, metricas_evaluating_r1, metricas_evaluating_r2)
metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)













#------------------------------------------------------------------------------


#LM para ingreso

train_pred<-subset(training,select=c(-id,-Li,-Lp,-Pobre_1))


reg_lin<-lm(ing~.,data=train_pred)


#Modelo fuera de muestra

evaluating$ing_hat<-predict(reg_lin,subset(evaluating,select=c(-id,-Li,-Lp,-Pobre_1)))


eval_lm<-subset(evaluating,select=c(-id,-Li,-Lp,-Pobre_1 ,-ing))

evaluating$ing_hat<-predict(reg_lin,eval_lm)



evaluating$pobre_hat<-ifelse(evaluating$ing_hat<evaluating$Lp,1,0)

evaluating$pobre_hat<-factor(evaluating$pobre_hat)
evaluating$Pobre_1<-factor(evaluating$Pobre_1)

cm<-confusionMatrix(evaluating$pobre_hat,evaluating$Pobre_1)


resultados<-data.frame(Modelo="LM",Base="Predicción",
                       Accuracy=cm$overall[1],
                       Sensitivity=cm$byClass[1],
                       Specificity=cm$byClass[2])

resultados


#------------------------------------------------------------------------------

# Decision tree


  # Clasificación

training$Pobre_1<-as.factor(training$Pobre_1)
train_clas<-subset(training,select=c(-id,-Li,-Lp,-ing))


Control=trainControl(method= "repeatedcv",number=3,repeats=3,
                     summaryFunction=multiClassSummary)

arbol=caret::train(Pobre_1~.,data=train_clas,method="rpart",trControl=Control,tuneLength=10)

arbol

evaluating$pobre_hat_arbol<-predict(arbol,subset(evaluating,select=-c(id,Li,Lp,Pobre_1,ing)))
                         
cm_arbol<-confusionMatrix(evaluating$pobre_hat_arbol,reference = evaluating$Pobre_1)
cm_arbol

resultados2<-data.frame(Modelo="Arbol",Base="Clasificación",
                       Accuracy=cm_arbol$overall[1],
                       Sensitivity=cm_arbol$byClass[1],
                       Specificity=cm_arbol$byClass[2])

resultados_general<-rbind(resultados,resultados2)
resultados_general







#------------------------------------------------------------------------------

  # XGBoost para Clasificación

p_load("ranger",install=T)


tr_m<-subset(training,select=c(-id,-Li,-Lp,-ing))
ev_m<-subset(evaluating,select=c(-id,-Li,-Lp,-ing))

tr_m$Pobre_1<-as.factor(tr_m$Pobre_1)
ev_m$Pobre_1<-as.factor(ev_m$Pobre_1)

  #RF
grid_default <- expand.grid(nrounds = c(100,200),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.1,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl<- trainControl(method = "cv",
                            number = 2,
                            summaryFunction = fiveStats,
                            verbose=T)


xgtree <- train(Pobre_1~.,data=tr_m,method="xgbTree",trControl=ctrl,metric="Sens",
                tuneGrid=grid_default)



ev_m$pred_arbol <-predict(xgtree,newdata=ev_m)

cm_xg <- confusionMatrix(ev_m$pred_arbol,ev_m$Pobre_1)


resultados_xg<-data.frame(Modelo="XGBoost",Base="Clasificación",
                        Accuracy=cm_xg$overall[1],
                        Sensitivity=cm_xg$byClass[1],
                        Specificity=cm_xg$byClass[2])

resultados_xg






#------------------------------------------------------------------------------
  #Logit con todas las variables

train_clas<-subset(training,select=c(-id,-Li,-Lp,-ing))
eval_<-subset(evaluating,select=c(-id,-Li,-Lp))



train_clas$Pobre_1<-as.factor(train_clas$Pobre_1)

log_todo<-glm(Pobre_1~. , family=binomial(link="logit") , data=train_clas)

evaluating$pobre_logit_hat_todo<-predict(log_todo,eval_)


evaluating$pobre_logit_hat_todo<-factor(evaluating$pobre_logit_hat_todo)
evaluating$Pobre_1<-factor(evaluating$Pobre_1)

cm<-confusionMatrix(evaluating$pobre_logit_hat_todo,evaluating$Pobre_1)


resultados3<-data.frame(Modelo="Logit_entero",Base="Clasificación",
                       Accuracy=cm$overall[1],
                       Sensitivity=cm$byClass[1],
                       Specificity=cm$byClass[2])

resultados3

resultados_general<-rbind(resultados,resultados2,resultados3)
resultados_general


