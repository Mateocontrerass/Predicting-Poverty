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
       rattle,doParallel,mixgb, install = TRUE)
library(tidyverse)
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
  #Descriptivas
  
  p_load(summarytools,install=T)
  
  library(kableExtra)
  library(magrittr)
  
  
  #names(base_completa)
  #P6020,P6040,P6210s1,Pobre,ing
  
  base_completa$Pobre<-factor(base_completa$Pobre,levels=c(0,1),labels=c("No","Si"))
  
  
  prueba<-subset(subset(base_completa,as.numeric(base_completa$P6210s1)<15))
  as.num
  
  p <- subset(subset(base_completa,as.numeric(base_completa$P6210s1)<15)) %>%
    ggplot( aes(x=P6210s1, fill=Pobre)) +
    geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    labs(fill="")+labs(x="Años de educación",y="Total",title="Total de años de educación \n entre personas pobres y no pobres")
  
  
  p #Grafica conteo años de educación
  ggsave("Views/Pobreza_educacion.jpg",plot=p)
  
   p_load(crosstable,install=T)
   p_load(officer,install=T)
  
   
   #Tablas
   base_completa<-base_completa %>% 
     rename(
         Sexo=P6020)
   
   
   base_completa$P6020<-factor(base_completa$P6020,levels=c(1,2),labels=c("Hombre","Mujer"))
   
   ct1 = crosstable(base_completa, c(Sexo), by=Pobre, total="both", 
                    percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%
     as_flextable() %>% (path="Views/sexo_pobre.docx")
   
   ct1
   
    #No supe guardar este objeto asi que lo hice manual.
   
   
   base_completa<-base_completa %>% 
     rename(
         P6020=Sexo)  
   
   
   
  #//
   
   g <-subset(subset(base_completa,as.numeric(base_completa$Nper)<13)) %>%
     ggplot(aes(x=Nper, fill=Pobre)) +
     geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge') +
     scale_fill_manual(values=c("#69b3a2", "#404080")) +
     labs(fill="")+labs(x="Cantidad de personas por hogar",y="Conteo de hogares",title="Personas por \n hogar discriminado entre pobres y no pobres")+scale_x_continuous(breaks = seq(0, 12,1))

   g
   ggsave("Views/nper_pobreza.jpg",plot=g)
   
# //
   
     
   base_completa$P6100<-factor(base_completa$P6100,levels=c(1,2,3,9),labels=c("Contributivo","Especial","Subsidiado","No_sabe"))
   
   h <-subset(subset(base_completa,as.numeric(base_completa$P6100)<4)) %>%
     ggplot(aes(x=P6100, fill=Pobre)) +
     geom_bar( color="#e9ecef", alpha=0.6, position = 'dodge') +
     scale_fill_manual(values=c("#69b3a2", "#404080"))+
     labs(x="",y="Conteo de hogares",title="Regimen de salud afiliado")
   
   h
   ggsave("Views/salud_pobreza.jpg",plot=h)
   
   
   
   
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
  set.seed(666)
    x<- subset(training, select = c(-Li,-Lp,-id,-Pobre_1, -ing))
    y <- training$ing
    
    modelo_lasso <-glmnet(
      x = newxt,
      y,
      alpha = 1,
      nlambda = ,
      standardize = T
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
  
    # Cada predicci?n se va a evaluar
    set.seed(666)
    
    newxt<-data.matrix(subset(training, select = c(-Li,-Lp,-id,-Pobre_1, -ing)))
    modelo_lasso_cv <-cv.glmnet(
      x = newxt,
      y,
      alpha = 1,
      nlambda = ,
      nfold = 10,
      standardize = T
    )
    plot(modelo_lasso_cv)
  
    
    # Guardamos el mejor Lasso
    mejor_lambda_lasso = modelo_lasso_cv$lambda.min
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
    
    cm_lasso
    
    resultados_l<-data.frame(Modelo="Lasso",Base="Predicción",
                           Accuracy=cm_lasso$overall[1],
                           Sensitivity=cm_lasso$byClass[1],
                           Specificity=cm_lasso$byClass[2])
    
    resultados_l
  
  #Ridge 
  modelo_ridge <- glmnet(
    x ,
    y ,
    alpha = 0,
    nlambda = ,
    standardize = T
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
  
  #eavaluamos el mejor lambda
  set.seed(666)
  
  modelo_ridge_cv <-cv.glmnet(
    x = newxt,
    y,
    alpha = 0,
    nlambda = ,
    nfold = 10,
    standardize = T
  )
  plot(modelo_lasso_cv)
  
  
  # Guardamos el mejor Ridge
  mejor_lambda_ridge = modelo_ridge_cv$lambda.min
  #predicción con la base de datos training
 
   y_hat_in2 <- predict.glmnet(modelo_ridge,
                              newxt,
                              s = mejor_lambda_ridge)
  
  #predicción con la base de datos test
  y_hat_out2 <- predict.glmnet(modelo_ridge,
                               newx,
                               s = mejor_lambda_ridge)
  
  #evaluamos los resultados
  resultados_ridge_1 <-predict(modelo_ridge, newxt, s = mejor_lambda_ridge)
  
  resultados_ridge_2 <-predict(modelo_ridge, newx, s = mejor_lambda_ridge)
  
  pobre_hat_r<-ifelse(resultados_ridge_2<evaluating$Lp,1,0)
  plot(pobre_hat_r)
  
  pobre_hat_r<-factor(pobre_hat_r)
  Pobre_1<-factor(evaluating$Pobre_1)
  
  cm_ridge<-confusionMatrix(pobre_hat_r, Pobre_1)
  
  
  resultados_r<-data.frame(Modelo="ridge",Base="Predicción",
                         Accuracy=cm_ridge$overall[1],
                         Sensitivity=cm_ridge$byClass[1],
                         Specificity=cm_ridge$byClass[2])
  resultados_r
  
  ##Elastic net 
 
  
  modelo_elastic <- glmnet(
    x ,
    y ,
   alpha = 0.5,
    nlambda = ,
    standardize = T
  )
  
  # Analicemos cómo cambian los coeficientes para diferentes lambdas
  regularizacion3 <- modelo_elastic$beta %>% 
    as.matrix() %>%
    t() %>% 
    as_tibble() %>%
    mutate(lambda = modelo_elastic$lambda)
  
  regularizacion3 <- regularizacion3 %>%
    pivot_longer(
      cols = !lambda, 
      names_to = "predictor",
      values_to = "coeficientes"
    )
  
  regularizacion3 %>%
    ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
    geom_line() +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10",
                                    scales::math_format(10^.x))
    ) +
    labs(title = "Coeficientes del modelo en función de la regularización (elastic)", x = "Lambda", y = "Coeficientes") +
    theme_bw() +
    theme(legend.position="bottom")
  
  
  
  #obtener el lambda óptimo
  set.seed(666)
  modelo_elastic_cv<-cv.glmnet(
    x=newxt,
    y,
    alpha = 0.5,
    nfolds = 10,
    standardize = T
   
  )
  
  plot(modelo_elastic_cv)
  
  mejor_lambda_elastic = modelo_elastic_cv$lambda.min
  
  
  resultados_elastic_1 <-predict(modelo_elastic, newxt, s =  mejor_lambda_elastic)

  resultados_elastic_2 <-predict(modelo_elastic, newx, s =  mejor_lambda_elastic)
  
  pobre_hat_elastic<-ifelse(resultados_elastic_2<evaluating$Lp,1,0)
  plot(pobre_hat_elastic)
  
  pobre_hat_elastic<-factor(pobre_hat_elastic)
  Pobre_1<-factor(Pobre_1)
  
  cm_elastic<-confusionMatrix(pobre_hat_elastic,Pobre_1)
  
  
  resultados_e<-data.frame(Modelo="elastic net",Base="Predicción",
                         Accuracy=cm_elastic$overall[1],
                         Sensitivity=cm_elastic$byClass[1],
                         Specificity=cm_elastic$byClass[2])
  resultados_e
  
  
  resultados_regularizacion<-rbind(resultados_r,resultados_l, resultados_e)
  resultados_regularizacion
  
  ##XGBoost 
  #install.packages("xgboost")
  
  library(xgboost)
  newxgt <- x %>% 
    as.matrix() %>% 
    xgb.DMatrix(data = .,label = y)
  
  newxg <- newx%>% 
    xgb.DMatrix(data = ., label = evaluating$ing)
  
 params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.3, gamma=0, max_depth=6, subsample=1,colsample_bytree=1)
 set.seed(666)
  modelo_xgb <- xgb.cv(data = newxgt, params = params,
                        nrounds = 300, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F) 
  
  modelo_xgb
 pred_xgb <-predict (modelo_xgb, newxg)
  
  
#------------------------------------------------------------------------------

  #LM para ingreso

train_pred<-subset(training,select=c(-id,-Li,-Lp,-Pobre_1))


reg_lin<-lm(ing~.,data=train_pred)
summary(reg_lin)


y_out_lm<-predict(reg_lin,evaluating)



#===============================================================================

## Carlos

#-------------------------------------------------------------------------------

### Logit - Corrección de imbalance

#-------------------------------------------------------------------------------

load("data/data_imputada2")
load("data/evaluating")

set.seed(666)

### train

training <- data_rf_train
rm(data_rf_train)

skim(training)

training <- subset(training, select = -c(id, Li, Lp, ing))

variables_numericas <- c("P6040", "P6426", "P5000",
                         "P5010", "P5130", "Nper", "Npersug")
escalador <- preProcess(training[, variables_numericas])
train_s <- training
test_s <- evaluating
train_s[, variables_numericas] <- predict(escalador, training[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, evaluating[, variables_numericas])

training <- train_s
evaluating <- test_s

rm(escalador, train_s, test_s)

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

## Remuestreo - SMOTE

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

Pobre_1_hat_oversampling <- ifelse(Pobre_1_logit_oversampling>regla1,1,0) ## Prediccion de Pobreza


ggplot(train_oversampling, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(train_oversampling, aes(x = Pobre_1_hat_oversampling)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_oversampling <- confusionMatrix(data = factor(Pobre_1_hat_oversampling),
                                         reference = factor(train_oversampling$Pobre_1),
                                         mode = "sens_spec", positive = "1")

cm_logit_oversampling <- cm_logit_oversampling$table

skim(train_oversampling$Pobre_1)


acc_oversampling <- Accuracy(y_pred = Pobre_1_hat_oversampling, y_true = train_oversampling$Pobre_1)
pre_oversampling <- Precision(y_pred = Pobre_1_hat_oversampling, y_true = train_oversampling$Pobre_1, positive = 1)
rec_oversampling <- Recall(y_pred = Pobre_1_hat_oversampling, y_true = train_oversampling$Pobre_1, positive = 1)
f1_oversampling <- F1_Score(y_pred = Pobre_1_hat_oversampling, y_true = train_oversampling$Pobre_1, positive = 1)
spf_oversampling <- Specificity(y_pred = Pobre_1_hat_oversampling, y_true = train_oversampling$Pobre_1, positive = 1)
FPR_oversampling <- cm_logit_oversampling[2,1]/sum(cm_logit_oversampling[2,])
FNR_oversampling <- cm_logit_oversampling[1,2]/sum(cm_logit_oversampling[1,])

metricas_oversampling <- data.frame(Modelo = "Logit - correccion de imbalance",
                                    "Muestreo" = "SMOTE", 
                                    "Evaluación" = "Dentro de muestra",
                                    "Accuracy" = acc_oversampling,
                                    "Precision" = pre_oversampling,
                                    "Recall" = rec_oversampling,
                                    "F1" = f1_oversampling,
                                    "Specificity" = spf_oversampling,
                                    "FPR" = FPR_oversampling,
                                    "FNR" = FNR_oversampling)

## Curva de ROC

predicciones_oversampling <- prediction(Pobre_1_hat_oversampling, train_oversampling$Pobre_1)
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

Pobre_1_hat_oversampling_evaluate <- ifelse(Pobre_1_logit_oversampling_evaluate>regla1,1,0) ## Prediccion de Pobre_1za

cm_logit_oversampling_evaluate <- confusionMatrix(data = factor(Pobre_1_hat_oversampling_evaluate),
                                                  reference = factor(evaluating$Pobre_1),
                                                  mode = "sens_spec", positive = "1")

cm_logit_oversampling_evaluate <- cm_logit_oversampling_evaluate$table

skim(evaluating$Pobre_1)


acc_oversampling_evaluate <- Accuracy(y_pred = Pobre_1_hat_oversampling_evaluate, y_true = evaluating$Pobre_1)
pre_oversampling_evaluate <- Precision(y_pred = Pobre_1_hat_oversampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
rec_oversampling_evaluate <- Recall(y_pred = Pobre_1_hat_oversampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
f1_oversampling_evaluate <- F1_Score(y_pred = Pobre_1_hat_oversampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
spf_oversampling_evaluate <- Specificity(y_pred = Pobre_1_hat_oversampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
FPR_oversampling_evaluate <- cm_logit_oversampling_evaluate[2,1]/sum(cm_logit_oversampling_evaluate[2,])
FNR_oversampling_evaluate <- cm_logit_oversampling_evaluate[1,2]/sum(cm_logit_oversampling_evaluate[1,])

metricas_oversampling_evaluate <- data.frame(Modelo = "Logit - correcion imbalance",
                                             "Muestreo" = "SMOTE", 
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

Pobre_1_hat_undersampling <- ifelse(Pobre_1_logit_undersampling>regla1,1,0) ## Prediccion de Pobreza


ggplot(train_undersampling, aes(x = Pobre_1)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1?",
       x = "",
       y = "Distribución")

ggplot(train_undersampling, aes(x = Pobre_1_hat_undersampling)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "¿Es la persona Pobre_1? - Predicción",
       x = "",
       y = "Distribución")

cm_logit_undersampling <- confusionMatrix(data = factor(Pobre_1_hat_undersampling),
                                          reference = factor(train_undersampling$Pobre_1),
                                          mode = "sens_spec", positive = "1")

cm_logit_undersampling <- cm_logit_undersampling$table

skim(train_undersampling$Pobre_1)


acc_undersampling <- Accuracy(y_pred = Pobre_1_hat_undersampling, y_true = train_undersampling$Pobre_1)
pre_undersampling <- Precision(y_pred = Pobre_1_hat_undersampling, y_true = train_undersampling$Pobre_1, positive = 1)
rec_undersampling <- Recall(y_pred = Pobre_1_hat_undersampling, y_true = train_undersampling$Pobre_1, positive = 1)
f1_undersampling <- F1_Score(y_pred = Pobre_1_hat_undersampling, y_true = train_undersampling$Pobre_1, positive = 1)
spf_undersampling <- Specificity(y_pred = Pobre_1_hat_undersampling, y_true = train_undersampling$Pobre_1, positive = 1)
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

predicciones_undersampling <- prediction(Pobre_1_hat_undersampling, train_undersampling$Pobre_1)
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

Pobre_1_hat_undersampling_evaluate <- ifelse(Pobre_1_logit_undersampling_evaluate>regla1,1,0) ## Prediccion de Pobre_1za

cm_logit_undersampling_evaluate <- confusionMatrix(data = factor(Pobre_1_hat_undersampling_evaluate),
                                                   reference = factor(evaluating$Pobre_1),
                                                   mode = "sens_spec", positive = "1")

cm_logit_undersampling_evaluate <- cm_logit_undersampling_evaluate$table

skim(evaluating$Pobre_1)


acc_undersampling_evaluate <- Accuracy(y_pred = Pobre_1_hat_undersampling_evaluate, y_true = evaluating$Pobre_1)
pre_undersampling_evaluate <- Precision(y_pred = Pobre_1_hat_undersampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
rec_undersampling_evaluate <- Recall(y_pred = Pobre_1_hat_undersampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
f1_undersampling_evaluate <- F1_Score(y_pred = Pobre_1_hat_undersampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
spf_undersampling_evaluate <- Specificity(y_pred = Pobre_1_hat_undersampling_evaluate, y_true = evaluating$Pobre_1, positive = 1)
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

thresholds <- seq(0.1, 0.9, length.out = 100)
opt_t <- data.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(Pobre_1_logit_training > t)
  f1_t <- F1_Score(y_true = training$Pobre_1, y_pred = y_pred_t,
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

regla <- mejor_t

Pobre_1_hat_threshold_op_training <- ifelse(Pobre_1_logit_training>regla,1,0) ## Prediccion de Pobre_1za

cm_logit_threshold_op_training <- confusionMatrix(data = factor(Pobre_1_hat_threshold_op_training),
                                                  reference = factor(training$Pobre_1),
                                                  mode = "sens_spec", positive = "1")

cm_logit_threshold_op_training <- cm_logit_threshold_op_training$table


acc_threshold_op_training <- Accuracy(y_pred = Pobre_1_hat_threshold_op_training, y_true = training$Pobre_1)
pre_threshold_op_training <- Precision(y_pred = Pobre_1_hat_threshold_op_training, y_true = training$Pobre_1, positive = 1)
rec_threshold_op_training <- Recall(y_pred = Pobre_1_hat_threshold_op_training, y_true = training$Pobre_1, positive = 1)
f1_threshold_op_training <- F1_Score(y_pred = Pobre_1_hat_threshold_op_training, y_true = training$Pobre_1, positive = 1)
spf_threshold_op_training <- Specificity(y_pred = Pobre_1_hat_threshold_op_training, y_true = training$Pobre_1, positive = 1)
FPR_threshold_op_training <- cm_logit_threshold_op_training[2,1]/sum(cm_logit_threshold_op_training[2,])
FNR_threshold_op_training <- cm_logit_threshold_op_training[1,2]/sum(cm_logit_threshold_op_training[1,])

metricas_threshold_op_training <- data.frame(Modelo = "Logit - umbral optimo",
                                             "Muestreo" = NA, 
                                             "Evaluación" = "Fuera de muestra",
                                             "Accuracy" = acc_threshold_op_training,
                                             "Precision" = pre_threshold_op_training,
                                             "Recall" = rec_threshold_op_training,
                                             "F1" = f1_threshold_op_training,
                                             "Specificity" = spf_threshold_op_training,
                                             "FPR" = FPR_threshold_op_training,
                                             "FNR" = FNR_threshold_op_training)



Pobre_1_hat_threshold_op_evaluating <- ifelse(Pobre_1_logit_evaluating>regla,1,0) ## Prediccion de Pobre_1za

cm_logit_threshold_op_evaluating <- confusionMatrix(data = factor(Pobre_1_hat_threshold_op_evaluating),
                                                    reference = factor(evaluating$Pobre_1),
                                                    mode = "sens_spec", positive = "1")

cm_logit_threshold_op_evaluating <- cm_logit_threshold_op_evaluating$table


acc_threshold_op_evaluating <- Accuracy(y_pred = Pobre_1_hat_threshold_op_evaluating, y_true = evaluating$Pobre_1)
pre_threshold_op_evaluating <- Precision(y_pred = Pobre_1_hat_threshold_op_evaluating, y_true = evaluating$Pobre_1, positive = 1)
rec_threshold_op_evaluating <- Recall(y_pred = Pobre_1_hat_threshold_op_evaluating, y_true = evaluating$Pobre_1, positive = 1)
f1_threshold_op_evaluating <- F1_Score(y_pred = Pobre_1_hat_threshold_op_evaluating, y_true = evaluating$Pobre_1, positive = 1)
spf_threshold_op_evaluating <- Specificity(y_pred = Pobre_1_hat_threshold_op_evaluating, y_true = evaluating$Pobre_1, positive = 1)
FPR_threshold_op_evaluating <- cm_logit_threshold_op_evaluating[2,1]/sum(cm_logit_threshold_op_evaluating[2,])
FNR_threshold_op_evaluating <- cm_logit_threshold_op_evaluating[1,2]/sum(cm_logit_threshold_op_evaluating[1,])

metricas_threshold_op_evaluating <- data.frame(Modelo = "Logit - umbral optimo",
                                               "Muestreo" = NA, 
                                               "Evaluación" = "Fuera de muestra",
                                               "Accuracy" = acc_threshold_op_evaluating,
                                               "Precision" = pre_threshold_op_evaluating,
                                               "Recall" = rec_threshold_op_evaluating,
                                               "F1" = f1_threshold_op_evaluating,
                                               "Specificity" = spf_threshold_op_evaluating,
                                               "FPR" = FPR_threshold_op_evaluating,
                                               "FNR" = FNR_threshold_op_evaluating)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Tabla de metricas

metricas <- bind_rows(metricas_training_r1, metricas_evaluating_r1, metricas_training_r2,
                      metricas_evaluating_r2, metricas_oversampling, metricas_oversampling_evaluate,
                      metricas_undersampling, metricas_undersampling_evaluate,
                      metricas_threshold_op_training, metricas_threshold_op_evaluating)

metricas1 <- bind_rows(metricas_evaluating_r1, metricas_evaluating_r2, metricas_threshold_op_evaluating,
                       metricas_oversampling_evaluate, metricas_undersampling_evaluate)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

metricas1 %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------















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
cm

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


