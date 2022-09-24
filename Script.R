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
#  Anal铆sis exploratorio de ambas bases por separado.
colnames(hogares)
  #skim(hogares)


colnames(personas)
  # id presente en ambas tablas


#------------------------------------------------------------------------------
# Unir las bases
base_completa <-personas %>%  left_join(hogares)
  #Uni贸n de ambas bases.

colnames(base_completa)

base_completa<-subset(base_completa,select=c(-Dominio,-Orden))
  #Dropeamos dominio que es lo mismo que Depto

(base_completa)

rm(hogares,personas)
  #Borramos las bases que no utilizamos

#------------------------------------------------------------------------------

# Borramos las variables que no est谩n en ambas bases de datos (base_completa vs train)



hogares <- readRDS("data/test_hogares.Rds")
personas <- readRDS("data/test_personas.Rds")

test <-personas %>%  left_join(hogares) 
  #Pegamos las bases train

test<-subset(test,select=c(-Dominio,-Orden))
  #Dominio no sirve porque es igual a depto


rm(hogares,personas)
  #Borramos las bases individuales

columnas_test<-c(names(test))
  #Sacar las columnas presentes en el df: test

remover <- c("Pobre")

columnas<-append(columnas_test,"Pobre")

#------------------------------------------------------------------------------
# factores para base completa

base_completa<-base_completa[,columnas]
  #Depuramos la base para que tenga las mismas columnas que test.
  #Si no se tienen las mismas features falla el modelo.

  variables_categoricas <- c("Depto")
  
  for (v in variables_categoricas){ base_completa[, v] <- as.factor(base_completa[, v, drop = T])}
  
  
  filtro<-base_completa$Clase==1
  base_completa$Clase[filtro]<-0
  
  filtro<-base_completa$Clase==2
  base_completa$Clase[filtro]<-1
  
  unique(base_completa$Clase)

  #Volver 1 y 0 la variable Clase.

is.factor(base_completa$Depto)

#Variables efectivamente categoricas

base_completa$Clase<-factor(base_completa$Clase)
is.factor(base_completa$Clase)
#Factor tambien la variable clase.



#------------------------------------------------------------------------------
#Factor para variables de df test
variables_categoricas <- c("Depto")

for (v in variables_categoricas){ test[, v] <- as.factor(test[, v, drop = T])}


filtro<-test$Clase==1
test$Clase[filtro]<-0

filtro<-test$Clase==2
test$Clase[filtro]<-1

unique(test$Clase)

#Volver 1 y 0 la variable Clase.

is.factor(test$Depto)

#Variables efectivamente categoricas

test$Clase<-factor(test$Clase)
is.factor(test$Clase)
#Factor tambien la variable clase.

#------------------------------------------------------------------------------


# Generando muestra train y evaluate 

prop.table(table(base_completa$Pobre))
#74% personas no pobres y 25% de personas pobres.

set.seed(666)

split1 <- createDataPartition(base_completa$Pobre , p = 0.7)[[1]]

training = base_completa[split1,]
#Base de entrenamiento con 70% de los datos

evaluating = base_completa[-split1,]
#Base de evaluaci贸n de hiperparametros

#No hace falta hacer la partici贸n para la muestra test dado que el ejercicio ya la da.

rm(base_completa)
rm(columnas,columnas_test,remover,split1)
  #Para mantener el espacio de trabajo limpio



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


skim(training)
  # Sugiero descartar las variables que tienen una tasa de completado <0.6 
  # porque considero que pueden hacer mucho m谩s ruido del que aportan dado que 
  # me gustaria imputar los NAN a ver que tal. 






#------------------------------------------------------------------------------

# Implementaci贸n de modelos

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

# Analicemos c髆o cambian los coeficientes para diferentes lambdas
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
  labs(title = "Coeficientes del modelo en funci髇 de la regularizaci髇 (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")

newx<-data.matrix(select(evaluating,c(-id, -Pobre)))
predicciones_lasso <- predict(modelo_lasso, 
                              newx )
lambdas_lasso <- modelo_lasso$lambda

# Cada predicci髇 se va a evaluar
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



# Random Forest (Mateo)

  # Modelo basico de clasificaci贸n sin imputaci贸n
  
modelo1 <- decision_tree(mode="classification") 
  #Establecimiento del modelo

training$Pobre<-factor(training$Pobre)
evaluating$Pobre<-factor(evaluating$Pobre)


modelo1_fit <- fit(modelo1, Pobre ~ . -id, data = training)

modelo1_fit

