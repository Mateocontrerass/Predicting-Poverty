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
       rattle,doParallel)
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

colnames(base_completa)

base_completa<-subset(base_completa,select=c(-Dominio,-Orden))
  #Dropeamos dominio que es lo mismo que Depto

(base_completa)

rm(hogares,personas)
  #Borramos las bases que no utilizamos

objeto<-skim(base_completa)
indices_chiquitos<-which(objeto$complete_rate>0.5)
base_completa<-base_completa[,indices_chiquitos]
  #Acá hago el skim para saber %NAN, elimino las variables con %NAN>0.5
  

#------------------------------------------------------------------------------

# Borramos las variables que no están en ambas bases de datos (base_completa vs train)


hogares <- readRDS("data/test_hogares.Rds")
personas <- readRDS("data/test_personas.Rds")


  #Cargamos las bases test

test <-personas %>%  left_join(hogares) 
  #Pegamos las bases train

test<-subset(test,select=c(-Dominio,-Orden))
  #Dominio no sirve porque es igual a depto


rm(hogares,personas)
  #Borramos las bases individuales

columnas_test<-c(names(test))
columnas_base<-c(names(base_completa))

  #Sacar las columnas presentes en en los DF


  columnas_total<-intersect(columnas_base,columnas_test)
  #Intersección entre variables comunes
  columnas_total
  
  remover <- c("Pobre")
  columnas<-append(columnas_total,"Pobre")
  #Agregamos pobre a la base total
  
  test<-test[,columnas_total]
  #Para que test tenga las mismas variables
  
  rm(objeto,columnas_base,columnas_total,v,variables_categoricas,indices_chiquitos,
     filtro)
  

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
#Base de evaluación de hiperparametros

#No hace falta hacer la partición para la muestra test dado que el ejercicio ya la da.

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





  # Sugiero descartar las variables que tienen una tasa de completado <0.5 
  # porque considero que pueden hacer mucho más ruido del que aportan dado que 
  # me gustaria imputar los NAN a ver que tal. 
#------------------------------------------------------------------------------





#------------------------------------------------------------------------------

# Implementación de modelos

#Elastic net Federico


#------------------------------------------------------------------------------

# Random Forest (Mateo)

  # Modelo basico de clasificación sin imputación
  
modelo1 <- decision_tree(mode="classification") 
  #Establecimiento del modelo

training$Pobre<-factor(training$Pobre)
evaluating$Pobre<-factor(evaluating$Pobre)


modelo1_fit <- fit(modelo1, Pobre ~ . -id, data = training)

modelo1_fit


#------------------------------------------------------------------------------



