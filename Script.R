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
p_load(tidyverse,dplyr,here,skimr,tidyr,gamlr,modelsummary,caret,rio)
library(tidyverse)


#------------------------------------------------------------------------------
# Cargar las bases de datos

#Mateo
setwd("~/Programacion/BDML/Predicting-Poverty")
hogares <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/train_hogares.Rds")
personas <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/train_personas.Rds")

#Carlos
setwd("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty")
hogares <- readRDS("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty/data/train_hogares.Rds")
personas <- readRDS("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty/data/train_personas.Rds")

#Federico


#------------------------------------------------------------------------------
#  Analísis exploratorio de ambas bases por separado.
colnames(hogares)
skim(hogares)


colnames(personas)
  # id presente en ambas tablas


#------------------------------------------------------------------------------
# Unir las bases
base_completa <-personas %>%  left_join(hogares)
  #Unión de ambas bases.


rm(hogares,personas)
  #Borramos las bases que no utilizamos

#------------------------------------------------------------------------------

# Borramos las variables que no están en ambas bases de datos (base_completa vs train)


hogares <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/test_hogares.Rds")
personas <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/test_personas.Rds")
  #Cargamos las bases train

base_test <-personas %>%  left_join(hogares)
  #Pegamos las bases train

rm(hogares,personas)
  #Borramos las bases individuales

columnas_test<-c(names(base_test))
  #Sacar las columnas presentes en el df: test

remover <- c("Pobre")

columnas<-append(columnas_test,"Pobre")

  



base_completa<-base_completa[,columnas]
  #Depuramos la base para que tenga las mismas columnas que test.
  #Si no se tienen las mismas features falla el modelo.

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

# Implementación de modelos



  






