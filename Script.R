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
#setwd("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty")
#hogares <- readRDS("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty/data/train_hogares.Rds")
#personas <- readRDS("C:/Users/caaya/OneDrive - Universidad de los Andes/universidad/8 semestre/BigData/Problem sets/Problem set 2/Predicting-Poverty/data/train_personas.Rds")

#Federico


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

base_completa<-subset(base_completa,select=-Dominio)
  #Dropeamos dominio que es lo mismo que Depto

(base_completa)

rm(hogares,personas)
  #Borramos las bases que no utilizamos

#------------------------------------------------------------------------------

# Borramos las variables que no están en ambas bases de datos (base_completa vs train)


hogares <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/test_hogares.Rds")
personas <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/test_personas.Rds")
  #Cargamos las bases train

test <-personas %>%  left_join(hogares) 
  #Pegamos las bases train

test<-subset(test,select=-Dominio)
  #Dominio no sirve porque es igual a depto


rm(hogares,personas)
  #Borramos las bases individuales

columnas_test<-c(names(base_test))
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


skim(training$Pobre)


#------------------------------------------------------------------------------

# Implementación de modelos


# Random Forest (Mateo)






