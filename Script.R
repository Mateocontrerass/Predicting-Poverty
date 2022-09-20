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
p_load(tidyverse,dplyr,here,skimr,tidyr)
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
df <-personas %>%  left_join(hogares)
  #Unión de ambas bases.


#------------------------------------------------------------------------------

# Generando muestra train, evaluate and test

## 75% of the sample size
smp_train_size <- floor(0.7 * nrow(df)) ## cambiar el 0.75 por tamaño de muestra train

## set the seed to make your partition reproducible
set.seed(10101)

train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

smp_eva_size <- floor(0.2 * nrow(df)) ## cambiar el 0.75 por tamaño de muestra eval
smp_test_size <- floor(0.1 * nrow(df)) ## cambiar el 0.75 por tamaño de muestra train



