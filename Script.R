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
setwd("~/Programacion/BDML/Predicting-Poverty")
train_hogares <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/train_hogares.Rds")
train_personas <- readRDS("~/Programacion/BDML/Predicting-Poverty/Stores/data/train_personas.Rds")

#------------------------------------------------------------------------------
#  Analísis exploratorio de ambas bases por separado.
colnames(train_hogares)
skim(train_hogares)


colnames(train_personas)
  # id presente en ambas tablas






#------------------------------------------------------------------------------
# Unir las bases
train <-train_personas %>%  left_join(train_hogares)
  #Unión de ambas bases.