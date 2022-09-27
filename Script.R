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

colnames(base_completa)

base_completa<-subset(base_completa,select=c(-Dominio,-Orden,-Fex_c,-Fex_dpto,-Npobres,-Nindigentes))
  #Dropeamos dominio que es lo mismo que Depto


rm(hogares,personas)
  #Borramos las bases que no utilizamos

objeto<-skim(base_completa)
  #Guardamos el skim para sacar el complete_rate
indices_chiquitos<-which(objeto$complete_rate>0.5)
  #Indices de complete rate>0.5
base_completa<-base_completa[,indices_chiquitos]
  #Implementamos estas variables

  

#------------------------------------------------------------------------------

# Borramos las variables que no están en ambas bases de datos (base_completa vs train)


hogares <- readRDS("data/test_hogares.Rds")
personas <- readRDS("data/test_personas.Rds")


  #Cargamos las bases test

test <-personas %>%  left_join(hogares) 
  #Pegamos las bases train

test<-subset(test,select=c(-Dominio,-Orden,-Fex_c,-Fex_dpto,-Npobres,-Nindigentes))
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

  base_completa<-base_completa[,columnas]
  #Depuramos la base para que tenga las mismas columnas que test.
  #Si no se tienen las mismas features falla el modelo.
  
  rm(objeto,columnas_base,columnas_total,v,variables_categoricas,indices_chiquitos,
     filtro)
  

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
  
  

  
  variables_categoricas <- c("Depto","P6020","P6050","P6090","P6100",
                             "P6210","P6210s1","P6240","Oficio",
                             "P7500s2","P7500s3","P7505","P7510s1",
                             "P7510s2","P7510s3","P7510s5","P7510s6",
                             "P7510s7",
                             "Pet","Oc","P5090","Clase","Pobre")
  

  c(names(base_completa))
    
  base_completa[,variables_categoricas]<-lapply(base_completa[,variables_categoricas],factor)
  
  skim(base_completa)
  
  filtro<-base_completa$Clase==1
  base_completa$Clase[filtro]<-0
  
  filtro<-base_completa$Clase==2
  base_completa$Clase[filtro]<-1
  
  unique(base_completa$Clase)

  #Volver 1 y 0 la variable Clase.

  base_completa$Clase<-as.factor(base_completa$Clase)




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

rm(filtro,hola,v,variables_categoricas)

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

  # NOTAS IMPORTANTES

  # No utilizar las siguientes variables para la estimación: 
  # ID / Li / Lp 

skim(training)

unique(training$Clase)


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

# Random Forest (Mateo)

  # Modelo basico de clasificación sin imputación
  
modelo1 <- decision_tree(mode="classification") 
  #Establecimiento del modelo

training$Pobre<-factor(training$Pobre)
evaluating$Pobre<-factor(evaluating$Pobre)


modelo1_fit <- fit(modelo1, Pobre ~ . -id, data = training)

modelo1_fit

training$Pobre
testing$pobre

  # Random Forest para clasificación


#------------------------------------------------------------------------------



