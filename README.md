# Predicting poverty

## Motivación
Este proyecto se desarrolla como respuesta al problem set 2 de la clase Big data y Machine Learning. La motivación del trabajo investigativo surge en la necesidad de determinar si un hogar se encuentra en situación de pobreza de manera más rapida y barata que ejecutando las recurrentes encuestas con las que se obtiene esta información. Al final, todo este esfuerzo hace parte de facilitar intervenciones de politica publica más eficientes, mejorando el impacto y el costo/beneficio de estas estrategias.

## Estructura del repositorio

El repositorio cuenta con 4 carpetas principales: _Data, Scripts, Views y Document_. En la carpeta _Data_ se encuentran las bases de datos utilizadas para el desarrollo del proyecto. En _Scripts_ están los cuadernos en los cuales se escribió todo el codigo utilizado. En _Views_ se guardaron todas las imagenes y resultados obtenidos. Por ultimo, en la carpeta _Document_ se guardó el archivo final requerido por el problem set.

## ¿Como se hizo?

### Tecnología utilizada
El software estadistico utilizado para el pre-procesamiento de las bases de datos y la implementación de los modelos es R versión 4.2.1 

### Datos
Los datos que se utilizaron fueron provistos por el ejercicio. Sin embargo, fueron extraidos originalmente de la Gran Encuesta Integrada de Hogares de Colombia del 2018. El diccionario de la base se puede encontrar en el siguiente [link](http://microdatos.dane.gov.co/index.php/catalog/608/datafile/F1#page=F2&tab=data-dictionary). Las bases de datos están separadas por persona y por hogar para las bases de entrenamiento y testeo de los modelos. 


### Metodología
El reto de construir los modelos predictivos comienza desde la carga de las bases de datos, dado que contienen variables _NAN_ que son especialmente conflictivas al momento de hacer las predicciones. Como resultado de esto se plantea la siguiente metodología:

1. Pre-procesamiento

  i) Cargar las bases de datos y unirlas por __id__ para entrenamiento y prueba.
  
  ii) Imputar las observaciones __NAN__.
  
  iii) Crear variables dicotomas que capturen n-1 niveles de las variables categoricas para tener una mejor manipulación de los datos.

2. Modelaje
  
  En esta sección se busca probar 5 modelos de clasificación y 5 modelos de predicción del ingreso, que al contrastarlo con la linea de pobreza (_Lp_ en la base de       datos) nos indique si la persona es pobre, y consecuentemente el hogar.

3. Elección del modelo con mejores metricas
  
  Al final, se elegirá el modelo con mejores resultados predictivos sobre la base de evaluación teniendo en cuenta que las clasificaciones como FN tienen un mayor peso   que las clasificaciones de FP. Es por esto que la metrica principal a optimizar es la sensitividad.
  
### Pre-procesamiento
Algunos de las modificaciones más importantes hechas a las bases de datos son: 

i) Se agregaron las bases de hogar y personas para tener dos bases de datos: una para entrenamiento y la otra para prueba.

ii) Se imputaron las observaciones que tenian valores vacios para facilitar la implementación de los modelos predictivos. El algoritmo utilizado fue obtenido de la libreria `mixgb`que facilita el comando con el mismo nombre que ejecuta una estructura de imputación multiple basada en _XGBoost
, boostraping y predictive mean matching_. Para más información sobre la libreria está este [repositorio.](https://github.com/agnesdeng/mixgb/blob/master/README.md)

iii) Por ultimo, se estableció que la aproximación más sencilla frente a las variables categoricas era crear una variable indicadora para los n-1 niveles de cada una. 

## ¿Cómo replicar los resultados?
Para replicar los resultados se sugiere clonar el repositorio entero a una carpeta de su preferencia y correr el __Script__ provisto. No hay necesidad de establecer el directorio dado que se hace automaticamente. Sin embargo, puede que se necesiten instalar algunas librerias a mano.

