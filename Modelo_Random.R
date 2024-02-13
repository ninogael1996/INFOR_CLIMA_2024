###MODELO DE PREDICCION ESTADISTICO PROYECTO#####

### 1. Instalación y carga de paquetes.
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
library(zoo)
library(stats)

### Carga de datos

setwd("~/ING AMBIENTAL/INFORMATICA23-24/Proyecto")
temp <- read.csv("Temp_Final.csv", sep = ",", dec = ".", header = T)

### 2. Analisis de Datos.

temp_ts <- ts(temp$Temperatura, frequency = 12, start = c(1975,1))
plot(temp_ts, main = "Serie de Tiempo de Temperatura 1975-2019", xlab = "Años", ylab = "Temperatura (°C)")

  ### Vamos a realizar una descomposicion de la serie de tiempo en los siguientes parámetros.
temp_ts_descomp <- decompose(temp_ts) 

plot(temp_ts_descomp$x, main = "Original", col = "black", ylab = "Serie de tiempo")
plot(temp_ts_descomp$trend, main = "Tendencia", col = "blue", ylab = "Valores")
plot(temp_ts_descomp$seasonal, main = "Estacionalidad", col = "red", ylab = "Valores")
plot(temp_ts_descomp$random, main = "Irregularidad", col = "green", ylab = "Valores")  

### 3. Creación de Variables de fecha y carga de Datos.

fecha_final <- as.Date("2019-07-01")

fecha_pred <- as.Date("2020-07-01")

date_inicial <- as.Date("1975-01-01")

date_final <- as.Date("2019-06-01")

Fecha <- as.Date(seq(from = date_inicial, to = date_final, by="month"))



### 4. Los datos debemos convertir a data frame.

temp <- as.data.frame(temp)
 
    ### Unir las fechas creadas con la temperatura para tener en el formato que R solicita-

temp <- as.data.frame(cbind(temp$Temperatura, Fecha))
    
    ### Renombrar las variables.
names(temp) <- c("Temperatura", "Fecha")

    ### Recalibrar la variable fecha.
    ### Debemos transformar la columna fecha en el formato de fecha para que Rstudio reconozca que esta
    ### columna son las fechas que fueran tomadas nuestras temperaturas.
temp$Fecha <- as.Date(temp$Fecha)

   
    ### Crear un nuevo Data Frame para los datos que vamos a predecir.
fecha_predic <- as.Date(seq(from = fecha_final, to = (fecha_pred), by = "month"))
Temperatura <- as.numeric(NA)
fecha_predic <- as.data.frame(cbind(Temperatura, fecha_predic))
fecha_predic$fecha_predic <- as.Date(fecha_predic$fecha_predic)
names(fecha_predic) <- c("Temperatura", "Fecha")

    ### Unir las dos matrices creadas.
temp <- rbind(temp, fecha_predic)

    ### Crear un duplicado de las fechas para no perder datos en el proceso de modelamiento
temp$Fecha_dup <- temp$Fecha

    ### Vamos a dividir nuestra fecha en año, mes y día.
temp <- temp %>% separate(Fecha, c("Año", "Mes", "Dia"))

    ### Convertir Año, Mes y Dia en valores numéricos.
temp$Año <- as.numeric(temp$Año)
temp$Mes <- as.numeric(temp$Mes)
temp$Dia <- as.numeric(temp$Dia)


    ### Crear nuestros datos de entrenamiento y testeo.
set.seed(1996)
train <- createDataPartition(na.omit(subset(temp, temp$Fecha_dup < fecha_final))$Temperatura
                             , p = 0.7, list = F) 
test <- rbind(temp[-train,] , subset(temp, temp$Fecha_dup >= fecha_final))

### 5. Modelamiento con el algoritmo Random Forest.

mod_rf <- randomForest(Temperatura ~ Fecha_dup, data = temp[train,], 
                       type = "regression", ntree = 500)
pred_rf <- predict(mod_rf, test)

datos_rf <- cbind(pred_rf, test)

    ## Obtener el error absoluto y en porcentaje del modelo calculado.
error_abs_rf <-RMSE(datos_rf$Temperatura, datos_rf$pred_rf, na.rm = T)

error_por_rf <- error_abs_rf / datos_rf[datos_rf$Fecha_dup == max(na.omit(datos_rf)$Fecha_dup), ]$Temperatura
error_por_rf*100

MAE(datos_rf$Temperatura, datos_rf$pred_rf, na.rm = T)
### 6. Dibujar el modelo con el comportamiento normal de Temperatura

ggplot() + geom_line(data = datos_rf, aes(x = Fecha_dup, y = Temperatura), color = "black") +
  geom_line(data = datos_rf, aes(x = Fecha_dup, y = pred_rf), color = "red") + 
  ggtitle("Modelo de Predicción para Temperatura Estacion Iñaquito ")


### Realizamos el mismo proceso para precipitación porque coinciden las fechas para la toma de datos

setwd("~/ING AMBIENTAL/INFORMATICA23-24/Proyecto")
preci <- read.csv("Preci_Final.csv", sep = ",", dec = ".", header = T)

### 2. Analisis de Datos.

preci_ts <- ts(preci$Parque.Bicentenario, frequency = 12, start = c(1975,1))
plot(preci_ts, main = "Serie de Tiempo de Precipitación 1975-2019", xlab = "Años", ylab = "Precipitación (mm)")

### Vamos a realizar una descomposicion de la serie de tiempo en los siguientes parámetros.
preci_ts_descomp <- decompose(preci_ts) 

plot(preci_ts_descomp$x, main = "Original", col = "black", ylab = "Serie de tiempo")
plot(preci_ts_descomp$trend, main = "Tendencia", col = "blue", ylab = "Valores")
plot(preci_ts_descomp$seasonal, main = "Estacionalidad", col = "red", ylab = "Valores")
plot(preci_ts_descomp$random, main = "Irregularidad", col = "green", ylab = "Valores")  

### 3. Los datos debemos convertir a data frame.

preci <- as.data.frame(preci)

### Renombrar las variables.

names(preci) <- c("Fecha", "Precipitacion")

### Unir las fechas creadas con la temperatura para tener en el formato que R solicita-

preci <- as.data.frame(cbind(Fecha, preci$Precipitacion))


### Recalibrar la variable fecha.
### Debemos transformar la columna fecha en el formato de fecha para que Rstudio reconozca que esta
### columna son las fechas que fueran tomadas nuestras temperaturas.
preci$Fecha <- as.Date(preci$Fecha)

names(preci) <- c("Fecha", "Precipitacion")

### Crear un nuevo Data Frame para los datos que vamos a predecir.
fecha_predic_preci <- as.Date(seq(from = fecha_final, to = (fecha_pred), by = "month"))
Precipitacion <- as.numeric(NA)
fecha_predic_preci <- as.data.frame(cbind(fecha_predic_preci, Precipitacion))
fecha_predic_preci$fecha_predic_preci <- as.Date(fecha_predic_preci$fecha_predic_preci)
names(fecha_predic_preci) <- c("Fecha", "Precipitacion")

### Unir las dos matrices creadas.
preci <- rbind(preci, fecha_predic_preci)

### Crear un duplicado de las fechas para no perder datos en el proceso de modelamiento
preci$Fecha_dup_preci <- preci$Fecha

### Vamos a dividir nuestra fecha en año, mes y día.
preci <- preci %>% separate(Fecha, c("Año", "Mes", "Dia"))

### Convertir Año, Mes y Dia en valores numéricos.
preci$Año <- as.numeric(preci$Año)
preci$Mes <- as.numeric(preci$Mes)
preci$Dia <- as.numeric(preci$Dia)

### Crear nuestros datos de entrenamiento y testeo.
set.seed(1997)
train_preci <- createDataPartition(na.omit(subset(preci, preci$Fecha_dup_preci < fecha_final))$Precipitacion
                             , p = 0.7, list = F) 
test_preci <- rbind(preci[-train_preci,] , subset(preci, preci$Fecha_dup_preci >= fecha_final))

### 4. Modelamiento con el algoritmo Random Forest.

mod_rf_preci <- randomForest(Precipitacion ~ Fecha_dup_preci, data = preci[train_preci,], 
                       type = "regression", ntree = 500)

pred_rf_preci <- predict(mod_rf_preci, test_preci)

datos_rf_preci <- cbind(pred_rf_preci, test_preci)

## Obtener el error absoluto y en porcentaje del modelo calculado.
error_abs_rf_preci <-RMSE(datos_rf_preci$Precipitacion, datos_rf_preci$pred_rf_preci, na.rm = T)

error_por_rf_preci <- error_abs_rf / datos_rf_preci[datos_rf_preci$Fecha_dup_preci == max(na.omit(datos_rf_preci)$Fecha_dup_preci), ]$Precipitacion
error_por_rf_preci*100

MAE(datos_rf_preci$Precipitacion, datos_rf_preci$pred_rf_preci, na.rm = T)
### 5. Dibujar el modelo con el comportamiento normal de Temperatura

ggplot() + geom_line(data = datos_rf_preci, aes(x = Fecha_dup_preci, y = Precipitacion), color = "black") +
  geom_line(data = datos_rf_preci, aes(x = Fecha_dup_preci, y = pred_rf_preci), color = "red") + 
  ggtitle("Modelo de Predicción para Precipitación Estacion Iñaquito ")

