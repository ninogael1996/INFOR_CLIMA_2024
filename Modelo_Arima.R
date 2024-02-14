setwd("~/ING AMBIENTAL/INFORMATICA23-24/Proyecto")
datos <- read.csv("Temp_Final.csv", sep = ",", dec = ".", header = T)
View(datos)

par(mfrow = c(1,1))
ST1 <- ts(datos$Temperatura, frequency = 12, start = c(1975,1))
plot(ST1, xlab="Años",ylab="Temperatura (°C)",main="Serie de Tiempo Estación Parque IÑAQUITO")

library(forecast)# Contiene el modelo ARIMA
library(tseries) #Para series de tiempo
library(TSA)     #Para series de tiempo
library(urca)    #Para hacer el Test de Raiz Unitaria (detectar hay o no estacionariedad)
library(ggplot2) #Para hacer gráficos
library(dplyr)   #Para la manipulación de datos (filtrar, seleccionar, agregar, transformar)
library(stats)   #Se usa para diversas pruebas estadísticas (medias,varianza, arima,etc)
library(seasonal)#Para calcular la serie ajustada de estacionalidad
library(smooth)

#Utilizamos la función decompose (del paquete cargado previamente "STATS")
ST1_descomp <- decompose(ST1)

par(mfrow = c(2,2))
plot(ST1_descomp$x, main = "Original", col = "black", ylab = "Serie de tiempo")
plot(ST1_descomp$trend, main = "Tendencia", col = "blue", ylab = "Valores")
plot(ST1_descomp$seasonal, main = "Estacionalidad", col = "red", ylab = "Valores")
plot(ST1_descomp$random, main = "Irregularidad", col = "green", ylab = "Valores")

# Realizar prueba de raíz unitaria
adf.test(datos$Temperatura)


#Para hacer este proceso se usa la libreria seasonal instalada y cargada previamente.
#Colocamos el nombre a la nueva variable: ipir_SA (serie ajustada por estacionalidad)
ST1_SA <- seasadj(ST1_descomp)

par(mfrow = c(1,1))
#Graficar serie de tiempo original y ajustada
plot(ST1,xlab="Meses",ylab="Temperatura (°C)",main="Serie de Tiempo Estación Parque IÑAQUITO Cod:M0055")
lines(ST1_SA, col = "red")
legend("bottomright", legend = c("Serie Original", "Serie Ajustada"), col = c("black", "red"), lty = 1)

Acf(ST1_SA)
Pacf(ST1_SA)

#Corremos la función auto.arima:
modelo_arima <- auto.arima(ST1_SA, seasonal = TRUE)
modelo_arima

Model1 <- Arima(ST1_SA, order = c(1,0,2))
Box.test(Model1$residuals, lag = 20, type = "Ljung-Box")
shapiro.test(Model1$residuals)

predicciones1 <- forecast(Model1, h = 12, level = c(95)) 
plot(predicciones1, main="Predicciones para 1 año")
predicciones1

Model2 <- Arima(ST1_SA, order = c(3,1,1))
Box.test(Model2$residuals, lag = 20, type = "Ljung-Box")
shapiro.test(Model2$residuals)


predicciones2 <- forecast(Model2, h = 12, level = c(95)) 
plot(predicciones2, main="Predicciones para 1 año")
predicciones2


