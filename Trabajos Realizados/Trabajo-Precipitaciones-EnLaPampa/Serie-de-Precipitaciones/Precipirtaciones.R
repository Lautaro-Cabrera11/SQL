install.packages("tidyverse")
install.packages("lubridate")
install.packages("tseries")
install.packages("astsa")
install.packages("foreign")
install.packages("timsac")
install.packages("vars")
install.packages("lmtest")
install.packages("mFilter")
install.packages("dynlm")
install.packages("nlme")
install.packages("broom")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("tidyr")
install.packages("forecast")
install.packages("fpp2")
install.packages("ggplot2")
install.packages("reshape2")
library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(broom)
library(kableExtra)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(ggplot2)
library(readxl)

library(readxl)
library(reshape2)

clasificacion_manual <- c(0, 0, 1,2,2,1,1,0,2,2,0,0,1,0,0,0,0,0,2,2,1,1,0,0,0,0,1,1,0,2,2,1,0,1,2,2,2,1,1,1,0,0,0,1,2,2,1,0,1,1,1,2,1,1,2,2,2,1,1,1,1,0,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,1,2,1,2,2,1,2,2,2,0,1,1,2,2,2,1,0,2,2,2,2) 
length(clasificacion_manual)
dummy_variables <- model.matrix(~ factor(clasificacion_manual) - 1)
# Leer el archivo Excel
datos_excel <- read_excel("Luiggi-Precip.xlsx", header=TRUE)

# Convertir el dataframe en una serie temporal
Luiggi_Precip_TS <- ts(data = as.vector(t(Luiggi_Precip)))

# Mostrar la serie temporal
print(Luiggi_Precip_TS)


# Verificar si mi_vector es un vector
es_vector <- is.vector(Luiggi_Precip_TS)

# Imprimir el resultado
print(es_vector)  # Esto imprimirá TRUE si mi_vector es un vector

mi_serie_freq_12 <- ts(Luiggi_Precip_TS, frequency = 12, star=c(1921,1))
mi_serie_freq_12
plot(mi_serie_freq_12)
mi_serie_con_clasificacion <- cbind(mi_serie_freq_12, clasificacion_manual)
length(mi_serie_freq_12)
clasificacion_replicada <- rep(clasificacion_manual, each = 12)
clasificacion_replicada
length(clasificacion_replicada)

mi_serie_con_clasificacion <- cbind(mi_serie_freq_12, clasificacion_replicada)

#Base de datos con Dummys
mi_serie_con_clasificacion
acf(mi_serie_freq_12, lag.max=48)
pacf(mi_serie_freq_12, lag.max=48)

#Probamos modelos 
fit1prueba <- arima(mi_serie_freq_12, order=c(1,1,3), seasonal=c(order=c(1,1,1), period=12),xreg=clasificacion_replicada, method = "ML")
summary(fit1prueba)
AIC(fit1prueba)
acf(fit1prueba$residuals, lag.max=48)
pacf(fit1prueba$residuals, lag.max=48)
tsdiag(fit1prueba)


#Estimacion-sinDummy
fit1 <- arima(mi_serie_freq_12, order=c(0,1,0), seasonal=c(order=c(0,0,0), period=12), method = "ML")

acf(fit1$residuals, lag.max=48)
pacf(fit1$residuals, lag.max=48)


fit2 <- arima(mi_serie_freq_12, order=c(0,0,0), seasonal=c(order=c(0,1,0), period=12), method = "ML")

acf(fit2$residuals, lag.max=48)
pacf(fit2$residuals, lag.max=48)


fit3 <- arima(mi_serie_freq_12, order=c(2,0,0), seasonal=c(order=c(0,1,0), period=12), method = "ML")

acf(fit3$residuals, lag.max=48)
pacf(fit3$residuals, lag.max=48)

fit4 <- arima(mi_serie_freq_12, order=c(2,0,0), seasonal=c(order=c(1,1,0), period=12), method = "ML")

acf(fit4$residuals, lag.max=48)
pacf(fit4$residuals, lag.max=48)

fit5 <- arima(mi_serie_freq_12, order=c(1,1,3), seasonal=c(order=c(1,1,0), period=12), method = "ML")

acf(fit5$residuals, lag.max=48)
pacf(fit5$residuals, lag.max=48)

#Este es el mejor aparentemente: 

fit6 <- arima(mi_serie_freq_12, order=c(1,1,3), seasonal=c(order=c(1,1,1), period=12), method = "ML")

acf(fit6$residuals, lag.max=48)
pacf(fit6$residuals, lag.max=48)
tsdiag(fit6)


fitAuto <- auto.arima(mi_serie_freq_12, ic = "aic")
summary(fitAuto )

acf(fitAuto$residuals, lag.max=48)
pacf(fitAuto$residuals, lag.max=48)

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fitAuto)
AIC(fit5)
AIC(fit6)

#Predicciones : 

# Número de pasos hacia adelante para predecir
pasos <- 12  # Por ejemplo, predicción de 12 pasos hacia adelante

# Realizar predicciones para los próximos 'pasos' pasos
predicciones <- predict(fit6, n.ahead = pasos)

# Mostrar las predicciones

print(predicciones)

#Probamos con otras predicciones 

forecast(fit6,h=12)

