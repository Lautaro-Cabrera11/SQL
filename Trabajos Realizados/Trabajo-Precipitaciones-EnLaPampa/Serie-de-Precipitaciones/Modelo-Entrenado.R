#Vamos a partir el modelo en 2 !

# Longitud total de los datos
n <- length(mi_serie_freq_12)
#indice_entrenamiento <- 1:996
#indice_prediccion <- 997:length(mi_serie_freq_12)

# Obtener el índice para separar los datos
indice_entrenamiento <- 996
indice_entrenamiento
# Datos de entrenamiento (80%)
datos_entrenamiento <- mi_serie_freq_12[1:indice_entrenamiento]
datos_entrenamiento 
# Datos de validación (20%)
datos_validacion <- mi_serie_freq_12[997:length(mi_serie_freq_12)]
datos_validacion

#Vamos a estimar, cual es el mejor : 

entrenamiento_fit1 <- arima(datos_entrenamiento, order=c(0,1,0), seasonal=c(order=c(0,0,0), period=12), method = "ML")

entrenamiento_1acf(entrenamiento_fit1$residuals, lag.max=48)
entrenamiento_1pacf(entrenamiento_fit1$residuals, lag.max=48)


entrenamiento_fit2 <- arima(datos_entrenamiento, order=c(0,0,0), seasonal=c(order=c(0,1,0), period=12), method = "ML")

entrenamiento_2acf(entrenamiento_fit2$residuals, lag.max=48)
entrenamiento_2pacf(entrenamiento_fit2$residuals, lag.max=48)


entrenamiento_fit3 <- arima(datos_entrenamiento, order=c(2,0,0), seasonal=c(order=c(0,1,0), period=12), method = "ML")

entrenamiento_3acf(entrenamiento_fit3$residuals, lag.max=48)
entrenamiento_3pacf(entrenamiento_fit3$residuals, lag.max=48)

entrenamiento_fit4 <- arima(datos_entrenamiento, order=c(2,0,0), seasonal=c(order=c(1,1,0), period=12), method = "ML")

entrenamiento_4acf(entrenamiento_fit4$residuals, lag.max=48)
entrenamiento_4pacf(entrenamiento_fit4$residuals, lag.max=48)

entrenamiento_fit5 <- arima(datos_entrenamiento, order=c(1,1,3), seasonal=c(order=c(1,1,0), period=12), method = "ML")

entrenamiento_5acf(entrenamiento_fit5$residuals, lag.max=48)
entrenamiento_5pacf(entrenamiento_fit5$residuals, lag.max=48)

#Este es el mejor aparentemente: 

entrenamiento_fit6 <- arima(datos_entrenamiento, order=c(1,1,3), seasonal=c(order=c(1,1,1), period=12), method = "ML")

entrenamiento_6acf(entrenamiento_fit6$residuals, lag.max=48)
entrenamiento_6pacf(entrenamiento_fit6$residuals, lag.max=48)
tsdiag(fit6)

#Agregamos Dummy
#Probamos modelos 
clasificacion_manualentrenada <- c(0, 0, 1,2,2,1,1,0,2,2,0,0,1,0,0,0,0,0,2,2,1,1,0,0,0,0,1,1,0,2,2,1,0,1,2,2,2,1,1,1,0,0,0,1,2,2,1,0,1,1,1,2,1,1,2,2,2,1,1,1,1,0,1,1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,2,2,1,1,1) 
length(clasificacion_manualentrenada)
clasificacion_replicada <- rep(clasificacion_manualentrenada, each = 12)
length(clasificacion_replicada)
fit1pruebadummy <- arima(datos_entrenamiento, order=c(1,1,3), seasonal=c(order=c(1,1,1), period=12),xreg=clasificacion_replicada, method = "ML")
summary(fit1prueba)
AIC(fit1pruebadummy)
acf(fit1pruebadummy$residuals, lag.max=48)
pacf(fit1pruebadummy$residuals, lag.max=48)
tsdiag(fit1pruebadummy)


entrenamientofitAuto <- auto.arima(datos_entrenamiento, ic = "aic")
summary(fitAuto )

acf(entrenamientofitAuto$residuals, lag.max=48)
pacf(entrenamientofitAuto$residuals, lag.max=48)

AIC(entrenamiento_fit1)
AIC(entrenamiento_fit2)
AIC(entrenamiento_fit3)
AIC(entrenamiento_fit4)
AIC(entrenamientofitAuto)
AIC(entrenamiento_fit5)
AIC(entrenamiento_fit6)
AIC(fit1pruebadummy)

#no fue mejor con Dummy

