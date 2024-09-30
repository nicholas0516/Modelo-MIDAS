#-------------------------------------------------------------------------------
#                         MIDAS: desempleo (semanal) 
#-------------------------------------------------------------------------------

#---- Paquetes ----

library(forecast)    # Para hacer pronósticos con modelos arima
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados más estéticos.
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(readxl)      # Para leer archivos excel 
library(car)         # Para usar la función qqPlot
library(tsibble)     # Para poder emplear objetos de series de tiempo tsibble
library(feasts)      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
library(reshape2)    # Paquete de visualización de datos 
library(tidyverse)   # Visualizacion y manipulacion de datos
library(boot)        # Para hacer boottrapping 
library(midasr)      # Paquete para poder trabajar con modelos MIDAS en R studio
library(lmtest)      # Pruebas estadísticas específicas para modelos de regresión lineal y modelos de series de tiempo
library(dplyr)       # Manipulación y transformación de datos de manera eficiente y efectiva
library(writexl)
library(ggplot2)

#---- Datos ----
Data <- file.choose()

lf <- read_excel(Data, sheet = "TD")

hf <- read_excel(Data,
                 sheet = "Semanal4")

hf$Date <- as.Date(hf$Date)
lf$Date <- as.Date(lf$Date)


#---- Objeto serie de tiempo  ----

#Con objeto ts
#linkedin     <- ts(hf$LinkedIn,                  start = c(2018,9), frequency = 48)
#palabra_2    <- ts(hf$Trabajo_si_hay,            start = c(2018,9), frequency = 48)
#agencia      <- ts(hf$agencia_publica_de_empleo, start = c(2018,9), frequency = 48)
Empleo      <- ts(hf$Empleo, start = c(2018,10), frequency = 48)
Trabajo      <- ts(hf$Trabajo, start = c(2018,10), frequency = 48)
pmi          <- ts(lf$PMI,  start = c(2018,10), frequency = 12)
#IPC           <- ts(lf$IPC,   start = c(2015,10), frequency = 12)
TD           <- ts(lf$TD,   start = c(2018,10), frequency = 12)

autoplot(Empleo)+
  labs(title = "Serie de tiempo",       
       x = "Tiempo",
       y = "Valor",
       colour = "#00a0dc")+
  theme_bw()

#---- Estpmi#---- Estacionarias ----

#linkedin     <- na.omit(diff(log(linkedin)))
#palabra_2    <- na.omit(diff(log(palabra_2)))
#agencia      <- na.omit(diff(log(agencia)))
Empleo <- na.omit(diff(Empleo));adf.test(Empleo)
Trabajo <- na.omit(diff(Trabajo));adf.test(Trabajo)
pmi           <- na.omit(diff(pmi));adf.test(pmi)
#IPC     <- na.omit(diff(IPC));adf.test(IPC)
TD     <- na.omit(diff(TD));adf.test(TD)

#Correlaciones
matriz_correlacion <- cor(lf[, c("TD", "PMI")])
matriz_correlacion


#---- Modelo ---- 
#Modelo con dummies
 hf$dummy1  <- ifelse(format(hf$Date,"%Y-%m")    == "2020-04",      1, 0)          #Abril 2020        (sí)
 hf$dummy2  <- ifelse(format(hf$Date, "%m")      == "01",           1, 0)          #Todos los enero   (sí)
 hf$dummy3  <- ifelse(format(hf$Date,"%m")       == "02",           1, 0)          #Todos los febrero (sí) 
 hf$dummy4  <- ifelse(format(hf$Date,"%Y-%m")    == "2020-08",      1, 0)  #Agosto del 2020   (sí) 
 hf$dummy5  <- ifelse(format(hf$Date,"%Y-%m")    == "2020-05",      1, 0)  #Mayo del 2020   (sí) 
 hf$dummy6  <- ifelse(format(hf$Date,"%Y-%m")    == "2019-07",      1, 0)  #Agosto del 2020   (sí) 
 hf$dummy7  <- ifelse(format(hf$Date,"%Y-%m")    == "2020-06",      1, 0)  #Agosto del 2020   (sí) 
 hf$dummy8  <- ifelse(format(hf$Date,"%Y-%m")    == "2021-07",      1, 0)  #Agosto del 2020   (sí) 
 #hf$dummy10 <- ifelse(format(hf$Date,"%Y-%m")    == "2021-05",      1, 0)  #Agosto del 2020   (sí) 
 #hf$dummy9  <- ifelse(format(hf$Date,"%Y-%m")    == "2022-06",      1, 0)  #Agosto del 2020   (sí) 
 #hf$dummy11 <- ifelse(format(hf$Date,"%m")       == "12"     ,      1, 0)  #Agosto del 2020   (sí) 
 #hf$dummy12 <- ifelse(format(hf$Date,"%Y-%m")    == "2022-07",      1, 0)  #Agosto del 2020   (sí) 
 
dummy1  <- hf$dummy1 [2:265]
dummy2  <- hf$dummy2 [2:265]
dummy3  <- hf$dummy3 [2:265]
dummy4  <- hf$dummy4 [2:265]
dummy5  <- hf$dummy5 [2:265]
dummy6  <- hf$dummy6 [2:265]
dummy7  <- hf$dummy7 [2:265]
dummy8  <- hf$dummy8 [2:265]
#dummy10 <- hf$dummy10[2:265]

#dummy9  <- hf$dummy9 [2:265]
#dummy11 <- hf$dummy11[2:265]
#dummy12 <- hf$dummy12[2:265]
#+ mls(dummy11, 1, 4)
#+ mls(dummy10, 1, 4)
# + mls(dummy10, 1, 4)

#En este modelo se meten cuatro rezagos de las semanas para que se capture el efecto de las búsquedas del mes 
mr1 <- midas_r(TD ~    mls(pmi,1,1)  + mls(Trabajo,1,4)+ mls(dummy1, 1, 4)  + mls(dummy2, 1, 4) + mls(dummy3, 1, 4) + mls(dummy4, 1, 4) 
               + mls(dummy5 , 1, 4) + mls(dummy6 , 1, 4) + mls(dummy7, 1, 4) + mls(dummy8, 1, 4), start = NULL) ; summary(mr1)
##Correlogframa TD


#mr2 <- midas_r(td ~ mls(linkedin,4,4, almon) + mls(agencia,4,4) + mls(dummy1, 1, 4) + mls(dummy2, 1, 4) + mls(dummy3, 1, 4), start = c(##3))


xt <- data.frame(lf$TD)
col<- ts(xt, start = c(2018, 10), frequency = 12)

residuales_1    <- residuals(mr1)
fit_1           <- col - residuales_1     

#Ajuste

plot.ts(col,type="l",main= "Prueba",lwd=2)
points(fit_1, col="green",lwd=2, type = "l")

#---- Ruido blanco ----

media1 <- mean(residuales_1)

plot(residuales_1, type = "l")
abline(h = media1, col = "red", lty = 1) # Residuales son ruido blanco

#Autocorrelación , p value > 0.05
Box.test(residuales_1, lag = 20, type = "Ljung-Box")  #No hay autocorrelación
Box.test(residuales_1, lag = 30, type ='Box-Pierce')  #No hay autocorrelación  

#Estacionarios test-statistic < critical values
summary(ur.df(residuales_1, lags = 6, selectlags = "AIC", type = "none")) #Es estacionaria y no tiene raiz unitaria

#Normalidad 
hist(residuales_1)
qqPlot(residuales_1)


#---- Pronóstico ----

#New data. Se deben cargar los datos del mes a pronosticar. En este caso, los de agosto 
datos_pro_mensuales <- read_excel("Data.xlsx",
                                  sheet = "datos_pro_mensuales")
datos_pro_mensuales$Date  <- as.Date(datos_pro_mensuales$Date)
pmi_pro     <- ts(datos_pro_mensuales$PMI  , start = c(2024, 3), frequency = 12)
pmi_pro     <- na.omit(diff((pmi_pro)))


datos_pro_semanales <- read_excel("Data.xlsx",
                       sheet = "datos_pro_semanales")
datos_pro_semanales$Date  <- as.Date(datos_pro_semanales$Date)

Empleo_pro     <- ts(datos_pro_semanales$Empleo, start = c(2024, 2), frequency = 48)
Empleo_pro     <- na.omit(diff((Empleo_pro)))

Trabajo_pro     <- ts(datos_pro_semanales$Trabajo, start = c(2024, 2), frequency = 48)
Trabajo_pro     <- na.omit(diff((Trabajo_pro)))

datos_pro_semanales$dummy1  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")== "2020-04", 1, 0)  #Abril 2020 (sí)
datos_pro_semanales$dummy2  <- ifelse(format(datos_pro_semanales$Date, "%m")  == "01",      1, 0)  #Todos los enero (sí)
datos_pro_semanales$dummy3  <- ifelse(format(datos_pro_semanales$Date,"%m")   == "02",      1, 0)  #Todos los febrero (sí) 
datos_pro_semanales$dummy4  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2020-08", 1, 0)  #Agosto del 2020   (sí) 
datos_pro_semanales$dummy5  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2020-05", 1, 0)  #Mayo del 2020   (sí) 
datos_pro_semanales$dummy6  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2019-07", 1, 0)  #Agosto del 2020   (sí) 
datos_pro_semanales$dummy7  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2020-06", 1, 0)  #Agosto del 2020   (sí) 
datos_pro_semanales$dummy8  <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2021-07", 1, 0)  #Agosto del 2020   (sí) 
datos_pro_semanales$dummy10 <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2021-04", 1, 0)  #Agosto del 2020   (sí) 
datos_pro_semanales$dummy12 <- ifelse(format(datos_pro_semanales$Date,"%Y-%m")              == "2022-07", 1, 0)  #Julio del 2022   (sí) 

dummy1a  <- datos_pro_semanales$dummy1[2:5]
dummy2a  <- datos_pro_semanales$dummy2[2:5]
dummy3a  <- datos_pro_semanales$dummy3[2:5]
dummy4a  <- datos_pro_semanales$dummy4[2:5]
dummy5a  <- datos_pro_semanales$dummy5[2:5]
dummy6a  <- datos_pro_semanales$dummy6[2:5]
dummy7a  <- datos_pro_semanales$dummy7[2:5]
dummy8a  <- datos_pro_semanales$dummy8[2:5]
dummy10a <- datos_pro_semanales$dummy10[2:5]
dummy12a <- datos_pro_semanales$dummy12[2:5]

#Forecast

forecast(mr1, newdata = list(pmi = pmi_pro, Trabajo = Trabajo_pro,dummy1 = dummy1a, dummy2 = dummy2a, dummy3 = dummy3a,
                             dummy4 = dummy4a,  dummy5 = dummy5a, dummy6 = dummy6a, dummy7 = dummy7a, dummy8 = dummy8a))

#Nowcast 

nowcast <- forecast(mr1, newdata = list( pmi = pmi, Trabajo = Trabajo, dummy1 = dummy1, dummy2 = dummy2, dummy3 = dummy3,
                                         dummy4 = dummy4, dummy5 = dummy5, dummy6 = dummy6, dummy7 = dummy7, dummy8 = dummy8))

nowcast_df <- data.frame(Pronostico = nowcast$mean)

write_xlsx(nowcast_df, "pronosticos_4.xlsx")


#---- Evaluación del pronóstico ----

#---- Ventana expandible

# Pruebas

i <- 1# Establece i en 54

# Generar una secuencia de números basada en el valor de i
secuencia <- (19 + (4 * i))

# Iterar sobre cada número de la secuencia
for (num in secuencia) {
  print(num)
}

#[1 + (4 * (i - 1)):(19 + (4 * i))
#Crear los modelos expandibles, restar a la cantidad de semanas 6 y da el numero mas lejano del loop 
# para el valo mas bajo del loop se resta el valor mas alto -25 
# la formula debes ser 4xel numero mas bajo
#Finalemnet se le quita el numero mas bajo-1 a m

# Primer bucle para ajustar los modelos y verificar las longitudes
m <- list()
for (i in 35:60) {
  # Asignar un nombre a cada una de las variables explicativas
  TDD     <- TD[1:i]
  pmii    <- pmi[1:i]
  Trabajoo <- Trabajo[1:(140 + 4 * (i - 35))] # Va aumentando cada 4 datos, en 35 son 136 y en 59 son 236
  dummy11 <- dummy1[2:(141 + 4 * (i - 35))]
  dummy22 <- dummy2[2:(141 + 4 * (i - 35))]
  dummy33 <- dummy3[2:(141 + 4 * (i - 35))]
  dummy44 <- dummy4[2:(141 + 4 * (i - 35))]
  dummy55 <- dummy5[2:(141 + 4 * (i - 35))]
  dummy66 <- dummy6[2:(141 + 4 * (i - 35))]
  dummy77 <- dummy7[2:(141 + 4 * (i - 35))]
  dummy88 <- dummy8[2:(141 + 4 * (i - 35))]
  
  # Imprimir las longitudes de las series temporales
  cat("Iteración:", i, "\n")
  cat("Longitud TDD:", length(TDD), "\n")
  cat("Longitud pmii:", length(pmii), "\n")
  cat("Longitud Trabajoo:", length(Trabajoo), "\n")
  cat("Longitud dummy11:", length(dummy11), "\n")
  cat("Longitud dummy22:", length(dummy22), "\n")
  cat("Longitud dummy33:", length(dummy33), "\n")
  cat("Longitud dummy44:", length(dummy44), "\n")
  cat("Longitud dummy55:", length(dummy55), "\n")
  cat("Longitud dummy66:", length(dummy66), "\n")
  cat("Longitud dummy77:", length(dummy77), "\n")
  cat("Longitud dummy88:", length(dummy88), "\n")
  
  # Aplicar el modelo midas
  tryCatch({
    modelo_a <- update(mr1, formula = TDD ~ mls(pmii, 1, 1) +
                         mls(Trabajoo, 1, 4) 
                       + mls(dummy11, 1, 4) +
                       mls(dummy22, 1, 4) +
                       mls(dummy33, 1, 4) +
                       mls(dummy44, 1, 4) +
                       mls(dummy55, 1, 4) +
                       mls(dummy66, 1, 4) +
                       mls(dummy77, 1, 4) +
                       mls(dummy88, 1, 4)
                       , start = NULL)
    m[[i]] <- modelo_a
  }, error = function(e) {
    cat("Error en la iteración:", i, "\n", e$message, "\n")
    next
  })
}

m <- m[-c(1:34)] #Borrar los vacios 1 menos que el numero mas bajo

#Realizar la línea de código que me hace el pronostico de seis pasos adelante para los 24 modelos almacenados 
#en la lista m. 

# Para la variable h se coge el numero mas bajo y se usa su ultima fecha para la diferencia
# luego se copia y paga hasta el ultima dato que tengamos, lo mismo con h_pmi

#Data para el forecast

  #Lo primero que toca hacer es especificar los datos que se usarán para el forecast de los seis pasos adelante de cada 
  #uno de los 26 modelos almacenados en m.
  
  nd <- read_excel("Data.xlsx",
                   sheet = "h")
  nd$Date <- as.Date(nd$Date)
  
  new_Empleo     <- ts(nd$Empleo, start = c(2021, 08), frequency = 48)
  new_Empleo     <- na.omit(diff(new_Empleo))
  
  new_Trabajo     <- ts(nd$Trabajo, start = c(2021, 08), frequency = 48)
  new_Trabajo     <- na.omit(diff((new_Trabajo)))

  nd$dummy1  <- ifelse(format(nd$Date,"%Y-%m") == "2020-04", 1, 0)  #Abril 2020 (sí)
  nd$dummy2  <- ifelse(format(nd$Date, "%m")   == "01",      1, 0)  #Todos los enero (sí)
  nd$dummy3  <- ifelse(format(nd$Date,"%m")    == "02",      1, 0)  #Todos los febrero (sí) 
  nd$dummy4  <- ifelse(format(nd$Date,"%Y-%m")    == "2020-08",      1, 0)  #Agosto del 2020   (sí)
  nd$dummy5  <- ifelse(format(nd$Date,"%Y-%m")    == "2020-05",      1, 0)  #Mayo del 2020   (sí)
  nd$dummy6  <- ifelse(format(nd$Date,"%Y-%m")    == "2019-07",      1, 0)  #Agosto del 2020   (sí)
  nd$dummy7  <- ifelse(format(nd$Date,"%Y-%m")    == "2020-06",      1, 0)  #Agosto del 2020   (sí)
  nd$dummy8  <- ifelse(format(nd$Date,"%Y-%m")    == "2021-07",      1, 0)  #Agosto del 2020   (sí)

  
  dummy1nd <- nd$dummy1[2:129] #Porque son 129 datos, o los sdtaos en new_empleo
  dummy2nd <- nd$dummy2[2:129]
  dummy3nd <- nd$dummy3[2:129]
  dummy4nd <- nd$dummy4[2:129]
  dummy5nd <- nd$dummy5[2:129]
  dummy6nd <- nd$dummy6[2:129]
  dummy7nd <- nd$dummy7[2:129]
  dummy8nd <- nd$dummy8[2:129]
  
  #Bucle para las dummies
  #La dumme esta colgada por una unidad fentre a la cantidad de datos en h
  #la formula debe dar la cantidad de datos menos 1
  
  dummy_1 <- list()
  for (i in 1:26) {
    a <- dummy1nd[((i - 1) * 4 + 1):(i * 4)]  # Crear grupos de 4 elementos
    dummy_1[[i]] <- a
  }
  
  # Verificar los resultados
  for (i in 1:26) {
    cat("Iteración:", i, "Datos:", dummy_1[[i]], "\n")
  }

  dummy_2 <- list()
  for (i in 1:26) {
    a <- dummy2nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_2[[i]] <- a
  }
  
  dummy_3 <- list()
  for (i in 1:26) {
    a <- dummy3nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_3[[i]] <- a
  }
  
  dummy_4 <- list()
  for (i in 1:26) {
    a <- dummy4nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_4[[i]] <- a
  }

  dummy_5 <- list()
  for (i in 1:26) {
    a <- dummy5nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_5[[i]] <- a
  }

  dummy_6 <- list()
  for (i in 1:26) {
    a <- dummy6nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_6[[i]] <- a
  }

  dummy_7 <- list()
  for (i in 1:26) {
    a <- dummy7nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_7[[i]] <- a
  }

  dummy_8 <- list()
  for (i in 1:26) {
    a <- dummy8nd[((i - 1) * 4 + 1):(i * 4)]  # Eliminar el último elemento
    dummy_8[[i]] <- a
  }
  
  for (i in 1:26) {
    dummy_1[[i]][is.na(dummy_1[[i]])] <- 0
    dummy_2[[i]][is.na(dummy_2[[i]])] <- 0
    dummy_3[[i]][is.na(dummy_3[[i]])] <- 0
    dummy_4[[i]][is.na(dummy_4[[i]])] <- 0
    dummy_5[[i]][is.na(dummy_5[[i]])] <- 0
    dummy_6[[i]][is.na(dummy_6[[i]])] <- 0
    dummy_7[[i]][is.na(dummy_7[[i]])] <- 0
    dummy_8[[i]][is.na(dummy_8[[i]])] <- 0
  }
 
  nd_pmi <- read_excel("Data.xlsx",
                   sheet = "h_pmi")
  nd_pmi$Date <- as.Date(nd_pmi$Date)
  new_pmi     <- ts(nd_pmi$PMI, start = c(2021, 08), frequency = 12)
  new_pmi     <- na.omit(diff((new_pmi)))

   #Crear una lista con la nueva data 

  newdata_prueba <- list()
  for (i in 1:26) {
    newdata_prueba[[i]] <- list(
      pmii        = new_pmi[[i]],
      Trabajoo    = new_Trabajo[((i - 1) * 4 + 1):(i * 4)]
      ,dummy11     = dummy_1[[i]],
      dummy22     = dummy_2[[i]],
      dummy33     = dummy_3[[i]],
      dummy44     = dummy_4[[i]],
      dummy55     = dummy_5[[i]],
      dummy66     = dummy_6[[i]],
      dummy77     = dummy_7[[i]],
      dummy88     = dummy_8[[i]]
    )
    
    # Imprimir las longitudes y los valores de las series en newdata_prueba
    cat("Iteración newdata_prueba:", i, "\n")
    cat("Longitud pmii:", length(newdata_prueba[[i]]$pmii), "\n")
    cat("Valores pmii:", newdata_prueba[[i]]$pmii, "\n")
    cat("Longitud Trabajoo:", length(newdata_prueba[[i]]$Trabajoo), "\n")
    cat("Valores Trabajoo:", newdata_prueba[[i]]$Trabajoo, "\n")
    for (j in 1:8) {
      dummy_name <- paste0("dummy", j, j)
      if (!is.null(newdata_prueba[[i]][[dummy_name]])) {
        cat("Longitud", dummy_name, ":", length(newdata_prueba[[i]][[dummy_name]]), "\n")
        cat("Valores", dummy_name, ":", newdata_prueba[[i]][[dummy_name]], "\n")
      }
    }
  }
  
  #Crear la tabla que me almacenerá la lista de los sesgos
  sesgo_1   = matrix(nrow = 26 ,ncol = 6)  
  
  for(i in 1:26){
                sesgo_1[i,1:6] <- TD[(34+i):(39+i)]-forecast(m[[i]], newdata = newdata_prueba[[i]])$mean
                }

  #forecast(m[[i]], newdata = list(linkedin = new_linkedin[1+(4*(i-1)):(21 +(4*i))],agenciaa = new_agencia[1+(4*(i-1)):(21+(4*i))], dummy11 = dummy_1[[i]], dummy22 = dummy_2[[i]], dummy33 = dummy_3[[i]])
  
indicadores = rbind(sesgo_1 = colMeans(sesgo_1),MSE=rep(0,6),RMSE=rep(NA,6),MAE=rep(NA,6))
for (i in 1:6){
  indicadores[2,i] = mean((sesgo_1[,i]))
  indicadores[3,i] = sqrt(mean((sesgo_1[,i])^2))
  indicadores[4,i] = mean(abs(sesgo_1[,i]))}
print(indicadores,digits = 3)
