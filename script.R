
# Ambiente de trabajo
setwd("~/Documentos/GitHub/IntroductionToGeostatistics")

load("~/Documentos/GitHub/IntroductionToGeostatistics/data/RData.RData")

# Cargar librerías --------------------------------------------------------

# Geoestadística
library('gstat')
library('geoR')
library('geoRglm')

# Objetos espaciales
library('sp')

# Algunos estadísticos
library('moments')

# Reestructurar datos
library('reshape2')

# Transformaciones Box-Cox
library('forecast')

# Autocorrelaciones - Indices de Moran y Geary
library('ncf')

# Dependencia espacial
library('spdep')

# Akaike
library('MASS')

# Importancia relativa predictores
library('relaimpo')

# Gráficos

# Versión dev de ggplot2, requiere:
# devtools::install_github("hadley/scales")
# devtools::install_github("tidyverse/ggplot2")
library('ggplot2')
library('ggplot2')
library('gridExtra')
library('corrgram') # correlogramas

# Escalas de colores
library('RColorBrewer')


# Cargar datos ------------------------------------------------------------

# Cargar datos
datosRindeMaiz <- read.csv(file = "data/DataExamenGeo16.csv", header = TRUE, sep = ",", dec = ".")

# Mostrar las primeras seis filas
head(datosRindeMaiz)
coordinates(datosRindeMaiz) <- c("Long", "Lat")
proj4string(datosRindeMaiz) <- CRS(as.character(NA))
str(datosRindeMaiz)

# Histogramas de variables ------------------------------------------------

# Ver histogramas de las variables
ggDatosRindeMaiz <- ggplot(data = datosRindeMaiz@data)

# Maíz C1
ggHistMaizC1 <- ggDatosRindeMaiz +
  geom_histogram(aes(x = datosRindeMaiz$maizC1),
                 binwidth = 0.5, fill = "orange", col = "white", alpha = 0.8) +
  labs(title = "", x = "Rendimiento maíz (t/ha)", y = "Frecuencia") + 
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) + 
  scale_y_continuous(limits = c(0, 325), breaks = seq(0, 325, 50)) + 
  theme(plot.margin = unit(c( -1, 1, 1, 1), "mm"))

ggBoxPlotMaizC1 <- ggDatosRindeMaiz +
  geom_boxplot(aes(y = datosRindeMaiz$maizC1, x = rep("RM", length(datosRindeMaiz$maizC1))), 
               fill = "orange", col = "black", alpha = 0.8) + 
  labs(title = "Camp.1", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2), position = "top") +
  theme(plot.margin = unit(c(1, 1, -1, 1), "mm")) +
  coord_flip()

# Napa C1
ggHistNapaDicC1 <- ggDatosRindeMaiz +
  geom_histogram(aes(x = datosRindeMaiz$NapaDicC1), 
                 binwidth = 0.25, fill = "darkblue", col = "white", alpha = 0.7) +
  labs(title = "", x = "Profundidad napa (m)", y = "Frecuencia") + 
  scale_x_continuous(limits = c(1.5, 6.75), breaks = seq(1.5, 6.75, 1)) + 
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) +
  theme(plot.margin = unit(c( -1, 1, 1, 1), "mm"))

ggBoxPlotNapaDicC1 <- ggDatosRindeMaiz +
  geom_boxplot(aes(y = datosRindeMaiz$NapaDicC1, x = rep("PN", length(datosRindeMaiz$NapaDicC1))), 
               fill = "darkblue", col = "black", alpha = 0.7) + 
  labs(title = "Camp.1", x = "", y = "") + 
  scale_y_continuous(limits = c(1.5, 6.75), breaks = seq(1.5, 6.75, 1), position = "top") + 
  theme(plot.margin = unit(c(1, 1, -1, 1), "mm")) +
  coord_flip()

# Maíz C2
ggHistMaizC2 <- ggDatosRindeMaiz +
  geom_histogram(aes(x = datosRindeMaiz$maizC2), 
                 binwidth = 0.5, fill = "orange", col = "white", alpha = 0.8) +
  labs(title = "", x = "Rendimiento maíz (t/ha)", y = "Frecuencia") + 
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(0, 325), breaks = seq(0, 325, 50)) +
  theme(plot.margin = unit(c( -1, 1, 1, 1), "mm"))

ggBoxPlotMaizC2 <- ggDatosRindeMaiz +
  geom_boxplot(aes(y = datosRindeMaiz$maizC2, x = rep("RM", length(datosRindeMaiz$maizC2))), 
               fill = "orange", col = "black", alpha = 0.8) + 
  labs(title = "Camp.2", x = "", y = "") + 
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2), position = "top") +
  theme(plot.margin = unit(c(1, 1, -1, 1), "mm")) +
  coord_flip()

# Napa C2
ggHistNapaDicC2 <- ggDatosRindeMaiz +
  geom_histogram(aes(x = datosRindeMaiz$NapaDicC2), 
                 binwidth = 0.25, fill = "darkblue", col = "white", alpha = 0.7) +
  labs(title = "", x = "Profundidad napa (m)", y = "Frecuencia") + 
  scale_x_continuous(limits = c(1.5, 6.75), breaks = seq(1.5, 6.75, 1)) + 
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) + 
  theme(plot.margin = unit(c( -1, 1, 1, 1), "mm"))

ggBoxPlotNapaDicC2 <- ggDatosRindeMaiz +
  geom_boxplot(aes(y = datosRindeMaiz$NapaDicC2, x = rep("PN", length(datosRindeMaiz$NapaDicC2))), 
               fill = "darkblue", col = "black", alpha = 0.7) + 
  labs(title = "Camp.2", x = "", y = "") + 
  scale_y_continuous(limits = c(1.5, 6.75), breaks = seq(1.5, 6.75, 1), position = "top") +
  theme(plot.margin = unit(c(1, 1, -1, 1), "mm")) +
  coord_flip()

# Elevación
ggHistElev <- ggDatosRindeMaiz +
  geom_histogram(aes(x = datosRindeMaiz$Elev), 
                 binwidth = 0.2, fill = "darkred", col = "white", alpha = 0.7) +
  labs(title = "", x = "Elevación (m)", y = "Frecuencia") + 
  scale_x_continuous(limits = c(241.5, 247.5), breaks = seq(240, 248, 1)) + 
  scale_y_continuous(limits = c(0, 275), breaks = seq(0, 275, 50)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = unit(c( -1, 1, 1, 1), "mm"))

ggBoxPlotElev <- ggDatosRindeMaiz +
  geom_boxplot(aes(y = datosRindeMaiz$Elev, x = rep("EL", length(datosRindeMaiz$Elev))), 
               fill = "darkred", col = "black", alpha = 0.7) + 
  labs(title = "", x = "", y = "") + 
  scale_y_continuous(limits = c(241.5, 247.5), breaks = seq(240, 248, 1), position = "top") + 
  theme(plot.margin = unit(c(1, 1, -1, 1), "mm")) + 
  coord_flip()

X11()

# Ver plot 
grid.arrange(ggBoxPlotMaizC1, 
             ggHistMaizC1, 
             ggBoxPlotMaizC2, 
             ggHistMaizC2, 
             ggBoxPlotNapaDicC1, 
             ggHistNapaDicC1, 
             ggBoxPlotNapaDicC2, 
             ggHistNapaDicC2, 
             ggBoxPlotElev, 
             ggHistElev, 
             ncol = 3, nrow = 4, 
             layout_matrix = cbind(c(1,2,3,4), c(5,6,7,8), c(9,10,10,10)), 
             widths = c(1, 1, 1), heights = c(1, 3, 1, 3))


# Tendencia central y dispersión ------------------------------------------

ResumenEstadisticas <- function(df) {
  
  # Número de columnas del data.frame
  ncol = ncol(df)
  
  # Estadísticas
  maximo = numeric()
  minimo = numeric()
  
  media = numeric()
  mediana = numeric()
  moda = vector()
  
  varianza = numeric()
  desvioEstandar = numeric()
  cv = numeric()
  asimetria = numeric()
  kurtosis = numeric()
  
  NAs = numeric()
  clases = character()
  
  # Función para calcular la moda
  Moda <- function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Iteración que recorre todas las columnas del data.frame
  for(i in 1:ncol) {
    
    # Condición de que la columna para las estadísticas tenga que ser de tipo numérica 
    if(is.numeric(df[,i])) {
      
      mediana[i] = median(df[,i], na.rm = TRUE)
      media[i] = mean(df[,i], na.rm = TRUE)
      moda[i] = Moda(x = df[,i])
      varianza[i] = var(df[,i], na.rm = TRUE)
      desvioEstandar[i] = sd(df[,i], na.rm = TRUE)
      cv[i] = 100 * (sd(df[,i], na.rm = TRUE) / mean(df[,i], na.rm = TRUE))
      asimetria[i] = moments::skewness(df[,i], na.rm = TRUE)
      kurtosis[i] = moments::kurtosis(df[,i], na.rm = TRUE)
      minimo[i] = min(df[,i], na.rm = TRUE)
      maximo[i] = max(df[,i], na.rm = TRUE)
      NAs[i] = length(which(is.na(df[,i]) == TRUE))
      clases[i] = class(df[,i])
      
    } 
    
    # En caso de que no sea de tipo numérica asignar NAs a las 
    #  estadísticas pero calcular el número de NAs y tipo de datos
    else {
      
      mediana[i] = NA
      media[i] = NA
      moda[i] = Moda(x = df[,i])
      varianza[i] = NA
      desvioEstandar[i] = NA
      cv[i] = 100 * NA
      asimetria[i] = NA
      kurtosis[i] = NA
      minimo[i] = NA
      maximo[i] = NA
      NAs[i] = length(which(is.na(df[,i]) == TRUE))
      clases[i] = class(df[,i])
      
    }
  } 
  
  # Generar data.frame con todas las estadísticas
  summary = data.frame("mínimo" = minimo,
                       "máximo" = maximo,
                       "media" = media,
                       "mediana" = mediana,
                       "moda" = moda,
                       "varianza" = varianza,
                       "ds" = desvioEstandar,
                       "cv" = cv,
                       "asimetría" = asimetria,
                       "kurtosis" = kurtosis,
                       "NAs" = NAs,
                       "clase" = clases)
  
  # Asignar el nombre de las variables (columnas) 
  # como nombre de las filas para el objeto de salida
  rownames(summary) <- colnames(df)
  
  # Retorno de la función
  return(summary)
  
}

# Crear objeto con las principales estadísticas y mostrarlo en consola
(resumenDatosRindeMaiz <- ResumenEstadisticas(df = datosRindeMaiz@data))


# Normalidad --------------------------------------------------------------

# Elevación
shapiro.test(datosRindeMaiz$Elev) 

# Profundidad de la napa (C1)
shapiro.test(datosRindeMaiz$NapaDicC1)

# Profundidad de la napa (C2)
shapiro.test(datosRindeMaiz$NapaDicC2)

# Rendimiento del maíz (C1)
shapiro.test(datosRindeMaiz$maizC1) 

# Rendimiento del maíz (C2)
shapiro.test(datosRindeMaiz$maizC2)


# Transformaciones --------------------------------------------------------

# Transformar elevación

# Logaritmo
ElevLog <- log(datosRindeMaiz@data$Elev)
shapiro.test(ElevLog) # Probar normalidad de la transformación

# Raíz cuadrada
ElevSqrt <- sqrt(datosRindeMaiz@data$Elev)
shapiro.test(ElevSqrt) # Probar normalidad de la transformación

# Box-Cox
lambdaElev <- BoxCox.lambda(x = datosRindeMaiz@data$Elev) # Hallar Lambda
ElevBC <- BoxCox(x = datosRindeMaiz@data$Elev, lambda = lambdaElev) # Transf. vector
shapiro.test(ElevBC) # Probar normalidad de la transformación 

# Transformar profundidad napa (C1)

# Logaritmo
NapaDicC1Log <- log(datosRindeMaiz@data$NapaDicC1)
shapiro.test(NapaDicC1Log) 

# Raíz cuadrada
NapaDicC1Sqrt <- sqrt(datosRindeMaiz@data$NapaDicC1)
shapiro.test(NapaDicC1Sqrt) 

# Box-Cox
lambdaNapaDicC1 <- BoxCox.lambda(x = datosRindeMaiz@data$NapaDicC1) 
NapaDicC1BC <- BoxCox(x = datosRindeMaiz@data$NapaDicC1, lambda = lambdaNapaDicC1) 
shapiro.test(NapaDicC1BC)  

# Transformar profundidad napa (C2)

# Logaritmo
NapaDicC2Log <- log(datosRindeMaiz@data$NapaDicC2)
shapiro.test(NapaDicC2Log) 

# Raíz cuadrada
NapaDicC2Sqrt <- sqrt(datosRindeMaiz@data$NapaDicC2)
shapiro.test(NapaDicC2Sqrt) 

# Box-Cox
lambdaNapaDicC2 <- BoxCox.lambda(x = datosRindeMaiz@data$NapaDicC2) 
NapaDicC2BC <- BoxCox(x = datosRindeMaiz@data$NapaDicC2, lambda = lambdaNapaDicC2) 
shapiro.test(NapaDicC2BC)  

# Transformar rendimiento maíz (C1)

# Logaritmo
maizC1Log <- log(datosRindeMaiz@data$maizC1)
shapiro.test(maizC1Log) 

# Raíz cuadrada
maizC1Sqrt <- sqrt(datosRindeMaiz@data$maizC1)
shapiro.test(maizC1Sqrt) 

# Box-Cox
lambdaMaizC1Log <- BoxCox.lambda(x = datosRindeMaiz@data$maizC1) 
maizC1BC <- BoxCox(x = datosRindeMaiz@data$maizC1, lambda = lambdaMaizC1Log) 
shapiro.test(maizC1BC) 

# Transformar rendimiento maíz (C2)

# Logaritmo
maizC2Log <- log(datosRindeMaiz@data$maizC2)
shapiro.test(maizC2Log) 

# Raíz cuadrada
maizC2Sqrt <- sqrt(datosRindeMaiz@data$maizC2)
shapiro.test(maizC2Sqrt) 

# Box-Cox
lambdaMaizC2Log <- BoxCox.lambda(x = datosRindeMaiz@data$maizC2) 
maizC2BC <- BoxCox(x = datosRindeMaiz@data$maizC2, lambda = lambdaMaizC2Log) 
shapiro.test(maizC2BC) 


# Q-Q plots ---------------------------------------------------------------

# Q-Q plot rendimiento maíz (C1)
ggQQmaizC1 <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = maizC1)) +
  geom_abline(intercept = mean(datosRindeMaiz@data$maizC1, na.rm = TRUE), 
              slope = sd(datosRindeMaiz@data$maizC1, na.rm = TRUE), col = "red") +
  labs(title = "Rend.maíz C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC1Log <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = log(maizC1))) +
  geom_abline(intercept = mean(log(datosRindeMaiz@data$maizC1), na.rm = TRUE), 
              slope = sd(log(datosRindeMaiz@data$maizC1), na.rm = TRUE), col = "red") + 
  labs(title = "Log Rend.maíz C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC1Sqrt <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = sqrt(maizC1))) +
  geom_abline(intercept = mean(sqrt(datosRindeMaiz@data$maizC1), na.rm = TRUE), 
              slope = sd(sqrt(datosRindeMaiz@data$maizC1), na.rm = TRUE), col = "red") +
  labs(title = "Raíz cuadrada Rend.maíz C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC1BC <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = maizC1BC)) +
  geom_abline(intercept = mean(maizC1BC, na.rm = TRUE), 
              slope = sd(maizC1BC, na.rm = TRUE), col = "red") +
  labs(title = "Box-Cox Rend.maíz C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

# Q-Q plot rendimiento maíz (C2)
ggQQmaizC2 <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = maizC2)) +
  geom_abline(intercept = mean(datosRindeMaiz@data$maizC2, na.rm = TRUE), 
              slope = sd(datosRindeMaiz@data$maizC2, na.rm = TRUE), col = "red") +
  labs(title = "Rend.maíz C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC2Log <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = log(maizC2))) +
  geom_abline(intercept = mean(log(datosRindeMaiz@data$maizC2), na.rm = TRUE), 
              slope = sd(log(datosRindeMaiz@data$maizC2), na.rm = TRUE), col = "red") + 
  labs(title = "Log Rend.maíz C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC2Sqrt <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = sqrt(maizC2))) +
  geom_abline(intercept = mean(sqrt(datosRindeMaiz@data$maizC2), na.rm = TRUE), 
              slope = sd(sqrt(datosRindeMaiz@data$maizC2), na.rm = TRUE), col = "red") +
  labs(title = "Raíz cuadrada Rend.maíz C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQmaizC2BC <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = maizC2BC)) +
  geom_abline(intercept = mean(maizC2BC, na.rm = TRUE), 
              slope = sd(maizC2BC, na.rm = TRUE), col = "red") +
  labs(title = "Box-Cox Rend.maíz C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

# Q-Q plot profundidad de la napa (C1)
ggQQNapaDicC1 <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = NapaDicC1)) +
  geom_abline(intercept = mean(datosRindeMaiz@data$NapaDicC1), 
              slope = sd(datosRindeMaiz@data$NapaDicC1), col = "red") +
  labs(title = "Prof.napa C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC1Log <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = log(NapaDicC1))) +
  geom_abline(intercept = mean(log(datosRindeMaiz@data$NapaDicC1)), 
              slope = sd(log(datosRindeMaiz@data$NapaDicC1)), col = "red") + 
  labs(title = "Log Prof.napa C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC1Sqrt <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = sqrt(NapaDicC1))) +
  geom_abline(intercept = mean(sqrt(datosRindeMaiz@data$NapaDicC1)), 
              slope = sd(sqrt(datosRindeMaiz@data$NapaDicC1)), col = "red") +
  labs(title = "Raíz cuadrada Prof.napa C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC1BC <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = NapaDicC1BC)) +
  geom_abline(intercept = mean(NapaDicC1BC, na.rm = TRUE), 
              slope = sd(NapaDicC1BC, na.rm = TRUE), col = "red") +
  labs(title = "Box-Cox Prof.napa C1", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

# Q-Q plot profundidad de la napa (C2)
ggQQNapaDicC2 <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = NapaDicC2)) +
  geom_abline(intercept = mean(datosRindeMaiz@data$NapaDicC2), 
              slope = sd(datosRindeMaiz@data$NapaDicC2), col = "red") +
  labs(title = "Prof.napa C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC2Log <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = log(NapaDicC2))) +
  geom_abline(intercept = mean(log(datosRindeMaiz@data$NapaDicC2)), 
              slope = sd(log(datosRindeMaiz@data$NapaDicC2)), col = "red") + 
  labs(title = "Log Prof.napa C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC2Sqrt <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = sqrt(NapaDicC2))) +
  geom_abline(intercept = mean(sqrt(datosRindeMaiz@data$NapaDicC2)), 
              slope = sd(sqrt(datosRindeMaiz@data$NapaDicC2)), col = "red") +
  labs(title = "Raíz cuadrada Prof.napa C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQNapaDicC2BC <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = NapaDicC2BC)) +
  geom_abline(intercept = mean(NapaDicC2BC, na.rm = TRUE), 
              slope = sd(NapaDicC2BC, na.rm = TRUE), col = "red") +
  labs(title = "Box-Cox Prof.napa C2", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

# Q-Q plot elevación
ggQQElev <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = Elev)) +
  geom_abline(intercept = mean(datosRindeMaiz@data$Elev), 
              slope = sd(datosRindeMaiz@data$Elev), col = "red") +
  labs(title = "Elevación", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQElevLog <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = log(Elev))) +
  geom_abline(intercept = mean(log(datosRindeMaiz@data$Elev)), 
              slope = sd(log(datosRindeMaiz@data$Elev)), col = "red") + 
  labs(title = "Log Elevación", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQElevSqrt <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = sqrt(Elev))) +
  geom_abline(intercept = mean(sqrt(datosRindeMaiz@data$Elev)), 
              slope = sd(sqrt(datosRindeMaiz@data$Elev)), col = "red") +
  labs(title = "Raíz cuadrada Elevación", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

ggQQElevBC <- ggDatosRindeMaiz + 
  geom_qq(aes(sample = ElevBC)) +
  geom_abline(intercept = mean(ElevBC), slope = sd(ElevBC), col = "red") +
  labs(title = "Box-Cox Elevación", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")

# Ver plots 
X11()
grid.arrange(ggQQmaizC1, ggQQmaizC2, ggQQNapaDicC1, ggQQNapaDicC2, ggQQElev,
             ggQQmaizC1Log, ggQQmaizC2Log, ggQQNapaDicC1Log, ggQQNapaDicC2Log, ggQQElevLog,
             ggQQmaizC1Sqrt, ggQQmaizC2Sqrt, ggQQNapaDicC1Sqrt, ggQQNapaDicC2Sqrt, ggQQElevSqrt,
             ggQQmaizC1BC, ggQQmaizC2BC, ggQQNapaDicC1BC, ggQQNapaDicC2BC, ggQQElevBC,
             ncol = 4, nrow = 5, 
             layout_matrix = cbind(c(1,2,3,4,5), c(6,7,8,9,10), c(11,12,13,14,15), c(16,17,18,19,20))
)


# Análisis de tendencia ---------------------------------------------------

# Rendimiento maíz vs Longitud
ggTend_MaizC1VsLong <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC1, x = datosRindeMaiz@coords[,"Long"])) + 
  geom_smooth(aes(y = maizC1, x = datosRindeMaiz@coords[,"Long"])) + 
  labs(title = "", x = "Longitud", y = "Rendimiento maíz C1 (t/ha)")

ggTend_MaizC2VsLong <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC2, x = datosRindeMaiz@coords[,"Long"])) + 
  geom_smooth(aes(y = maizC2, x = datosRindeMaiz@coords[,"Long"])) + 
  labs(title = "", x = "Longitud", y = "Rendimiento maíz C2 (t/ha)")

# Rendimiento maíz vs Latitud
ggTend_MaizC1VsLat <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC1, x = datosRindeMaiz@coords[,"Lat"])) + 
  geom_smooth(aes(y = maizC1, x = datosRindeMaiz@coords[,"Lat"])) + 
  labs(title = "", x = "Latitud", y = "Rendimiento maíz C1 (t/ha)")

ggTend_MaizC2VsLat <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC2, x = datosRindeMaiz@coords[,"Lat"])) + 
  geom_smooth(aes(y = maizC2, x = datosRindeMaiz@coords[,"Lat"])) + 
  labs(title = "", x = "Latitud", y = "Rendimiento maíz C2 (t/ha)")

# Rendimiento maíz vs Elevación
ggTend_MaizC1VsElev <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC1, x = Elev)) + 
  geom_smooth(aes(y = maizC1, x = Elev)) + 
  labs(title = "", y = "Rendimiento maíz C1 (t/ha)", x = "Elevación (m)")

ggTend_MaizC2VsElev <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC2, x = Elev)) + 
  geom_smooth(aes(y = maizC2, x = Elev)) + 
  labs(title = "", y = "Rendimiento maíz C2 (t/ha)", x = "Elevación (m)")

# Rendimiento maíz vs Profundidad de la napa
ggTend_MaizC1VsNapaDicC1 <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC1, x = NapaDicC1)) + 
  geom_smooth(aes(y = maizC1, x = NapaDicC1)) + 
  labs(title = "", y = "Rendimiento maíz C1 (t/ha)", x = "Profundidad napa (m)")

ggTend_MaizC2VsNapaDicC2 <- ggDatosRindeMaiz +
  geom_point(aes(y = maizC2, x = NapaDicC2)) + 
  geom_smooth(aes(y = maizC2, x = NapaDicC2)) + 
  labs(title = "", y = "Rendimiento maíz C2 (t/ha)", x = "Profundidad napa (m)")

X11()
# Ver plots 
# grid.arrange(,
#              ncol = 4, nrow = 5, 
#              layout_matrix = cbind(c(1,2,3,4,5), c(6,7,8,9,10), c(11,12,13,14,15), c(16,17,18,19,20))
#              )

fitVarC1.Sph <- fit.variogram.gls(formula = maizC1 ~ NapaDicC1 + Elev,
                                  data = datosRindeMaiz[sampleC1,],
                                  model = modTeoSph.C1,
                                  cutoff = 400)

fitVarC1.Exp <- fit.variogram.gls(formula = maizC1 ~ NapaDicC1 + Elev,
                                  data = datosRindeMaiz[sampleC1,],
                                  model = modTeoExp.C1,
                                  cutoff = 400)

fitVarC1.Cir <- fit.variogram.gls(formula = maizC1 ~ NapaDicC1 + Elev,
                                  data = datosRindeMaiz[sampleC1,],
                                  model = modTeoCir.C1,
                                  cutoff = 400)

# Tomar muestra aleatoria de 1000 puntos
# sampleC1 <- sample(datosRindeMaiz$ID[-c(NAs, outliersC1)], size = 1000)

# geoR
intOK.C1 <- krige.control(type.krige = "OK", 
                          trend.d = trend.spatial(data~NapaDicC1+Elev, geodataC1),
                          trend.l = trend.spatial(data~NapaDicC1+Elev, geodataC1),
                          obj.model = likfitVarGeoC1.powexp)
intOK.C1b <- krige.conv(geodataC1, coords=geodataC1$coords, data=geodataC1$data,
                        locations=coordinates(puntosPredecirC1), krige=intOK.C1)

image(intOK.C1, puntosPredecirC1)



# geoR --------------------------------------------------------------------

# Matriz de posibles parámetros iniciales de rango y meseta para C1
iniCovParsC1 <- cbind("sigma" = seq(1, 3, length.out = 20),
                      "phi" = seq(60, 240, length.out = 20))

iniCovParsC1 <- as.matrix(iniCovParsC1) # convertir a matriz

# Ajuste modelo exponencial
# likfitVarGeoC1.exp <- likfit(geodataC1, 
#                              trend = trend.spatial(data~NapaDicC1+Elev, geodataC1), 
#                              ini.cov.pars = iniCovParsC1,
#                              fix.nugget = TRUE,
#                              nugget = 0,
#                              cov.model = "exponential", 
#                              lik.method = "REML")

# Ajuste modelo esférico
# likfitVarGeoC1.sph <- likfit(geodataC1, 
#                              trend = trend.spatial(data~NapaDicC1+Elev, geodataC1), 
#                              ini.cov.pars = iniCovParsC1,
#                              limits = pars.limits(phi = c(200,250), sigmasq = c(1.7,1.9)), 
#                              fix.nugget = TRUE,
#                              nugget = 0,
#                              cov.model = "spherical", 
#                              lik.method = "REML")

# Ajuste modelo potencia exponencial
# likfitVarGeoC1.powexp <- likfit(geodataC1, 
#                              trend = trend.spatial(data~NapaDicC1+Elev, geodataC1), 
#                              ini.cov.pars = iniCovParsC1,
#                              fix.nugget = TRUE,
#                              fix.kappa = FALSE,
#                              nugget = 0,
#                              cov.model = "powered.exponential", 
#                              lik.method = "REML")

# Ajuste modelo circular
likfitVarGeoC1.cir <- likfit(geodataC1,
                             trend = trend.spatial(data~NapaDicC1+Elev, geodataC1),
                             ini.cov.pars = iniCovParsC1,
                             fix.nugget = FALSE,
                             cov.model = "circular",
                             lik.method = "REML")

# Plot variograma empírico + modelos
plot(varGeoC1, main = "C1",
     ylab = "semivarianza", 
     xlab = "distancia",  pch = 19, col = "#0080FF", ylim = c(0,4))
lines.variomodel(likfitVarGeoC1.exp, col = "red", lty = 1)
lines.variomodel(likfitVarGeoC1.sph, col = "red", lty = 2)
lines.variomodel(likfitVarGeoC1.powexp, col = "red", lty = 3)
grid()
legend('topleft', lty = c(1,2,3), legend = c('Exp','Esf','PotExp'), col = rep("red",3))


# gstat fit variogram -----------------------------------------------------

fitVarOmniC1.1 <- fit.variogram(object = varOmniC1, model = modTeoSph.C1, fit.method = 7)

# Ajustar modelo teórico a variograma empírico
# mediante Máxima Verosimilitud Restringida (REML) 

## Ajuste del Modelo Esférico

fitVarC1.Sph.REML <- fit.variogram.reml(
  formula = maizC1 ~ NapaDicC1 + Elev,
  data = datosRindeMaiz[-c(NAs, outliersC1),],
  model = vgm(psill=1.7, model="Sph", range=80, nugget=0),
  debug.level = 65)
# Resultado: Negative log-likelyhood: 1160.19

fitVarC2.Sph.REML <- fit.variogram.reml(
  formula = maizC2 ~ NapaDicC2 + Elev,
  data = datosRindeMaiz[-outliersC2,],
  model = vgm(psill=0.6, model="Sph", range=150, nugget=0),
  debug.level = 65) 
# Resultado: Negative log-likelyhood: 

## Ajuste del Modelo Exponencial

fitVarC1.Exp.REML <- fit.variogram.reml(
  formula = maizC1 ~ NapaDicC1 + Elev,
  data = datosRindeMaiz[-c(NAs, outliersC1),],
  model = vgm(psill=1.7, model="Exp", range=80, nugget=0),
  debug.level = 65)
# Resultado: Negative log-likelyhood: -1197.16

fitVarC2.Exp.REML <- fit.variogram.reml(
  formula = maizC2 ~ NapaDicC2 + Elev,
  data = datosRindeMaiz[-outliersC2,],
  model = vgm(psill=0.8, model="Exp", range=100, nugget=0),
  debug.level = 65)
# Resultado: Negative log-likelyhood: 

## Ajuste del Modelo Circular

likfitVarGeoC1.cir
# likfit: estimated model parameters:
#      beta0      beta1      beta2      tausq    sigmasq        phi 
# "  6.2981" " -2.3431" "  0.0503" "  0.1375" "  1.8341" "116.0787" 
# Practical Range with cor=0.05 for asymptotic range: 116.0787
# 
# likfit: maximised log-likelihood = -2559

fitVarC1.Cir.REML <- fit.variogram.reml(
  formula = maizC1 ~ NapaDicC1 + Elev,
  data = datosRindeMaiz[-c(NAs, outliersC1),],
  model = vgm(psill=1.8341, model="Cir", range=116.0787, nugget=0.1375),
  debug.level = 65)
# Resultado: Negative log-likelyhood: -1105.18 (modelo elegido)

fitVarC2.Cir.REML <- fit.variogram.reml(
  formula = maizC2 ~ NapaDicC2 + Elev,
  data = datosRindeMaiz[-outliersC2,],
  model = vgm(psill=1, model="Cir", range=150, nugget=0),
  debug.level = 65)
# Resultado: Negative log-likelyhood: 


