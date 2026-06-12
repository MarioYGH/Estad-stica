#Práctica de Estadística Descriptiva
## 10 de Junio de 2026

#Descargar archivo
url <- "https://drive.google.com/uc?export=download&id=1nPBaZebW-t0muRY12U5e-ag70wQOSMUA"
SALUD <- read.csv(url)
View(SALUD)

#¿Qué columnas tiene el archivo SALUD?
names(SALUD)

#¿De qué tipo es el archivo SALUD?
class(SALUD)

#¿Cómo está estructurado este data.frame?
str(SALUD)

#Instalar paquetería para análisis de datos
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("moments")
install.packages("psych")

# Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(psych)

#Generando estadísticas descriptivas básicas de la variable EDAD
describe(SALUD$Edad)

#Crear función para calcular la moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Crear una tabla con estadísticas descriptivas de la variable EDAD
EDAD <- SALUD$Edad

media <- mean(EDAD, na.rm = TRUE)
mediana <- median(EDAD, na.rm = TRUE)
moda <- moda(EDAD)
varianza <- var(EDAD, na.rm = TRUE)
des.est <- sd(EDAD, na.rm = TRUE)
minimo <- min(EDAD, na.rm = TRUE)
maximo <- max(EDAD, na.rm = TRUE)
rango <- maximo - minimo
cv <- des.est / media * 100
cuartiles <- quantile(EDAD, na.rm = TRUE)
ric <- IQR(EDAD, na.rm = TRUE)
asimetria <- skewness(EDAD, na.rm = TRUE)
curtosis_valor <- kurtosis(EDAD, na.rm = TRUE)

TABLA <- data.frame(
  Media = media,
  Mediana = mediana,
  Moda= moda,
  Varianza = varianza,
  Desviacion_Estandar = des.est,
  Minimo = minimo,
  Maximo = maximo,
  Rango = rango,
  Coeficiente_Variacion = cv,
  Rango_Intercuartilico = ric,
  Asimetria = asimetria,
  Curtosis = curtosis_valor
)

View(TABLA)

#Guardar tabla en archivo csv

write.csv(TABLA, "ANALISIS_EDAD.csv", row.names = FALSE)

#Para saber dónde se guardó el archivo
getwd()

#Analizando la variable EDAD en función del Género
EDAD_GENERO<-SALUD %>%
  group_by(Genero) %>%
  summarise(
    media = mean(Edad, na.rm = TRUE),
    mediana = median(Edad, na.rm = TRUE),
    desviacion = sd(Edad, na.rm = TRUE),
    varianza = var(Edad, na.rm = TRUE),
    minimo = min(Edad, na.rm = TRUE),
    maximo = max(Edad, na.rm = TRUE),
    rango = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
    n = n()
  )

write.csv(EDAD_GENERO, "EDAD_VS_GENERO.csv", row.names = FALSE)

#Analizando la variable PULSO CARDÍACO en función del Género

RITMO_GENERO<-SALUD %>%
  group_by(Genero) %>%
  summarise(
    media = mean(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    mediana = median(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    desviacion = sd(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    varianza = var(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    minimo = min(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    maximo = max(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    rango = max(Ritmo_Cardiaco_LPM, na.rm = TRUE) - min(Ritmo_Cardiaco_LPM, na.rm = TRUE),
    n = n()
  )

write.csv(RITMO_GENERO, "RITMO_CARDIACO_VS_GENERO.csv", row.names = FALSE)

#Generando un HISTOGRAMA para la variable EDAD

hist(EDAD)

#Dando mayores detalles al histograma

hist(
  EDAD,
  breaks = 3,
  main = "Histograma de EDAD",
  xlab = "Edad (años)",
  col="blue",
)

#Creando un gráfico de cajas para la variable EDAD
boxplot(
  EDAD,
  main = "Diagrama de cajas de edad",
  ylab = "Edad",
  col = "lightblue"
)

#Creando un gráfico de cajas para la variable EDAD, por género.

boxplot(
  Edad ~ Genero,
  data = SALUD ,
  main = "Edad por género",
  xlab = "Género",
  ylab = "Edad",
  col = "lightgreen"
)
  
  #Creando un gráfico de cajas para la variable RITMO CARDÍACO, por fumador.
  
  boxplot(
    Ritmo_Cardiaco_LPM ~ Fumador,
    data = SALUD ,
    main = "Ritmo cardíaco en función del hábito de fumar",
    xlab = "Fumador",
    ylab = "Ritmo cardíaco",
    col = "purple"
)

#Generar una gráfica de Presión Sistólica contra la edad
  
  plot(
    SALUD$Edad,
    SALUD$Presion_Sistolica,
    main = "Relación entre presión sistólica y la edad",
    xlab = "Edad (años)",
    ylab = "Presión Sistólica",
    col="red"
  )

  #Generar una gráfica de Presión Sistólica contra la Presión Diastólica
  
  plot(
    SALUD$Presion_Diastolica,
    SALUD$Presion_Sistolica,
    main = "Relación entre presión sistólica y presión diastólica",
    xlab = "Presión diastólica",
    ylab = "Presión Sistólica",
    col="orange"
  )
  
  #Haciendo un resumen de variables cualitativas
  
  TABLA_GENERO<-table(SALUD$Genero)
  TABLA_GENERO
  
  TABLA_HABITO_FUMAR<-table(SALUD$Fumador)
  TABLA_HABITO_FUMAR

  #Haciendo un gráfico de sectores para las variables cualitativas
  pie(TABLA_GENERO)
  pie(TABLA_HABITO_FUMAR)
  
  #Haciendo un gráfico de barras para las variables cualiativas
  barplot(TABLA_GENERO)
  barplot(TABLA_HABITO_FUMAR)
  
  #Presentando dos gráficas en una sola ventana y editando colores
  par(mfrow = c(1, 2))
  pie(TABLA_GENERO, col=c("blue","pink"))
  pie(TABLA_HABITO_FUMAR, col=c("black","yellow"))
  
  par(mfrow = c(1, 2))
  barplot(TABLA_GENERO, col=c("blue","pink"))
  barplot(TABLA_HABITO_FUMAR, col=c("black","yellow"))
  
#¿En qué variable se tiene mayor dispersión?
  
  #Creamos un data.frame únicamente con las variables cuantitativas

SALUD1<-data.frame(
  EDAD=SALUD$Edad,
  SIST=SALUD$Presion_Sistolica,
  DIAST=SALUD$Presion_Diastolica,
  RITMO=SALUD$Ritmo_Cardiaco_LPM,
  GLUCOSA=SALUD$Glucosa_mg_dL,
  COL_HDL=SALUD$Colesterol_HDL_Bueno,
  COL_LDL=SALUD$Colesterol_LDL_Malo)

View(SALUD1)

 #Calculamos el coeficiente de variación para cada variable

CV<-data.frame(
  Media=colMeans(SALUD1),
  Desv.Est=sapply(SALUD1, sd),
  COEFICIENTE=(Desv.Est/Media)*100
)

View(CV)

#¿Qué tipo de sesgo y curtosis tiene cada variable?

SESGO_CURTOSIS<-data.frame(SESGO=skewness(SALUD1),CURTOSIS=kurtosis(SALUD1))

View(SESGO_CURTOSIS)
