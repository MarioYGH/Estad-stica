# ------------------------------------------------------------
# Análisis descriptivo de base de datos ECG - Arrhythmia
# ------------------------------------------------------------
# Mario Yahir García Hernández 
# Curso propedéutico Maestría CD - Estadística
# 11/06/2026

# ------------------------------------------------------------
# 1. Cargar paquetes
# ------------------------------------------------------------

paquetes <- c("ggplot2", "moments", "corrplot")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ------------------------------------------------------------
# 2. Cargar base de datos
# ------------------------------------------------------------

ECG <- read.csv("C:/Users/mygarcia/Downloads/García_Hernández_MY_arrhythmia.csv", header = FALSE, na.strings = "?")

# Ver estructura general
dim(ECG)
str(ECG)
head(ECG)

# ------------------------------------------------------------
# 3. Seleccionar variables principales para el análisis
# ------------------------------------------------------------

# Nota:
# La base original tiene muchas columnas.
# Para este análisis se toman variables clínicas y ECG fáciles de interpretar.

ECG_analisis <- data.frame(
  Edad = ECG[, 1],
  Sexo = ECG[, 2],
  Altura = ECG[, 3],
  Peso = ECG[, 4],
  Duracion_QRS = ECG[, 5],
  Intervalo_PR = ECG[, 6],
  Intervalo_QT = ECG[, 7],
  Intervalo_T = ECG[, 8],
  Intervalo_P = ECG[, 9],
  Frecuencia_cardiaca = ECG[, 15],
  Diagnostico = ECG[, 280]
)

# Convertir variables cualitativas a factor para poder graficarlas mejor
ECG_analisis$Sexo <- factor(
  ECG_analisis$Sexo,
  levels = c(0, 1),
  labels = c("Masculino", "Femenino")
)

ECG_analisis$Diagnostico <- factor(ECG_analisis$Diagnostico)

# Revisar variables seleccionadas
names(ECG_analisis)
str(ECG_analisis)

# ------------------------------------------------------------
# 4. Descripción breve de la base
# ------------------------------------------------------------

# Esta base contiene mediciones derivadas de electrocardiogramas.
# La última columna representa el diagnóstico o tipo de arritmia.
# La clase 1 representa un ECG normal.
# Las clases 2 a 16 representan distintos tipos de alteraciones cardíacas.

# ------------------------------------------------------------
# 5. Variables cuantitativas y cualitativas
# ------------------------------------------------------------

variables_cuantitativas <- c(
  "Edad",
  "Altura",
  "Peso",
  "Duracion_QRS",
  "Intervalo_PR",
  "Intervalo_QT",
  "Intervalo_T",
  "Intervalo_P",
  "Frecuencia_cardiaca"
)

variables_cualitativas <- c(
  "Sexo",
  "Diagnostico"
)

# ------------------------------------------------------------
# 6. Resumen descriptivo general
# ------------------------------------------------------------

summary(ECG_analisis[, variables_cuantitativas])

summary(ECG_analisis[, variables_cualitativas])

# ------------------------------------------------------------
# 7. Tabla de curtosis
# ------------------------------------------------------------

tabla_curtosis <- data.frame(
  Variable = variables_cuantitativas,
  Curtosis = NA,
  Interpretacion = NA
)

for (i in 1:length(variables_cuantitativas)) {
  
  variable <- variables_cuantitativas[i]
  valores <- ECG_analisis[[variable]]
  
  k <- kurtosis(valores, na.rm = TRUE)
  
  tabla_curtosis$Curtosis[i] <- round(k, 3)
  
  if (k < 3) {
    tabla_curtosis$Interpretacion[i] <- "Curtosis menor a 3: menor apuntalamiento"
  } else if (k == 3) {
    tabla_curtosis$Interpretacion[i] <- "Curtosis cercana a 3: similar a normal"
  } else {
    tabla_curtosis$Interpretacion[i] <- "Curtosis mayor a 3: mayor apuntalamiento"
  }
}

tabla_curtosis

# ------------------------------------------------------------
# 8. Histogramas de algunas variables cuantitativas
# ------------------------------------------------------------

# Se grafican solo algunas variables para no saturar el análisis.

ggplot(ECG_analisis, aes(x = Edad)) +
  geom_histogram(bins = 25, color = "black", fill = "lightblue") +
  labs(
    title = "Distribución de la edad",
    x = "Edad",
    y = "Frecuencia"
  )

ggplot(ECG_analisis, aes(x = Duracion_QRS)) +
  geom_histogram(bins = 25, color = "black", fill = "lightgreen") +
  labs(
    title = "Distribución de la duración QRS",
    x = "Duración QRS",
    y = "Frecuencia"
  )

ggplot(ECG_analisis, aes(x = Intervalo_QT)) +
  geom_histogram(bins = 25, color = "black", fill = "lightpink") +
  labs(
    title = "Distribución del intervalo QT",
    x = "Intervalo QT",
    y = "Frecuencia"
  )

ggplot(ECG_analisis, aes(x = Frecuencia_cardiaca)) +
  geom_histogram(bins = 25, color = "black", fill = "lightgray") +
  labs(
    title = "Distribución de la frecuencia cardiaca",
    x = "Frecuencia cardiaca",
    y = "Frecuencia"
  )

# ------------------------------------------------------------
# 9. Gráficos de variables cualitativas
# ------------------------------------------------------------

ggplot(ECG_analisis, aes(x = Sexo)) +
  geom_bar(fill = "coral", color = "black") +
  labs(
    title = "Distribución por sexo",
    x = "Sexo",
    y = "Cantidad de registros"
  )

ggplot(ECG_analisis, aes(x = Diagnostico)) +
  geom_bar(fill = "gold", color = "black") +
  labs(
    title = "Distribución de diagnósticos",
    x = "Clase diagnóstica",
    y = "Cantidad de registros"
  )

# ------------------------------------------------------------
# 10. Correlación entre variables cuantitativas
# ------------------------------------------------------------

matriz_correlacion <- cor(
  ECG_analisis[, variables_cuantitativas],
  use = "complete.obs"
)

matriz_correlacion

corrplot(
  matriz_correlacion,
  method = "color",
  type = "upper",
  tl.cex = 0.8,
  addCoef.col = "black",
  number.cex = 0.6
)

# ------------------------------------------------------------
# 11. Relación entre variables ECG
# ------------------------------------------------------------

ggplot(ECG_analisis, aes(x = Intervalo_QT, y = Frecuencia_cardiaca)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre intervalo QT y frecuencia cardiaca",
    x = "Intervalo QT",
    y = "Frecuencia cardiaca"
  )

ggplot(ECG_analisis, aes(x = Duracion_QRS, y = Frecuencia_cardiaca)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre duración QRS y frecuencia cardiaca",
    x = "Duración QRS",
    y = "Frecuencia cardiaca"
  )

# ------------------------------------------------------------
# 12. Comparación de frecuencia cardiaca por diagnóstico
# ------------------------------------------------------------

ggplot(ECG_analisis, aes(x = Diagnostico, y = Frecuencia_cardiaca)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Frecuencia cardiaca por diagnóstico",
    x = "Diagnóstico",
    y = "Frecuencia cardiaca"
  )

# ------------------------------------------------------------
# 13. Comparación de duración QRS por diagnóstico
# ------------------------------------------------------------

ggplot(ECG_analisis, aes(x = Diagnostico, y = Duracion_QRS)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Duración QRS por diagnóstico",
    x = "Diagnóstico",
    y = "Duración QRS"
  )

# ------------------------------------------------------------
# 14 Estadísticos descriptivos detallados
# ------------------------------------------------------------

# Función para calcular la moda
moda <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tabla_descriptiva <- data.frame(
  Variable = character(),
  Media = numeric(),
  Mediana = numeric(),
  Moda = numeric(),
  Desv_Estandar = numeric(),
  P25 = numeric(),
  P50 = numeric(),
  P75 = numeric()
)

for(variable in variables_cuantitativas){
  
  valores <- ECG_analisis[[variable]]
  
  tabla_descriptiva <- rbind(
    tabla_descriptiva,
    data.frame(
      Variable = variable,
      Media = round(mean(valores, na.rm = TRUE),2),
      Mediana = round(median(valores, na.rm = TRUE),2),
      Moda = round(moda(valores),2),
      Desv_Estandar = round(sd(valores, na.rm = TRUE),2),
      P25 = round(quantile(valores,0.25,na.rm=TRUE),2),
      P50 = round(quantile(valores,0.50,na.rm=TRUE),2),
      P75 = round(quantile(valores,0.75,na.rm=TRUE),2)
    )
  )
}

tabla_descriptiva

# ------------------------------------------------------------
# Estadísticos descriptivos de Frecuencia Cardiaca
# ------------------------------------------------------------

frecuencia <- ECG_analisis$Frecuencia_cardiaca

media_fc <- mean(frecuencia, na.rm = TRUE)
mediana_fc <- median(frecuencia, na.rm = TRUE)
moda_fc <- moda(frecuencia)
sd_fc <- sd(frecuencia, na.rm = TRUE)

percentiles_fc <- quantile(
  frecuencia,
  probs = c(0.25,0.50,0.75),
  na.rm = TRUE
)

media_fc
mediana_fc
moda_fc
sd_fc
percentiles_fc
