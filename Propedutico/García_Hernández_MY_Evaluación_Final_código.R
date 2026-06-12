# ------------------------------------------------------------
# Análisis descriptivo de base de datos HOUSES
# ------------------------------------------------------------
# Mario Yahir García Hernández
# Curso propedéutico Maestría CD - Estadística
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1. Cargar paquetes
# ------------------------------------------------------------

paquetes <- c("ggplot2", "moments", "dplyr")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ------------------------------------------------------------
# 2. Cargar base de datos
# ------------------------------------------------------------

CASAS <- read.csv("C:/Users/mygarcia/Downloads/HOUSES.csv", fileEncoding = "latin1")

# Exploración inicial

dim(CASAS)
str(CASAS)
head(CASAS)

# ------------------------------------------------------------
# 3. Seleccionar variables para análisis
# ------------------------------------------------------------

CASAS_analisis <- data.frame(
  Habitaciones = CASAS$Habitaciones..X1.,
  Banos = CASAS$Baños..X2.,
  Estrato = CASAS$Estrato..X3.,
  Antiguedad = CASAS$Antigüedad..X4.,
  Area_Construida = CASAS$Área.Construida..X5.,
  Precio = CASAS$Precio..Y.
)

variables_cuantitativas <- names(CASAS_analisis)

# ------------------------------------------------------------
# 4. Tipo de apuntalamiento (Curtosis)
# ------------------------------------------------------------

tabla_curtosis <- data.frame(
  Variable = variables_cuantitativas,
  Curtosis = NA,
  Tipo = NA
)

for(i in 1:length(variables_cuantitativas)){
  
  variable <- variables_cuantitativas[i]
  
  k <- kurtosis(
    CASAS_analisis[[variable]],
    na.rm = TRUE
  )
  
  tabla_curtosis$Curtosis[i] <- round(k,3)
  
  if(k < 3){
    
    tabla_curtosis$Tipo[i] <- "Platicurtica"
    
  } else if(k > 3){
    
    tabla_curtosis$Tipo[i] <- "Leptocurtica"
    
  } else {
    
    tabla_curtosis$Tipo[i] <- "Mesocurtica"
    
  }
}

tabla_curtosis

# ------------------------------------------------------------
# 5. Tipo de sesgo (Asimetría)
# ------------------------------------------------------------

tabla_sesgo <- data.frame(
  Variable = variables_cuantitativas,
  Asimetria = NA,
  Tipo = NA
)

for(i in 1:length(variables_cuantitativas)){
  
  variable <- variables_cuantitativas[i]
  
  s <- skewness(
    CASAS_analisis[[variable]],
    na.rm = TRUE
  )
  
  tabla_sesgo$Asimetria[i] <- round(s,3)
  
  if(s > 0){
    
    tabla_sesgo$Tipo[i] <- "Positivo"
    
  } else if(s < 0){
    
    tabla_sesgo$Tipo[i] <- "Negativo"
    
  } else {
    
    tabla_sesgo$Tipo[i] <- "Simetria"
    
  }
}

tabla_sesgo

# ------------------------------------------------------------
# 6 Diagramas de caja y bigotes
# ------------------------------------------------------------

par(mfrow = c(2,3))

for(variable in variables_cuantitativas){
  
  boxplot(
    CASAS_analisis[[variable]],
    main = variable,
    col = "lightblue"
  )
}

# ------------------------------------------------------------
# 7. ¿En cuál estrato el precio es mayor?
# ------------------------------------------------------------

precio_estrato <- CASAS_analisis %>%
  group_by(Estrato) %>%
  summarise(
    Precio_Promedio = mean(Precio)
  )

precio_estrato

precio_maximo <- precio_estrato[
  which.max(precio_estrato$Precio_Promedio),
]

precio_maximo

# ------------------------------------------------------------
# 8. ¿Qué variable tiene mayor dispersión?
# ------------------------------------------------------------

tabla_dispersion <- data.frame(
  Variable = variables_cuantitativas,
  Media = colMeans(CASAS_analisis),
  Desv_Estandar = sapply(
    CASAS_analisis,
    sd
  )
)

tabla_dispersion$CV <- (
  tabla_dispersion$Desv_Estandar /
    tabla_dispersion$Media
) * 100

tabla_dispersion

mayor_dispersion <- tabla_dispersion[
  which.max(tabla_dispersion$CV),
]

mayor_dispersion

# ------------------------------------------------------------
# 9. ¿En cuál estrato se presenta mayor sesgo?
# ------------------------------------------------------------

sesgo_estrato <- CASAS_analisis %>%
  group_by(Estrato) %>%
  summarise(
    Sesgo = skewness(Precio)
  )

sesgo_estrato

mayor_sesgo <- sesgo_estrato[
  which.max(abs(sesgo_estrato$Sesgo)),
]

mayor_sesgo

# ------------------------------------------------------------
# 10. ¿En cuál estrato se tiene mayor dispersión?
# ------------------------------------------------------------

dispersion_estrato <- CASAS_analisis %>%
  group_by(Estrato) %>%
  summarise(
    Media = mean(Precio),
    Desv_Estandar = sd(Precio)
  )

dispersion_estrato$CV <- (
  dispersion_estrato$Desv_Estandar /
    dispersion_estrato$Media
) * 100

dispersion_estrato

estrato_mayor_dispersion <- dispersion_estrato[
  which.max(dispersion_estrato$CV),
]

estrato_mayor_dispersion

# ------------------------------------------------------------
# 11. Relación Precio vs Área Construida
# ------------------------------------------------------------

ggplot(
  CASAS_analisis,
  aes(
    x = Area_Construida,
    y = Precio
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Precio vs Área Construida",
    x = "Área Construida",
    y = "Precio"
  )

# ------------------------------------------------------------
# 12. Correlación
# ------------------------------------------------------------

correlacion <- cor(
  CASAS_analisis$Area_Construida,
  CASAS_analisis$Precio
)

correlacion

