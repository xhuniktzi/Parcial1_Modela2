# Cargar librerías
library(dplyr)
library(tidyr)

# Leer datos
datos_originales <- read.csv("C:\\Users\\xhuni\\Projects\\Parcial1_Modela2\\Datos_PP_012025.csv", 
                             sep = ";", 
                             stringsAsFactors = FALSE, 
                             header = TRUE)

# Transformar y limpiar datos
datos_procesados <- datos_originales %>%
  pivot_longer(cols = everything(), names_to = "nombre_columna", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(
    cadena = ifelse(grepl("CADENA", valor), valor, NA),
    disponibilidad = ifelse(!grepl("CADENA", valor), valor, NA)
  ) %>%
  fill(cadena, .direction = "down") %>%
  filter(!is.na(disponibilidad)) %>%
  select(cadena, disponibilidad)

# Limpiar valores de las columnas 'cadena' y 'disponibilidad'
datos_procesados$cadena <- gsub("CADENA ", "", datos_procesados$cadena)
datos_procesados$disponibilidad <- as.numeric(gsub("%", "", datos_procesados$disponibilidad)) / 100

# Separar datos por cadena
datos_cadena1 <- datos_procesados %>% filter(cadena == "1")
datos_cadena2 <- datos_procesados %>% filter(cadena == "2")

# Calcular estadísticas descriptivas para cada cadena
media_cadena1 <- mean(datos_cadena1$disponibilidad, na.rm = TRUE)
sd_cadena1 <- sd(datos_cadena1$disponibilidad, na.rm = TRUE)

media_cadena2 <- mean(datos_cadena2$disponibilidad, na.rm = TRUE)
sd_cadena2 <- sd(datos_cadena2$disponibilidad, na.rm = TRUE)

# Definir límites para la capacidad de proceso
limite_inferior <- 0.95
limite_objetivo <- 0.98  # Variable definida pero no utilizada en el cálculo
limite_superior <- 1

# Calcular capacidad de proceso (índice sigma) para Cadena 1
z_superior_cadena1 <- (limite_superior - media_cadena1) / sd_cadena1
probabilidad_superior_cadena1 <- pnorm(z_superior_cadena1)

z_inferior_cadena1 <- (limite_inferior - media_cadena1) / sd_cadena1
probabilidad_inferior_cadena1 <- pnorm(z_inferior_cadena1)

indice_capacidad_cadena1 <- probabilidad_superior_cadena1 - probabilidad_inferior_cadena1

# Calcular capacidad de proceso (índice sigma) para Cadena 2
z_superior_cadena2 <- (limite_superior - media_cadena2) / sd_cadena2
probabilidad_superior_cadena2 <- pnorm(z_superior_cadena2)

z_inferior_cadena2 <- (limite_inferior - media_cadena2) / sd_cadena2
probabilidad_inferior_cadena2 <- pnorm(z_inferior_cadena2)

indice_capacidad_cadena2 <- probabilidad_superior_cadena2 - probabilidad_inferior_cadena2

# Imprimir resultados
cat("Cadena 1\n")
cat("Media: ", media_cadena1, "\n")
cat("Desviación estándar: ", sd_cadena1, "\n")
cat("Índice de capacidad: ", indice_capacidad_cadena1, "\n\n")

cat("Cadena 2\n")
cat("Media: ", media_cadena2, "\n")
cat("Desviación estándar: ", sd_cadena2, "\n")
cat("Índice de capacidad: ", indice_capacidad_cadena2, "\n")
