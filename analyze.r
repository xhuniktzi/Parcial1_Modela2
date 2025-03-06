# Librerias
library(dplyr)
library(tidyr)

# Datos
datos <- read.csv("C:\\Users\\xhuni\\Projects\\Parcial1_Modela2\\Datos_PP_012025.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE)

# Limpieza de datos
datos_limpios <- datos %>%
  pivot_longer(cols = everything(), names_to = "Columna", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  mutate(
    Cadena = ifelse(grepl("CADENA", Valor), Valor, NA),
    Disponibilidad = ifelse(!grepl("CADENA", Valor), Valor, NA)
  ) %>%
  fill(Cadena, .direction = "down") %>%
  filter(!is.na(Disponibilidad)) %>%
  select(Cadena, Disponibilidad)

datos_limpios$Cadena <- gsub("CADENA ", "", datos_limpios$Cadena)
datos_limpios$Disponibilidad <- as.numeric(gsub("%", "", datos_limpios$Disponibilidad)) / 100

# Separar datos por cadena
cadena1 <- datos_limpios %>% filter(Cadena == "1")
cadena2 <- datos_limpios %>% filter(Cadena == "2")

# Estadisticas
media_c1 <- mean(cadena1$Disponibilidad, na.rm = TRUE)
sd_c1 <- sd(cadena1$Disponibilidad, na.rm = TRUE)

media_c2 <- mean(cadena2$Disponibilidad, na.rm = TRUE)
sd_c2 <- sd(cadena2$Disponibilidad, na.rm = TRUE)

# Capacidad de proceso
lsl <- 0.95
t <- 0.98
usl <- 1

# Calculo de sigma para cadena 1
sigma_high_c1 <- (usl - media_c1)/sd_c1
norm_sigma_high_c1 <- pnorm(sigma_high_c1)

sigma_low_c1 <- (lsl - media_c1)/sd_c1
norm_sigma_low_c1 <- pnorm(sigma_low_c1)

result_c1 <- norm_sigma_high_c1 - norm_sigma_low_c1

# Calculo de sigma para cadena 2
sigma_high_c2 <- (usl - media_c2)/sd_c2
norm_sigma_high_c2 <- pnorm(sigma_high_c2)

sigma_low_c2 <- (lsl - media_c2)/sd_c2
norm_sigma_low_c2 <- pnorm(sigma_low_c2)

result_c2 <- norm_sigma_high_c2 - norm_sigma_low_c2

# Impresion de resultados
cat("Cadena 1\n")
cat("Media: ", media_c1, "\n")
cat("Desviacion estandar: ", sd_c1, "\n")
cat("Sigma: ", result_c1, "\n")

cat("\nCadena 2\n")
cat("Media: ", media_c2, "\n")
cat("Desviacion estandar: ", sd_c2, "\n")
cat("Sigma: ", result_c2, "\n")