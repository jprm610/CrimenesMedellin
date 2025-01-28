data <- read.csv("data/original/informacion_predios.csv", stringsAsFactors = FALSE)

dim(data) #=> 1041413, 15

# Solo tomamos las columnas necesarias 
# para relacionar el barrio con el estrato
data <- data[, c("codComuna", "codBarrio", "Estrato")]

# 1 ---------------------------------------------------------------------
# Construimos el codigo del barrio uniendo 
# el numero de la comuna con el numero del barrio
library(dplyr)
data <- data %>%
  mutate(
    codigo_barrio = paste0(codComuna, sprintf("%02d", as.numeric(codBarrio)))
  )

# 2 -----------------------------------------------------------------------
# Función para calcular la moda
calcular_moda <- function(x) {
  freq <- table(x)
  moda <- as.numeric(names(freq)[which.max(freq)])
  return(moda)
}

# Agrupamos por cada barrio y le asginamos el estrato 
# como la moda de los estratos de los predios
estrato_por_barrio <- data %>%
  group_by(codigo_barrio) %>%
  summarise(
    estrato = calcular_moda(Estrato),
    total_predios = n()
  )

estrato_por_barrio <- estrato_por_barrio %>%
  arrange(codigo_barrio)

head(estrato_por_barrio)

# EXPORTAMOS
write.csv(estrato_por_barrio, file = "data/estratos.csv", row.names = FALSE)
