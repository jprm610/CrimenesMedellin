homicidios <- read.csv("data/homicidios.csv", stringsAsFactors = FALSE)
predios <- read.csv("data/predios.csv", stringsAsFactors = FALSE)

# 1 ----------------------------------------------------------------------------
# Calcular distancias de un incidente a todos los predios
library(RANN)

# Coordenadas de predios (como matriz)
predios_matrix <- as.matrix(predios[, c("LONGITUD", "LATITUD")])

# Coordenadas de incidentes (como matriz)
homicidios_matrix <- as.matrix(homicidios[, c("LONGITUD", "LATITUD")])

# Encontrar los k vecinos más cercanos (e.g., k = 5)
k <- 5
radio = 0.002
resultado_knn <- nn2(predios_matrix, homicidios_matrix, k = k, searchtype = "radius", radius = 0.005)

# Extraer índices y distancias
indices <- resultado_knn$nn.idx  # Índices de los predios más cercanos
distancias <- resultado_knn$nn.dists  # Distancias a esos predios

# 2 ----------------------------------------------------------------------------
# Asociar ESTRATO Y DESTINACION_PREDIOS a cada incidente

# Crear una lista para guardar resultados por homicidio
valores_k <- lapply(1:nrow(homicidios), function(i) {
  # Índices de los predios más cercanos al homicidio i
  vecinos <- indices[i, ]
  
  # Extraer las variables ESTRATO y DESTINACIÓN de esos predios
  estratos <- predios$ESTRATO[vecinos]
  destinaciones <- predios$DESTINACION[vecinos]
  
  list(estratos = estratos, destinaciones = destinaciones)
})

# Función para calcular la moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular la moda para cada homicidio
modas <- lapply(valores_k, function(v) {
  estrato_moda <- moda(v$estratos)
  destinacion_moda <- moda(v$destinaciones)
  
  list(estrato = estrato_moda, destinacion = destinacion_moda)
})

# Agregar las columnas de moda al data frame homicidios
homicidios["ESTRATO"] <- sapply(modas, `[[`, "estrato")
homicidios["DESTINACION_PREDIOS"] <- sapply(modas, `[[`, "destinacion")

head(homicidios)

write.csv(homicidios, "data/homicidios_predios.csv", row.names = FALSE)
