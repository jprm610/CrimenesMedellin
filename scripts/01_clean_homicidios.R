data <- read.csv("data/original/homicidios.csv", na.strings = "Sin dato", stringsAsFactors = FALSE)

dim(data) #=> 19647, 36

# 1 ------------------------------------------------------------------------
# Hallamos las columnas que tengan un solo valor 
# y las guardamos junto al valor
colnames(data) <- toupper(colnames(data))
columnas_valor_unico <- list()
for (col in names(data)) {
  valores_unicos <- unique(data[[col]])
  
  if (length(valores_unicos) == 1) {
    columnas_valor_unico[[col]] <- valores_unicos
  }
}
# Eliminamos dichas columnas
data <- data[, !(names(data) %in% names(columnas_valor_unico))]

# 2 ---------------------------------------------------------------------
# Eliminar columnas cuya porporción de datos faltantes sea >= 40%
nas_resumen <- list()
N <- nrow(data)
for (col in names(data)) {
  cantidad_na <- sum(is.na(data[[col]]))
  proporcion_na <- round((cantidad_na / N) * 100, 2)
  nas_resumen[[col]] <- list(
    cantidad = cantidad_na,
    proporcion = proporcion_na
  )
}
columnas_a_eliminar <- names(nas_resumen)[sapply(nas_resumen, function(x) x[["proporcion"]] >= 40)]
data <- data[, !(names(data) %in% columnas_a_eliminar), drop = FALSE]

str(data)
