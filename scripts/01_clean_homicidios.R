data <- read.csv("data/original/homicidios.csv", na.strings = c("Sin dato", "NaN", "nan"), stringsAsFactors = FALSE)

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
cat("Se eliminaron las columnas:", paste(names(columnas_valor_unico), collapse = ", "), "\n")

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
cat("Se eliminaron las columnas:", paste(columnas_a_eliminar, collapse = ", "), "\n")

# 3 ---------------------------------------------------------------------
# Eliminar los registros donde LATITUD y LONGITUD son NA
n <- nrow(data[(is.na(data$LATITUD) & is.na(data$LONGITUD)), ])
data <- data[!(is.na(data$LATITUD) & is.na(data$LONGITUD)), ]
cat("Se eliminaron ", n, " registros donde LATITUD y LONGITUD son faltantes.\n")

str(data)
