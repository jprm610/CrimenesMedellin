data <- read.csv("data/original/homicidios.csv", na.strings = "Sin dato", stringsAsFactors = FALSE)

dim(data) #=> 19647, 36

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
data <- data[, !(names(data) %in% columnas_valor_unico)]

str(data)
