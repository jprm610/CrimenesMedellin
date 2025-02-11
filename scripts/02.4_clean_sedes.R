data <- read.csv("data/original/sedes_educativas.csv")
str(data)

library(dplyr)
library(tidyr)

data <- data %>% select("longitud", "latitud", "codigo_comuna", "sector", "estado")
colnames(data) <- toupper(colnames(data))
str(data)

data <- data %>% filter(ESTADO == "Activo")
str(data)

data <- data %>% drop_na(LONGITUD, LATITUD)
str(data)

data <- data %>% select(-ESTADO)
str(data)

write.csv(data, file = "data/colegios.csv", row.names = FALSE)
