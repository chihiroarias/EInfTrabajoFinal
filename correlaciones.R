library(tidyverse)

whitewine <- read.csv("winequality-white.csv", sep = ";")
redwine <- read.csv("winequality-red.csv", sep = ";")

# Unimos ambos datasets, creamos una variable 'type' para distinguir los vinos
wine_data <- bind_rows(
  whitewine %>% mutate(type = "white"),
  redwine %>% mutate(type = "red")
)

# Verificamos la estructura del dataset combinado
head(wine_data)

# Calculamos la matriz de correlación para las variables numéricas
correlation_matrix <- wine_data %>%
  select(-type) %>%  # Excluimos la variable categórica 'type'
  cor(use = "complete.obs")  # Usamos solo observaciones completas
print(correlation_matrix)

library(corrplot)

# Visualizamos la matriz de correlación
# Guardar a PNG de alta resolución y ajustar tamaños de texto
png("correlation_matrix_highres.png", width = 2000, height = 2000, res = 300, type = "cairo")
corrplot(correlation_matrix, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45,
         tl.cex = 1.2,    # tamaño de etiquetas de variables
         cl.cex = 1,      # tamaño de la leyenda de color
         mar = c(0,3,0,0))
title(main = "Matriz de Correlación de Variables del Vino", cex.main = 1.5)
dev.off()