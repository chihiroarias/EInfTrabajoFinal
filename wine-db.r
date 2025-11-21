#----------------------------------------------------------------------
# Wine Quality 
# Estructura del dataset:
# Quality - Calificación sensorial de calidad (0-10)
# Type - Tipo de vino (rojo o blanco)
# Fixed Acidity - Acidez fija (g/L)
# Volatile Acidity - Acidez volátil (g/L)
# Citric Acid - Ácido cítrico (g/L)
# Residual Sugar - Azúcar residual (g/L)
# Chlorides - Cloruros [salinidad del vino] (g/L)
# Free Sulfur Dioxide - Dióxido de azufre libre (mg/L)
# Total Sulfur Dioxide - Dióxido de azufre total [contiene sulfatos] (mg/L)
# Density - Densidad (g/cm^3)
# pH - pH del vino
# Sulphates - Sulfatos [contiene sulfatos] (g/L)
# Alcohol - Alcohol (%)
#----------------------------------------------------------------------
library(tidyverse)  
library(moments)    
library(knitr)     
library(gridExtra)  
library(scales)
library(corrplot)  

set.seed(2145)     # Fijamos semilla para reproducibilidad

theme_set(theme_minimal(base_size = 12))
#----------------------------------------------------------------------
# 1 - Cargar los datos
#----------------------------------------------------------------------

whitewine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-white.csv", sep = ";")
redwine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-red.csv", sep = ";")

# Unimos ambos datasets, creamos una variable 'type' para distinguir los vinos
wine_data <- bind_rows(
  whitewine %>% mutate(type = "white"),
  redwine %>% mutate(type = "red")
)

# Verificamos la estructura del dataset combinado
head(wine_data)

#----------------------------------------------------------------------
# Caso 1 - Wine Quality (wine_data completo)
#----------------------------------------------------------------------

n_wine <- nrow(wine_data)

cat('Tamaño muestral (n):', n_wine, '\n')

# Calcular todos los estadisticos descriptivos

describe_wine <- wine_data %>%
  select(-type) %>%  # Excluimos la variable categórica 'type'
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desvio = sd(valor, na.rm = TRUE),
    cv = desvio / media,
    minimo = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    rango = maximo - minimo,
    IQR = IQR(valor, na.rm = TRUE),
    asimetria = skewness(valor, na.rm = TRUE),
    curtosis = kurtosis(valor, na.rm = TRUE),
    .groups = "drop"
  )

view(describe_wine) # para ver la tabla completa

# 1.1 - Distribuciones (histogramas)

vars_numericas_wine <- wine_data %>% select(-type, where(is.numeric)) %>% names()

plot_list_wine <- list()
for(var in vars_numericas_wine){
  p <- ggplot(wine_data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, 
                   fill = "#6B4E71",      # Color morado para dataset combinado
                   alpha = 0.7,
                   color = "black") +
    geom_density(color = "darkblue", linewidth = 1) +
    labs(title = paste("Distribución de", var),
         subtitle = "Wine Data Completo",
         x = var, 
         y = "Densidad") +
    theme_minimal(base_size = 9)
  
  plot_list_wine[[var]] <- p
}

# Guardar las 12 graficas en un solo plot (4 columnas x 3 filas)
g_wine <- do.call(grid.arrange, c(plot_list_wine, ncol = 4))
ggsave("figura_histogramas_wine.png", 
       g_wine, width = 12, height = 8, dpi = 300)

# 1.2 - Evalucion de normalidad (QQ plots)

plot_qq_wine <- list()

for(var in vars_numericas_wine){
  p <- ggplot(wine_data, aes(sample = .data[[var]])) +
    stat_qq(color = "#6B4E71", alpha = 0.6) +
    stat_qq_line(color = "black", linewidth = 1) +
    labs(title = paste("QQ Plot:", var),
         subtitle = "Wine Data Completo",
         x = "Cuantiles teóricos (normal estándar)",
         y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 9)
  
  plot_qq_wine[[var]] <- p
}

g_qq_wine <- do.call(grid.arrange, c(plot_qq_wine, ncol = 4))
ggsave("figura_qqplots_wine.png", 
       g_qq_wine, width = 12, height = 8, dpi = 300)


# 1.3 - Identificacion de valores atipicos (boxplots)

plot_box_wine <- list()

for(var in vars_numericas_wine){
  p <- ggplot(wine_data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#6B4E71", 
                 alpha = 0.7,
                 outlier.color = "darkblue",
                 outlier.size = 2) +
    labs(title = paste("Boxplot:", var),
         subtitle = "Wine Data Completo (outliers = puntos azules)",
         y = var) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  plot_box_wine[[var]] <- p
}

g_box_wine <- do.call(grid.arrange, c(plot_box_wine, ncol = 4))
ggsave("figura_boxplots_wine.png", 
       g_box_wine, width = 12, height = 8, dpi = 300)

# 1.4 - Analisis de quality - Solo tabla de frecuencias porque anteriormente se analizo la variable quality

# Tabla de frecuencias 

tabla_quality_wine <- wine_data %>%
  count(quality) %>%
  mutate(
    proporcion = n / sum(n),
    porcentaje = proporcion * 100,
    acumulado = cumsum(proporcion)
  )

view(tabla_quality_wine)

# 1.5 - Analisis de correlaciones 

# Calculamos la matriz de correlación para las variables numéricas
correlation_matrix <- wine_data %>%
  select(-type) %>%  # Excluimos la variable categórica 'type'
  cor(use = "complete.obs")  # Usamos solo observaciones completas
print(correlation_matrix)

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





