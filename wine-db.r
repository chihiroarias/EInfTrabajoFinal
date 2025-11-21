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

set.seed(2145)     # Fijamos semilla para reproducibilidad

theme_set(theme_minimal(base_size = 12))
#----------------------------------------------------------------------
# 1 - Cargar los datos
#----------------------------------------------------------------------

red_wine <- read.csv(file = "/Users/milagroscancela/estad/wine+quality/winequality-red.csv", header = TRUE,  sep = ";", row.names = NULL,  stringsAsFactors = FALSE)
white_wine <- read.csv(file = "/Users/milagroscancela/estad/wine+quality/winequality-white.csv", header = TRUE,  sep = ";", row.names = NULL,  stringsAsFactors = FALSE)

# Convertir todas las columnas a numéricas (por si acaso)
red_wine [] <- lapply(red_wine, function(x) as.numeric(as.character(x)))
white_wine[] <- lapply(white_wine, function(x) as.numeric(as.character(x)))

#----------------------------------------------------------------------
# Caso 1 - Wine Quality red
#----------------------------------------------------------------------

n_red <- nrow(red_wine)

cat('Tamaño muestral (n):', n_red, '\n')

# Calcular todos los estadisticos descriptivos

describe_red <- red_wine%>%
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

view(describe_red) # para ver la tabla completa

# 1.1 - Distribuciones (histogramas)

vars_numericas_red <- red_wine %>% select(where(is.numeric)) %>% names()

plot_list_red <- list()
for(var in vars_numericas_red){
  p <- ggplot(red_wine, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, 
                   fill = "#8B0000",      # Rojo oscuro para vino tinto
                   alpha = 0.7,
                   color = "black") +
    geom_density(color = "darkred", linewidth = 1) +
    labs(title = paste("Distribución de", var),
         subtitle = "Vino Tinto",
         x = var, 
         y = "Densidad") +
    theme_minimal(base_size = 9)
  
  plot_list_red[[var]] <- p
}

# Guardar las 12 graficas en un solo plot (4 columnas x 3 filas)
g_red <- do.call(grid.arrange, c(plot_list_red, ncol = 4))
ggsave("figura_histogramas_red.png", 
       g_red, width = 12, height = 8, dpi = 300)

# 1.2 - Evalucion de normalidad (QQ plots)

plot_qq_red <- list()

for(var in vars_numericas_red){
  p <- ggplot(red_wine, aes(sample = .data[[var]])) +
    stat_qq(color = "#8B0000", alpha = 0.6) +
    stat_qq_line(color = "black", linewidth = 1) +
    labs(title = paste("QQ Plot:", var),
         subtitle = "Vino Tinto",
         x = "Cuantiles teóricos (normal estándar)",
         y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 9)
  
  plot_qq_red[[var]] <- p
}

g_qq_red <- do.call(grid.arrange, c(plot_qq_red, ncol = 4))
ggsave("figura_qqplots_red.png", 
       g_qq_red, width = 12, height = 8, dpi = 300)


# 1.3 - Identificacion de valores atipicos (boxplots)

plot_box_red <- list()

for(var in vars_numericas_red){
  p <- ggplot(red_wine, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#8B0000", 
                 alpha = 0.7,
                 outlier.color = "darkred",
                 outlier.size = 2) +
    labs(title = paste("Boxplot:", var),
         subtitle = "Vino Tinto (outliers = puntos rojos)",
         y = var) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  plot_box_red[[var]] <- p
}

g_box_red <- do.call(grid.arrange, c(plot_box_red, ncol = 4))
ggsave("figura_boxplots_red.png", 
       g_box_red, width = 12, height = 8, dpi = 300)

# 1.4 - Analisis de quality - Solo tabla de frecuencias porque anteriormente se analizo la variable quality

# Tabla de frecuencias 

tabla_quality_red <- red_wine %>%
  count(quality) %>%
  mutate(
    proporcion = n / sum(n),
    porcentaje = proporcion * 100,
    acumulado = cumsum(proporcion)
  )

view(tabla_quality_red)

# 1.5 - Analisis de correlaciones 

cor_matrix_red <- red_wine %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

view(cor_matrix_red)

# Visualizar con ggplot 

cor_df_red <- cor_matrix_red %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlacion")

p_cor_red <- ggplot(cor_df_red, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#8B0000", 
                       mid = "white", 
                       high = "#4444BB",
                       midpoint = 0, 
                       limits = c(-1, 1),
                       name = "Correlación\nde Pearson") +
  geom_text(aes(label = round(correlacion, 2)), size = 2.5) +
  labs(title = "Matriz de Correlaciones - Vino Tinto (12 variables)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave("figura_correlacion_red.png", 
       p_cor_red, width = 10, height = 9, dpi = 300)

# Correlaciones con quality (ordenadas por magnitud)
cor_con_quality_red <- cor_matrix_red[,"quality"] %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(correlacion = 2) %>%
  filter(variable != "quality") %>%
  arrange(desc(abs(correlacion)))

view(cor_con_quality_red)


#----------------------------------------------------------------------
# Caso 2 - Wine Quality white
#----------------------------------------------------------------------

n_white <- nrow(white_wine)

cat('Tamaño muestral (n):', n_white, '\n')

# Calcular todos los estadisticos descriptivos

describe_white <- white_wine%>%
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

view(describe_white) # para ver la tabla completa

# 2.1 - Distribuciones (histogramas)

vars_numericas_white <- white_wine %>% select(where(is.numeric)) %>% names()

plot_list_white <- list()
for(var in vars_numericas_white){
  p <- ggplot(white_wine, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, 
                   fill = "#F0E68C",      # Amarillo claro para vino blanco
                   alpha = 0.7,
                   color = "black") +
    geom_density(color = "darkgoldenrod", linewidth = 1) +
    labs(title = paste("Distribución de", var),
         subtitle = "Vino Blanco",
         x = var, 
         y = "Densidad") +
    theme_minimal(base_size = 9)
  
  plot_list_white[[var]] <- p
}

# Guardar las 12 graficas en un solo plot (4 columnas x 3 filas)
g_white <- do.call(grid.arrange, c(plot_list_white, ncol = 4))
ggsave("figura_histogramas_white.png", 
       g_white, width = 12, height = 8, dpi = 300)

# 2.2 - Evalucion de normalidad (QQ plots)

plot_qq_white <- list()

for(var in vars_numericas_white){
  p <- ggplot(white_wine, aes(sample = .data[[var]])) +
    stat_qq(color = "darkgoldenrod", alpha = 0.6) +
    stat_qq_line(color = "black", linewidth = 1) +
    labs(title = paste("QQ Plot:", var),
         subtitle = "Vino Blanco",
         x = "Cuantiles teóricos (normal estándar)",
         y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 9)
  
  plot_qq_white[[var]] <- p
}

g_qq_white <- do.call(grid.arrange, c(plot_qq_white, ncol = 4))
ggsave("figura_qqplots_white.png", 
       g_qq_white, width = 12, height = 8, dpi = 300)


# 2.3 - Identificacion de valores atipicos (boxplots)

plot_box_white <- list()

for(var in vars_numericas_white){
  p <- ggplot(white_wine, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#F0E68C", 
                 alpha = 0.7,
                 outlier.color = "darkgoldenrod",
                 outlier.size = 2) +
    labs(title = paste("Boxplot:", var),
         subtitle = "Vino Blanco (outliers = puntos dorados)",
         y = var) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  plot_box_white[[var]] <- p
}

g_box_white <- do.call(grid.arrange, c(plot_box_white, ncol = 4))
ggsave("figura_boxplots_white.png", 
       g_box_white, width = 12, height = 8, dpi = 300)

# 2.4 - Analisis de quality - Solo tabla de frecuencias porque anteriormente se analizo la variable quality

# Tabla de frecuencias 

tabla_quality_white <- white_wine %>%
  count(quality) %>%
  mutate(
    proporcion = n / sum(n),
    porcentaje = proporcion * 100,
    acumulado = cumsum(proporcion)
  )

view(tabla_quality_white)

# 2.5 - Analisis de correlaciones 

cor_matrix_white <- white_wine %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

view(cor_matrix_white)

# Visualizar con ggplot 

cor_df_white <- cor_matrix_white %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlacion")

p_cor_white <- ggplot(cor_df_white, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#8B0000", 
                       mid = "white", 
                       high = "#4444BB",
                       midpoint = 0, 
                       limits = c(-1, 1),
                       name = "Correlación\nde Pearson") +
  geom_text(aes(label = round(correlacion, 2)), size = 2.5) +
  labs(title = "Matriz de Correlaciones - Vino Blanco (12 variables)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

ggsave("figura_correlacion_white.png", 
       p_cor_white, width = 10, height = 9, dpi = 300)

# Correlaciones con quality (ordenadas por magnitud)
cor_con_quality_white <- cor_matrix_white[,"quality"] %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(correlacion = 2) %>%
  filter(variable != "quality") %>%
  arrange(desc(abs(correlacion)))

view(cor_con_quality_white)


