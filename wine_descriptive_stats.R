# ==============================================================================
# WINE QUALITY - ESTADÍSTICOS DESCRIPTIVOS
# Dataset: Wine Quality (Vinos Rojos y Blancos)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN Y PAQUETES
# ------------------------------------------------------------------------------

required_packages <- c("tidyverse", "moments")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

set.seed(2145)

# ------------------------------------------------------------------------------
# 2. CARGA DE DATOS
# ------------------------------------------------------------------------------

whitewine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-white.csv", sep = ";")
redwine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-red.csv", sep = ";")

wine_data <- bind_rows(
  whitewine %>% mutate(type = 1),  # white = 1
  redwine %>% mutate(type = 0)     # red = 0
)

# ------------------------------------------------------------------------------
# 3. ESTADÍSTICOS DESCRIPTIVOS GENERALES
# ------------------------------------------------------------------------------

cat("\n=== ESTADÍSTICOS DESCRIPTIVOS - WINE QUALITY DATASET ===\n\n")

# Variables numéricas (excluyendo quality, type ahora es numérica)
numeric_vars <- wine_data %>% 
  select(-quality) %>% 
  names()

descriptive_stats <- wine_data %>%
  select(-quality) %>%
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

cat("Estadísticos Descriptivos (Variables Numéricas):\n")
cat("Nota: type codificada como 0=red, 1=white\n\n")
print(descriptive_stats, n = Inf)

# ------------------------------------------------------------------------------
# 4. BOXPLOTS PARA DETECCIÓN DE OUTLIERS
# ------------------------------------------------------------------------------

cat("\n=== GENERANDO BOXPLOTS PARA OUTLIERS ===\n\n")

library(gridExtra)

# Crear gráficos individuales para cada variable
plot_list_box <- list()

for(var in numeric_vars) {
  p <- ggplot(wine_data, aes(x = "", y = .data[[var]])) +
    geom_boxplot(fill = "#6B4E71", 
                 alpha = 0.7,
                 outlier.color = "red",
                 outlier.size = 1.2,
                 outlier.alpha = 0.6) +
    labs(title = paste("Boxplot:", var),
         x = NULL,
         y = var) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  plot_list_box[[var]] <- p
}

# Combinar todos en un solo gráfico
g_box <- do.call(grid.arrange, c(plot_list_box, ncol = 4))
ggsave("descriptive_boxplots_outliers.png", g_box, width = 16, height = 11, dpi = 300)

cat("✓ Boxplots guardados en: descriptive_boxplots_outliers.png\n")
cat("   Ubicación completa:", file.path(getwd(), "descriptive_boxplots_outliers.png"), "\n")

# ------------------------------------------------------------------------------
# 5. EXPORTACIÓN DE RESULTADOS
# ------------------------------------------------------------------------------

cat("\n=== EXPORTANDO RESULTADOS ===\n\n")

write.csv(descriptive_stats, "descriptive_stats.csv", row.names = FALSE)
cat("✓ Exportado: descriptive_stats.csv\n")

# ------------------------------------------------------------------------------
# 6. RESUMEN EJECUTIVO
# ------------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("RESUMEN EJECUTIVO - ESTADÍSTICOS DESCRIPTIVOS\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("DATASET:\n")
cat("  Total observaciones:", nrow(wine_data), "\n")
cat("  Variables analizadas:", length(numeric_vars), "\n")
cat("  (type codificada: 0=red, 1=white)\n\n")

cat("VARIABLES CON MAYOR ASIMETRÍA (|skewness| > 1.5):\n")
highly_skewed <- descriptive_stats %>%
  filter(abs(asimetria) > 1.5) %>%
  arrange(desc(abs(asimetria))) %>%
  select(variable, asimetria)

if(nrow(highly_skewed) > 0) {
  for(i in 1:nrow(highly_skewed)) {
    cat(sprintf("  %s: %.3f\n", 
                highly_skewed$variable[i], 
                highly_skewed$asimetria[i]))
  }
} else {
  cat("  Ninguna variable presenta asimetría extrema\n")
}

cat("\nVARIABLES CON MAYOR DISPERSIÓN (CV > 0.5):\n")
high_variation <- descriptive_stats %>%
  filter(cv > 0.5) %>%
  arrange(desc(cv)) %>%
  select(variable, cv)

if(nrow(high_variation) > 0) {
  for(i in 1:nrow(high_variation)) {
    cat(sprintf("  %s: %.3f\n", 
                high_variation$variable[i], 
                high_variation$cv[i]))
  }
} else {
  cat("  Ninguna variable presenta dispersión extrema\n")
}

cat("\nARCHIVOS GENERADOS:\n")
cat("  1. descriptive_stats.csv\n")
cat("  2. descriptive_boxplots_outliers.png\n")

cat("\n", rep("=", 70), "\n", sep = "")
cat("ANÁLISIS COMPLETADO\n")
cat(rep("=", 70), "\n\n", sep = "")
