# ==============================================================================
# BOXPLOTS - DATASET COMBINADO (VINOS ROJOS Y BLANCOS JUNTOS)
# ==============================================================================

library(tidyverse)
library(gridExtra)

# Cargar datasets
whitewine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-white.csv", sep = ";")
redwine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-red.csv", sep = ";")

# Combinar datasets
wine_data <- bind_rows(
  whitewine %>% mutate(type = "white"),
  redwine %>% mutate(type = "red")
)

# Variables numéricas (sin incluir type, pero lo usaremos para comparar)
numeric_cols <- wine_data %>% select(-type) %>% names()

cat("Generando boxplots comparando Red vs White wine...\n")
cat("Total observaciones:", nrow(wine_data), "\n")
cat("Variables:", paste(numeric_cols, collapse = ", "), "\n\n")

# Crear boxplots comparando tipo de vino
plot_box <- list()

for(var in numeric_cols) {
  p <- ggplot(wine_data, aes(x = type, y = .data[[var]], fill = type)) +
    geom_boxplot(alpha = 0.7,
                 outlier.color = "darkblue",
                 outlier.size = 1.5) +
    scale_fill_manual(values = c("red" = "#C0504D", "white" = "#9BBB59")) +
    labs(title = paste("Boxplot:", var),
         x = "",
         y = var) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 10))
  
  plot_box[[var]] <- p
}

# Guardar como grid
g_box <- do.call(grid.arrange, c(plot_box, ncol = 4))
ggsave("wine_boxplots_red_vs_white.png", g_box, width = 14, height = 10, dpi = 300)

cat("\n✓ Boxplots Red vs White generados exitosamente!\n")
cat("Archivo guardado: wine_boxplots_red_vs_white.png\n")
