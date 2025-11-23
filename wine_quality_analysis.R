# ==============================================================================
# WINE QUALITY - ANÁLISIS DESCRIPTIVO Y EXPLORATORIO COMPLETO
# Dataset: Wine Quality (Vinos Rojos y Blancos)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN INICIAL Y PAQUETES
# ------------------------------------------------------------------------------

# Paquetes necesarios
required_packages <- c("tidyverse", "moments", "knitr", "gridExtra", 
                       "scales", "corrplot", "psych", "car")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Configuración general
set.seed(2145)
theme_set(theme_minimal(base_size = 12))
rm(list = ls())

# ------------------------------------------------------------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------------------------

# Cargar datasets individuales
whitewine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-white.csv", sep = ";")
redwine <- read.csv("C:/Users/arias/Desktop/EInfTrabajoFinal/winequality-red.csv", sep = ";")

# Combinar datasets con identificación de tipo
wine_data <- bind_rows(
  whitewine %>% mutate(type = "white"),
  redwine %>% mutate(type = "red")
)

# Estructura básica del dataset
cat("\n=== CARACTERÍSTICAS GENERALES DEL DATASET ===\n")
cat("Dimensiones del dataset:\n")
cat("Observaciones:", nrow(wine_data), "\n")
cat("Variables:", ncol(wine_data), "\n")
cat("\nDistribución por tipo de vino:\n")
print(table(wine_data$type))

cat("\nPrimeras observaciones:\n")
print(head(wine_data))

cat("\nEstructura de variables:\n")
str(wine_data)

# ------------------------------------------------------------------------------
# 3. CALIDAD DE DATOS Y VALIDACIÓN
# ------------------------------------------------------------------------------

cat("\n=== CALIDAD DE DATOS ===\n")

# Valores faltantes
missing_values <- colSums(is.na(wine_data))
cat("\nValores faltantes por variable:\n")
print(missing_values)
cat("Total de valores faltantes:", sum(missing_values), "\n")
cat("Porcentaje de casos completos:", 
    round(mean(complete.cases(wine_data)) * 100, 2), "%\n")

# Duplicados
duplicates <- sum(duplicated(wine_data))
cat("\nFilas duplicadas:", duplicates, "\n")
if(duplicates > 0) {
  cat("Porcentaje de duplicados:", round(duplicates/nrow(wine_data)*100, 2), "%\n")
}

# Validación de rangos
cat("\n--- Validación de Rangos ---\n")
numeric_cols <- wine_data %>% select(-type) %>% names()
for(col in numeric_cols) {
  cat(sprintf("%s: Min = %.3f, Max = %.3f\n", 
              col, min(wine_data[[col]], na.rm = TRUE), 
              max(wine_data[[col]], na.rm = TRUE)))
}

# ------------------------------------------------------------------------------
# 4. ESTADÍSTICOS DESCRIPTIVOS
# ------------------------------------------------------------------------------

cat("\n=== ESTADÍSTICOS DESCRIPTIVOS ===\n")

descriptive_stats <- wine_data %>%
  select(-type) %>%
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

print(descriptive_stats)

# ------------------------------------------------------------------------------
# 5. IDENTIFICACIÓN DE OUTLIERS
# ------------------------------------------------------------------------------

cat("\n=== DETECCIÓN DE OUTLIERS (Método IQR) ===\n")

outlier_summary <- data.frame(
  Variable = character(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  N_Outliers = integer(),
  Pct_Outliers = numeric(),
  stringsAsFactors = FALSE
)

for(col in numeric_cols) {
  Q1 <- quantile(wine_data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(wine_data[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- wine_data[[col]] < lower_bound | wine_data[[col]] > upper_bound
  n_outliers <- sum(outliers, na.rm = TRUE)
  pct_outliers <- round(n_outliers / nrow(wine_data) * 100, 2)
  
  outlier_summary <- rbind(outlier_summary, 
                           data.frame(Variable = col,
                                      Lower_Bound = lower_bound,
                                      Upper_Bound = upper_bound,
                                      N_Outliers = n_outliers,
                                      Pct_Outliers = pct_outliers))
}

print(outlier_summary)

# ------------------------------------------------------------------------------
# 6. ANÁLISIS DE NORMALIDAD
# ------------------------------------------------------------------------------

cat("\n=== PRUEBAS DE NORMALIDAD (Shapiro-Wilk) ===\n")

normality_results <- data.frame(
  Variable = character(),
  W_statistic = numeric(),
  p_value = numeric(),
  Normal = character(),
  stringsAsFactors = FALSE
)

for(col in numeric_cols) {
  sample_data <- if(nrow(wine_data) > 5000) {
    sample(wine_data[[col]], 5000)
  } else {
    wine_data[[col]]
  }
  
  shapiro_test <- shapiro.test(sample_data)
  
  normality_results <- rbind(normality_results,
                             data.frame(
                               Variable = col,
                               W_statistic = shapiro_test$statistic,
                               p_value = shapiro_test$p.value,
                               Normal = ifelse(shapiro_test$p.value > 0.05, 
                                               "Sí (p>0.05)", "No (p<0.05)")
                             ))
}

print(normality_results)

# ------------------------------------------------------------------------------
# 7. ANÁLISIS DE QUALITY (VARIABLE OBJETIVO)
# ------------------------------------------------------------------------------

cat("\n=== ANÁLISIS DE QUALITY (Variable Objetivo) ===\n")

# Tabla de frecuencias
quality_freq <- table(wine_data$quality)
quality_prop <- prop.table(quality_freq) * 100

quality_table <- data.frame(
  Quality = as.numeric(names(quality_freq)),
  Frequency = as.numeric(quality_freq),
  Percentage = round(as.numeric(quality_prop), 2)
)

print(quality_table)

cat("\nEstadísticos de Quality:\n")
cat("Rango:", min(wine_data$quality), "-", max(wine_data$quality), "\n")
cat("Media:", round(mean(wine_data$quality), 2), "\n")
cat("Mediana:", median(wine_data$quality), "\n")
cat("Moda:", names(which.max(quality_freq)), 
    "(", max(quality_freq), "observaciones)\n")

# Crear categorías de calidad
wine_data <- wine_data %>%
  mutate(quality_category = cut(quality,
                                breaks = c(0, 5, 6, 10),
                                labels = c("Baja (3-5)", "Media (6)", "Alta (7-9)"),
                                include.lowest = TRUE))

cat("\nDistribución por categorías:\n")
print(table(wine_data$quality_category))
print(prop.table(table(wine_data$quality_category)) * 100)

# ------------------------------------------------------------------------------
# 8. ANÁLISIS DE CORRELACIONES
# ------------------------------------------------------------------------------

cat("\n=== ANÁLISIS DE CORRELACIONES ===\n")

# Matriz de correlación
correlation_matrix <- wine_data %>%
  select(-type, -quality_category) %>%
  cor(use = "complete.obs")

print(round(correlation_matrix, 3))

# Correlaciones con Quality
cor_with_quality <- sort(correlation_matrix[, "quality"], decreasing = TRUE)
cat("\nVariables más correlacionadas con Quality:\n")
print(round(cor_with_quality, 3))

# Pares altamente correlacionados (multicolinealidad potencial)
cat("\n--- Pares de Variables Altamente Correlacionadas (|r| > 0.7) ---\n")
high_cor <- which(abs(correlation_matrix) > 0.7 & abs(correlation_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  high_cor_unique <- high_cor[high_cor[,1] < high_cor[,2], , drop = FALSE]
  for(i in seq_len(nrow(high_cor_unique))) {
    var1 <- rownames(correlation_matrix)[high_cor_unique[i,1]]
    var2 <- colnames(correlation_matrix)[high_cor_unique[i,2]]
    cor_val <- correlation_matrix[high_cor_unique[i,1], high_cor_unique[i,2]]
    cat(sprintf("%s vs %s: r = %.3f\n", var1, var2, cor_val))
  }
}

# ------------------------------------------------------------------------------
# 9. ANÁLISIS ANOVA POR QUALITY
# ------------------------------------------------------------------------------

cat("\n=== ANOVA - Diferencias por Nivel de Quality ===\n")

anova_results <- data.frame(
  Variable = character(),
  F_statistic = numeric(),
  p_value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for(col in numeric_cols) {
  if(col != "quality") {
    formula <- as.formula(paste(col, "~ factor(quality)"))
    anova_test <- aov(formula, data = wine_data)
    anova_summary <- summary(anova_test)
    f_stat <- anova_summary[[1]]$`F value`[1]
    p_val <- anova_summary[[1]]$`Pr(>F)`[1]
    
    anova_results <- rbind(anova_results,
                           data.frame(
                             Variable = col,
                             F_statistic = f_stat,
                             p_value = p_val,
                             Significant = ifelse(p_val < 0.05, "Sí (p<0.05)", "No")
                           ))
  }
}

print(anova_results)

# ------------------------------------------------------------------------------
# 10. ANÁLISIS DE MULTICOLINEALIDAD (VIF)
# ------------------------------------------------------------------------------

cat("\n=== ANÁLISIS DE MULTICOLINEALIDAD (VIF) ===\n")

model_data <- wine_data %>%
  select(-type, -quality_category) %>%
  na.omit()

vif_results <- data.frame(
  Variable = character(),
  VIF = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

for(col in numeric_cols) {
  if(col != "quality") {
    formula <- as.formula(paste(col, "~ ."))
    lm_model <- lm(formula, data = model_data[, names(model_data) != "quality"])
    r_squared <- summary(lm_model)$r.squared
    vif_value <- 1 / (1 - r_squared)
    
    interpretation <- if(vif_value < 5) {
      "Bajo"
    } else if(vif_value < 10) {
      "Moderado"
    } else {
      "Alto (problema potencial)"
    }
    
    vif_results <- rbind(vif_results,
                         data.frame(
                           Variable = col,
                           VIF = vif_value,
                           Interpretation = interpretation
                         ))
  }
}

vif_results <- vif_results[order(vif_results$VIF, decreasing = TRUE), ]
print(vif_results)

# ------------------------------------------------------------------------------
# 11. VISUALIZACIONES - HISTOGRAMAS Y DENSIDAD
# ------------------------------------------------------------------------------

cat("\n=== GENERANDO VISUALIZACIONES ===\n")

# Histogramas con densidad
plot_list_hist <- list()
for(var in numeric_cols) {
  p <- ggplot(wine_data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, 
                   fill = "#6B4E71",
                   alpha = 0.7,
                   color = "black") +
    geom_density(color = "darkblue", linewidth = 1) +
    labs(title = paste("Distribución de", var),
         x = var, 
         y = "Densidad") +
    theme_minimal(base_size = 9)
  
  plot_list_hist[[var]] <- p
}

g_hist <- do.call(grid.arrange, c(plot_list_hist, ncol = 4))
ggsave("wine_histograms.png", g_hist, width = 14, height = 10, dpi = 300)

# ------------------------------------------------------------------------------
# 12. VISUALIZACIONES - QQ PLOTS PARA EVALUACIÓN DE NORMALIDAD
# ------------------------------------------------------------------------------

plot_qq <- list()

for(var in numeric_cols) {
  # Obtener resultado de normalidad para la variable
  norm_result <- normality_results %>% 
    filter(Variable == var) %>% 
    pull(Normal)
  
  p <- ggplot(wine_data, aes(sample = .data[[var]])) +
    stat_qq(color = "#6B4E71", alpha = 0.6, size = 0.8) +
    stat_qq_line(color = "red", linewidth = 1, linetype = "dashed") +
    labs(title = paste("QQ Plot:", var),
         subtitle = paste("Normalidad:", norm_result),
         x = "Cuantiles teóricos (normal estándar)",
         y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 9) +
    theme(plot.subtitle = element_text(size = 7, color = "darkred"))
  
  plot_qq[[var]] <- p
}

g_qq <- do.call(grid.arrange, c(plot_qq, ncol = 4))
ggsave("wine_qqplots.png", g_qq, width = 14, height = 10, dpi = 300)

# QQ Plots individuales para variables clave en el análisis de normalidad
cat("\nGenerando QQ plots individuales para variables clave...\n")

key_vars_normality <- c("alcohol", "pH", "density", "residual.sugar", 
                        "chlorides", "free.sulfur.dioxide")

for(var in key_vars_normality) {
  norm_stat <- normality_results %>% filter(Variable == var)
  
  p_individual <- ggplot(wine_data, aes(sample = .data[[var]])) +
    stat_qq(color = "#6B4E71", alpha = 0.5, size = 1.5) +
    stat_qq_line(color = "red", linewidth = 1.5, linetype = "dashed") +
    labs(title = paste("QQ Plot:", var),
         subtitle = sprintf("Shapiro-Wilk: W = %.4f, p < 0.001\nDistribución: %s",
                            norm_stat$W_statistic, norm_stat$Normal),
         x = "Cuantiles Teóricos (Distribución Normal Estándar)",
         y = "Cuantiles de la Muestra") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "darkred"),
      axis.title = element_text(size = 12)
    )
  
  ggsave(paste0("qqplot_individual_", var, ".png"), 
         p_individual, width = 8, height = 6, dpi = 300)
}

# ------------------------------------------------------------------------------
# 13. VISUALIZACIONES - BOXPLOTS
# ------------------------------------------------------------------------------

plot_box <- list()

for(var in numeric_cols) {
  p <- ggplot(wine_data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#6B4E71", 
                 alpha = 0.7,
                 outlier.color = "darkblue",
                 outlier.size = 2) +
    labs(title = paste("Boxplot:", var),
         y = var) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  plot_box[[var]] <- p
}

g_box <- do.call(grid.arrange, c(plot_box, ncol = 4))
ggsave("wine_boxplots.png", g_box, width = 14, height = 10, dpi = 300)

# ------------------------------------------------------------------------------
# 14. VISUALIZACIONES - MATRIZ DE CORRELACIÓN
# ------------------------------------------------------------------------------

cat("\n=== GENERANDO MATRICES DE CORRELACIÓN ===\n")

# Matriz de correlación con coeficientes numéricos (método color)
png("correlation_matrix.png", width = 2000, height = 2000, res = 300, type = "cairo")
corrplot(correlation_matrix, 
         method = "color",
         type = "lower",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 1.2,
         cl.cex = 1,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         mar = c(0,3,2,0))
title(main = "Matriz de Correlación - Wine Quality Dataset", cex.main = 1.5)
dev.off()
cat("Generado: correlation_matrix.png (con coeficientes numéricos)\n")

# Matriz de correlación con círculos (visualización alternativa)
png("correlation_matrix_circles.png", width = 2000, height = 2000, res = 300, type = "cairo")
corrplot(correlation_matrix, 
         method = "circle", 
         type = "lower",
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 1.2,
         cl.cex = 1,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         mar = c(0,3,2,0))
title(main = "Matriz de Correlación - Wine Quality Dataset (Círculos)", cex.main = 1.5)
dev.off()
cat("Generado: correlation_matrix_circles.png (visualización con círculos)\n")

# ------------------------------------------------------------------------------
# 15. VISUALIZACIONES - QUALITY DISTRIBUTION
# ------------------------------------------------------------------------------

png("quality_distribution.png", width = 1200, height = 800, res = 150)
barplot(quality_freq,
        main = "Distribución de Calificaciones de Quality",
        xlab = "Quality Score",
        ylab = "Frecuencia",
        col = "steelblue",
        border = "black")
text(x = seq_along(quality_freq) * 1.2 - 0.5, 
     y = quality_freq + max(quality_freq) * 0.05,
     labels = quality_freq,
     cex = 0.9)
dev.off()

# ------------------------------------------------------------------------------
# 16. VISUALIZACIONES - SCATTER PLOTS CON QUALITY
# ------------------------------------------------------------------------------

top_vars <- names(sort(abs(correlation_matrix[, "quality"]), decreasing = TRUE)[2:7])

png("wine_scatter_quality.png", width = 2100, height = 1400, res = 150)
par(mfrow = c(2, 3))
for(var in top_vars) {
  plot(wine_data[[var]], wine_data$quality,
       main = paste("Quality vs", var),
       xlab = var,
       ylab = "Quality",
       pch = 19,
       col = rgb(0, 0, 1, 0.3),
       cex = 0.8)
  
  abline(lm(wine_data$quality ~ wine_data[[var]]), col = "red", lwd = 2)
  
  cor_val <- cor(wine_data[[var]], wine_data$quality, use = "complete.obs")
  legend("topright", 
         legend = sprintf("r = %.3f", cor_val),
         bty = "n",
         cex = 1.2)
}
dev.off()

# ------------------------------------------------------------------------------
# 17. VISUALIZACIONES - BOXPLOTS POR TYPE (RED VS WHITE)
# ------------------------------------------------------------------------------

png("wine_boxplots_by_type.png", width = 2100, height = 1400, res = 150)
par(mfrow = c(3, 4))

# Solo usar las 11 variables numéricas (excluyendo quality)
vars_for_type_comparison <- numeric_cols[numeric_cols != "quality"]

for(col in vars_for_type_comparison) {
  boxplot(wine_data[[col]] ~ wine_data$type,
          main = paste("Boxplot:", col),
          xlab = "Wine Type",
          ylab = col,
          col = c("red" = "#C0504D", "white" = "#9BBB59"),
          border = "black",
          names = c("Red", "White"))
}

# Agregar el boxplot de quality comparado por type
boxplot(wine_data$quality ~ wine_data$type,
        main = "Boxplot: type",
        xlab = "Wine Type",
        ylab = "Quality Score",
        col = c("red" = "#C0504D", "white" = "#9BBB59"),
        border = "black",
        names = c("Red", "White"))

dev.off()

# ------------------------------------------------------------------------------
# 18. TRANSFORMACIONES LOGARÍTMICAS
# ------------------------------------------------------------------------------

cat("\n=== TRANSFORMACIONES LOGARÍTMICAS ===\n")

skewed_vars <- descriptive_stats$variable[abs(descriptive_stats$asimetria) > 1]
cat("Variables con |asimetría| > 1:", paste(skewed_vars, collapse = ", "), "\n\n")

for(var in skewed_vars) {
  if(all(wine_data[[var]] > 0, na.rm = TRUE)) {
    log_var_name <- paste0("log_", var)
    wine_data[[log_var_name]] <- log(wine_data[[var]])
    cat(sprintf("Creada %s (asimetría = %.3f)\n", 
                log_var_name, 
                skewness(wine_data[[log_var_name]], na.rm = TRUE)))
  }
}

# ------------------------------------------------------------------------------
# 19. EXPORTACIÓN DE RESULTADOS
# ------------------------------------------------------------------------------

cat("\n=== EXPORTANDO RESULTADOS A CSV ===\n")

write.csv(descriptive_stats, "descriptive_statistics.csv", row.names = FALSE)
cat("Exportado: descriptive_statistics.csv\n")

write.csv(outlier_summary, "outlier_summary.csv", row.names = FALSE)
cat("Exportado: outlier_summary.csv\n")

write.csv(quality_table, "quality_distribution.csv", row.names = FALSE)
cat("Exportado: quality_distribution.csv\n")

write.csv(correlation_matrix, "correlation_matrix.csv")
cat("Exportado: correlation_matrix.csv\n")

write.csv(anova_results, "anova_results.csv", row.names = FALSE)
cat("Exportado: anova_results.csv\n")

write.csv(vif_results, "vif_results.csv", row.names = FALSE)
cat("Exportado: vif_results.csv\n")

# ------------------------------------------------------------------------------
# 20. REPORTE FINAL
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("REPORTE FINAL - WINE QUALITY ANALYSIS\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n1. CARACTERÍSTICAS DEL DATASET:\n")
cat("   - Observaciones:", nrow(wine_data), "\n")
cat("   - Variables originales:", length(numeric_cols), "\n")
cat("   - Casos completos:", sum(complete.cases(wine_data)), "\n")
cat("   - Valores faltantes:", sum(is.na(wine_data)), "\n")
cat("   - Duplicados:", duplicates, "\n")

cat("\n2. VARIABLE QUALITY:\n")
cat("   - Rango:", min(wine_data$quality), "-", max(wine_data$quality), "\n")
cat("   - Media:", round(mean(wine_data$quality), 2), "\n")
cat("   - Mediana:", median(wine_data$quality), "\n")
cat("   - Más frecuente:", names(which.max(quality_freq)), 
    "(", max(quality_freq), "observaciones)\n")

cat("\n3. TOP 5 CORRELACIONES CON QUALITY:\n")
top_cor_quality <- sort(correlation_matrix[, "quality"], decreasing = TRUE)[2:6]
for(i in seq_along(top_cor_quality)) {
  cat(sprintf("   - %s: r = %.3f\n", names(top_cor_quality)[i], top_cor_quality[i]))
}

cat("\n4. LIMITACIONES IDENTIFICADAS:\n")
cat("   - Variables con >10% outliers:", 
    sum(outlier_summary$Pct_Outliers > 10), "\n")
cat("   - Variables con VIF alto (>10):", 
    sum(vif_results$VIF > 10), "\n")
cat("   - Variables no normales (p<0.05):", 
    sum(normality_results$p_value < 0.05), "\n")

cat("\n5. ARCHIVOS GENERADOS:\n")
cat("   CSV:\n")
cat("   - descriptive_statistics.csv\n")
cat("   - correlation_matrix.csv\n")
cat("   - outlier_summary.csv\n")
cat("   - quality_distribution.csv\n")
cat("   - anova_results.csv\n")
cat("   - vif_results.csv\n")
cat("\n   PNG:\n")
cat("   - wine_histograms.png\n")
cat("   - wine_qqplots.png\n")
cat("   - wine_boxplots.png\n")
cat("   - correlation_matrix.png (con coeficientes)\n")
cat("   - correlation_matrix_circles.png (con círculos)\n")
cat("   - quality_distribution.png\n")
cat("   - wine_scatter_quality.png\n")
cat("   - wine_boxplots_by_type.png (RED vs WHITE comparison)\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Información de sesión R:\n")
print(sessionInfo())