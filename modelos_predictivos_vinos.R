#----------------------------------------------------------------------
# MODELOS PREDICTIVOS - Wine Quality Dataset
# Sección 3: Preparación de Datos, MRLS, MRLM y Evaluación
#
# SUPUESTOS VERIFICADOS EN ESTE SCRIPT:
# 1. Linealidad - Inspección gráfica (scatter plots, plot(model))
# 2. Media condicional cero - Test RESET de Ramsey
# 3. Homocedasticidad - Test de Breusch-Pagan
# 4. No autocorrelación - Test de Durbin-Watson
# 5. Normalidad - Test de Jarque-Bera
#----------------------------------------------------------------------
library(tidyverse)   # Manipulación de datos y gráficos
library(moments)     # Para cálculos de asimetría y curtosis
library(lmtest)      # Tests de modelos (bptest, resettest, dwtest)
library(car)         # Tests F y VIF
library(performance) # Diagnóstico de modelos
library(stargazer)   # Comparación de modelos
library(corrplot)    # Matriz de correlación
library(fBasics)     # Test de Jarque-Bera para normalidad

set.seed(2145)  # Reproducibilidad

#----------------------------------------------------------------------
# 1. PREPARACIÓN DE DATOS
#----------------------------------------------------------------------

# Cargar datos
whitewine <- read.csv("wine+quality/winequality-white.csv", sep = ";")
redwine <- read.csv("wine+quality/winequality-red.csv", sep = ";")

# Crear datasets separados por tipo de vino (análisis estratificado)
# CRÍTICO: El proyecto requiere análisis SEPARADO de vinos tintos y blancos
wine_data_red <- redwine %>%
  mutate(type = "red")

wine_data_white <- whitewine %>%
  mutate(type = "white")

# Dataset combinado (solo para comparaciones finales)
wine_data <- bind_rows(wine_data_white, wine_data_red)

cat("===== PREPARACIÓN DE DATOS =====\n")
cat("Vino Tinto - Observaciones:", nrow(wine_data_red), "\n")
cat("Vino Blanco - Observaciones:", nrow(wine_data_white), "\n")
cat("Total combinado:", nrow(wine_data), "\n\n")

# Verificar estructura
str(wine_data_red)
str(wine_data_white)

# Estadísticas descriptivas básicas por tipo
summary(wine_data_red %>% select(-type))
summary(wine_data_white %>% select(-type))


#//////////////////////////////////////////////////////////////////////////////////////
# 2. MODELOS DE REGRESIÓN LINEAL SIMPLE (MRLS) ----
#//////////////////////////////////////////////////////////////////////////////////////

cat("\n\n===== MODELOS DE REGRESIÓN LINEAL SIMPLE (MRLS) =====\n\n")

#-----------------------------
# 2.1 MRLS - VINO TINTO
#-----------------------------

cat("--- MODELO 1: MRLS Vino Tinto (Quality ~ Alcohol) ---\n")

# Gráfico de dispersión con recta de regresión
ggplot(wine_data_red, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.5, color = "#8B0000") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Quality vs Alcohol - Vino Tinto",
       subtitle = "Modelo de Regresión Lineal Simple",
       x = "Alcohol (%)",
       y = "Quality (0-10)") +
  theme_minimal()

ggsave("mrls_red_scatter.png", width = 8, height = 6, dpi = 300)

# Estimación del modelo
M1_red <- lm(quality ~ alcohol, data = wine_data_red)
summary(M1_red)

# Guardar predicciones y residuos
wine_data_red <- wine_data_red %>%
  mutate(
    pred1 = predict(M1_red),
    resid1 = resid(M1_red)
  )

# Métricas del modelo
M1_red_R2 <- summary(M1_red)$r.squared
M1_red_R2_adj <- summary(M1_red)$adj.r.squared
M1_red_sigma <- summary(M1_red)$sigma
M1_red_SCR <- deviance(M1_red)                    # RSS (Suma de Cuadrados Residual)
M1_red_SCT <- M1_red_SCR / (1 - M1_red_R2)        # TSS (Suma de Cuadrados Total)
M1_red_SCE <- M1_red_SCT - M1_red_SCR              # ESS (Suma de Cuadrados Explicada)
M1_red_MSE <- mean(wine_data_red$resid1^2)
M1_red_MAE <- mean(abs(wine_data_red$resid1))
M1_red_RMSE <- sqrt(M1_red_MSE)

cat("\nMétricas M1 (Vino Tinto):\n")
cat("R²:", round(M1_red_R2, 4), "\n")
cat("R² ajustado:", round(M1_red_R2_adj, 4), "\n")
cat("RMSE:", round(M1_red_RMSE, 4), "\n")
cat("MAE:", round(M1_red_MAE, 4), "\n\n")

# Intervalos de confianza para coeficientes
cat("Intervalos de Confianza 95% para coeficientes:\n")
print(confint(M1_red, level = 0.95))

# Gráfico de diagnóstico
par(mfrow = c(2,2))
plot(M1_red)
par(mfrow = c(1,1))


#-----------------------------
# 2.2 MRLS - VINO BLANCO
#-----------------------------

cat("\n--- MODELO 2: MRLS Vino Blanco (Quality ~ Alcohol) ---\n")

# Gráfico de dispersión con recta de regresión
ggplot(wine_data_white, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.5, color = "#FFD700") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Quality vs Alcohol - Vino Blanco",
       subtitle = "Modelo de Regresión Lineal Simple",
       x = "Alcohol (%)",
       y = "Quality (0-10)") +
  theme_minimal()

ggsave("mrls_white_scatter.png", width = 8, height = 6, dpi = 300)

# Estimación del modelo
M1_white <- lm(quality ~ alcohol, data = wine_data_white)
summary(M1_white)

# Guardar predicciones y residuos
wine_data_white <- wine_data_white %>%
  mutate(
    pred1 = predict(M1_white),
    resid1 = resid(M1_white)
  )

# Métricas del modelo
M1_white_R2 <- summary(M1_white)$r.squared
M1_white_R2_adj <- summary(M1_white)$adj.r.squared
M1_white_sigma <- summary(M1_white)$sigma
M1_white_SCR <- deviance(M1_white)
M1_white_SCT <- M1_white_SCR / (1 - M1_white_R2)
M1_white_SCE <- M1_white_SCT - M1_white_SCR
M1_white_MSE <- mean(wine_data_white$resid1^2)
M1_white_MAE <- mean(abs(wine_data_white$resid1))
M1_white_RMSE <- sqrt(M1_white_MSE)

cat("\nMétricas M1 (Vino Blanco):\n")
cat("R²:", round(M1_white_R2, 4), "\n")
cat("R² ajustado:", round(M1_white_R2_adj, 4), "\n")
cat("RMSE:", round(M1_white_RMSE, 4), "\n")
cat("MAE:", round(M1_white_MAE, 4), "\n\n")

# Intervalos de confianza para coeficientes
cat("Intervalos de Confianza 95% para coeficientes:\n")
print(confint(M1_white, level = 0.95))

# Gráfico de diagnóstico
par(mfrow = c(2,2))
plot(M1_white)
par(mfrow = c(1,1))


#//////////////////////////////////////////////////////////////////////////////////////
# 3. MODELOS DE REGRESIÓN LINEAL MÚLTIPLE (MRLM) ----
#//////////////////////////////////////////////////////////////////////////////////////

cat("\n\n===== MODELOS DE REGRESIÓN LINEAL MÚLTIPLE (MRLM) =====\n\n")

#-----------------------------
# 3.1 MRLM - VINO TINTO (Modelo Completo)
#-----------------------------

cat("--- MODELO 3: MRLM Vino Tinto (todas las variables) ---\n")

# Modelo con todas las variables predictoras
M2_red <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
               residual.sugar + chlorides + free.sulfur.dioxide + 
               total.sulfur.dioxide + density + pH + sulphates + alcohol,
             data = wine_data_red)

summary(M2_red)

# Guardar predicciones y residuos
wine_data_red <- wine_data_red %>%
  mutate(
    pred2 = predict(M2_red),
    resid2 = resid(M2_red)
  )

# Métricas del modelo
M2_red_R2 <- summary(M2_red)$r.squared
M2_red_R2_adj <- summary(M2_red)$adj.r.squared
M2_red_sigma <- summary(M2_red)$sigma
M2_red_SCR <- deviance(M2_red)
M2_red_SCT <- M2_red_SCR / (1 - M2_red_R2)
M2_red_SCE <- M2_red_SCT - M2_red_SCR
M2_red_MSE <- mean(wine_data_red$resid2^2)
M2_red_MAE <- mean(abs(wine_data_red$resid2))
M2_red_RMSE <- sqrt(M2_red_MSE)
M2_red_AIC <- AIC(M2_red)
M2_red_BIC <- BIC(M2_red)

cat("\nMétricas M2 (Vino Tinto - Modelo Completo):\n")
cat("R²:", round(M2_red_R2, 4), "\n")
cat("R² ajustado:", round(M2_red_R2_adj, 4), "\n")
cat("RMSE:", round(M2_red_RMSE, 4), "\n")
cat("MAE:", round(M2_red_MAE, 4), "\n")
cat("AIC:", round(M2_red_AIC, 2), "\n")
cat("BIC:", round(M2_red_BIC, 2), "\n\n")

# Test de significancia global (Test F)
cat("Test F de significancia global:\n")
cat("F-statistic:", summary(M2_red)$fstatistic[1], "\n")
cat("p-value:", pf(summary(M2_red)$fstatistic[1], 
                     summary(M2_red)$fstatistic[2],
                     summary(M2_red)$fstatistic[3], 
                     lower.tail = FALSE), "\n\n")

# Análisis de multicolinealidad (VIF)
cat("Factor de Inflación de Varianza (VIF):\n")
print(vif(M2_red))
cat("\n")

# Gráficos de diagnóstico
par(mfrow = c(2,2))
plot(M2_red)
par(mfrow = c(1,1))


#-----------------------------
# 3.2 MRLM - VINO BLANCO (Modelo Completo)
#-----------------------------

cat("\n--- MODELO 4: MRLM Vino Blanco (todas las variables) ---\n")

# Modelo con todas las variables predictoras
M2_white <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
                 residual.sugar + chlorides + free.sulfur.dioxide + 
                 total.sulfur.dioxide + density + pH + sulphates + alcohol,
               data = wine_data_white)

summary(M2_white)

# Guardar predicciones y residuos
wine_data_white <- wine_data_white %>%
  mutate(
    pred2 = predict(M2_white),
    resid2 = resid(M2_white)
  )

# Métricas del modelo
M2_white_R2 <- summary(M2_white)$r.squared
M2_white_R2_adj <- summary(M2_white)$adj.r.squared
M2_white_sigma <- summary(M2_white)$sigma
M2_white_SCR <- deviance(M2_white)
M2_white_SCT <- M2_white_SCR / (1 - M2_white_R2)
M2_white_SCE <- M2_white_SCT - M2_white_SCR
M2_white_MSE <- mean(wine_data_white$resid2^2)
M2_white_MAE <- mean(abs(wine_data_white$resid2))
M2_white_RMSE <- sqrt(M2_white_MSE)
M2_white_AIC <- AIC(M2_white)
M2_white_BIC <- BIC(M2_white)

cat("\nMétricas M2 (Vino Blanco - Modelo Completo):\n")
cat("R²:", round(M2_white_R2, 4), "\n")
cat("R² ajustado:", round(M2_white_R2_adj, 4), "\n")
cat("RMSE:", round(M2_white_RMSE, 4), "\n")
cat("MAE:", round(M2_white_MAE, 4), "\n")
cat("AIC:", round(M2_white_AIC, 2), "\n")
cat("BIC:", round(M2_white_BIC, 2), "\n\n")

# Test de significancia global (Test F)
cat("Test F de significancia global:\n")
cat("F-statistic:", summary(M2_white)$fstatistic[1], "\n")
cat("p-value:", pf(summary(M2_white)$fstatistic[1], 
                     summary(M2_white)$fstatistic[2],
                     summary(M2_white)$fstatistic[3], 
                     lower.tail = FALSE), "\n\n")

# Análisis de multicolinealidad (VIF)
cat("Factor de Inflación de Varianza (VIF):\n")
print(vif(M2_white))
cat("\n")

# Gráficos de diagnóstico
par(mfrow = c(2,2))
plot(M2_white)
par(mfrow = c(1,1))


#-----------------------------
# 3.3 MRLM - VINO TINTO (Modelo Reducido con variables significativas)
#-----------------------------

cat("\n--- MODELO 5: MRLM Vino Tinto (variables significativas) ---\n")

# Basándonos en el análisis del modelo completo, seleccionamos variables significativas
# Este modelo incluye solo las variables que mostraron ser significativas en M2_red
M3_red <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
               total.sulfur.dioxide + pH + sulphates + alcohol,
             data = wine_data_red)

summary(M3_red)

# Guardar predicciones y residuos
wine_data_red <- wine_data_red %>%
  mutate(
    pred3 = predict(M3_red),
    resid3 = resid(M3_red)
  )

# Métricas del modelo
M3_red_R2 <- summary(M3_red)$r.squared
M3_red_R2_adj <- summary(M3_red)$adj.r.squared
M3_red_RMSE <- sqrt(mean(wine_data_red$resid3^2))
M3_red_MAE <- mean(abs(wine_data_red$resid3))
M3_red_AIC <- AIC(M3_red)
M3_red_BIC <- BIC(M3_red)

cat("\nMétricas M3 (Vino Tinto - Modelo Reducido):\n")
cat("R²:", round(M3_red_R2, 4), "\n")
cat("R² ajustado:", round(M3_red_R2_adj, 4), "\n")
cat("RMSE:", round(M3_red_RMSE, 4), "\n")
cat("MAE:", round(M3_red_MAE, 4), "\n")
cat("AIC:", round(M3_red_AIC, 2), "\n")
cat("BIC:", round(M3_red_BIC, 2), "\n\n")


#-----------------------------
# 3.4 MRLM - VINO BLANCO (Modelo Reducido con variables significativas)
#-----------------------------

cat("\n--- MODELO 6: MRLM Vino Blanco (variables significativas) ---\n")

# Basándonos en el análisis del modelo completo, seleccionamos variables significativas
M3_white <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                 free.sulfur.dioxide + density + pH + sulphates + alcohol,
               data = wine_data_white)

summary(M3_white)

# Guardar predicciones y residuos
wine_data_white <- wine_data_white %>%
  mutate(
    pred3 = predict(M3_white),
    resid3 = resid(M3_white)
  )

# Métricas del modelo
M3_white_R2 <- summary(M3_white)$r.squared
M3_white_R2_adj <- summary(M3_white)$adj.r.squared
M3_white_RMSE <- sqrt(mean(wine_data_white$resid3^2))
M3_white_MAE <- mean(abs(wine_data_white$resid3))
M3_white_AIC <- AIC(M3_white)
M3_white_BIC <- BIC(M3_white)

cat("\nMétricas M3 (Vino Blanco - Modelo Reducido):\n")
cat("R²:", round(M3_white_R2, 4), "\n")
cat("R² ajustado:", round(M3_white_R2_adj, 4), "\n")
cat("RMSE:", round(M3_white_RMSE, 4), "\n")
cat("MAE:", round(M3_white_MAE, 4), "\n")
cat("AIC:", round(M3_white_AIC, 2), "\n")
cat("BIC:", round(M3_white_BIC, 2), "\n\n")


#//////////////////////////////////////////////////////////////////////////////////////
# 4. EVALUACIÓN Y COMPARACIÓN DE MODELOS ----
#//////////////////////////////////////////////////////////////////////////////////////

cat("\n\n===== EVALUACIÓN Y COMPARACIÓN DE MODELOS =====\n\n")

#-----------------------------
# 4.1 Comparación de modelos - VINO TINTO
#-----------------------------

cat("--- Comparación de Modelos: VINO TINTO ---\n\n")

# Tabla comparativa de métricas
comparacion_red <- tibble(
  Modelo = c("M1: MRLS (alcohol)", 
             "M2: MRLM (completo)", 
             "M3: MRLM (reducido)"),
  R2 = c(M1_red_R2, M2_red_R2, M3_red_R2),
  R2_ajustado = c(M1_red_R2_adj, M2_red_R2_adj, M3_red_R2_adj),
  RMSE = c(M1_red_RMSE, M2_red_RMSE, M3_red_RMSE),
  MAE = c(M1_red_MAE, M2_red_MAE, M3_red_MAE),
  AIC = c(AIC(M1_red), M2_red_AIC, M3_red_AIC),
  BIC = c(BIC(M1_red), M2_red_BIC, M3_red_BIC)
)

print(comparacion_red, n = Inf)

# Test F para comparar modelos anidados (M1 vs M2)
cat("\n\nTest F: M1 (MRLS) vs M2 (MRLM completo)\n")
anova(M1_red, M2_red)

# Test F para comparar modelos anidados (M3 vs M2)
cat("\nTest F: M3 (MRLM reducido) vs M2 (MRLM completo)\n")
anova(M3_red, M2_red)

# Test RESET de Ramsey para evaluar especificación del modelo
cat("\n\nTest RESET de Ramsey - Vino Tinto:\n")
cat("M1 (MRLS):\n")
print(resettest(M1_red, power = 2:3, type = "fitted"))
cat("\nM2 (MRLM completo):\n")
print(resettest(M2_red, power = 2:3, type = "fitted"))
cat("\nM3 (MRLM reducido):\n")
print(resettest(M3_red, power = 2:3, type = "fitted"))


#-----------------------------
# 4.2 Comparación de modelos - VINO BLANCO
#-----------------------------

cat("\n\n--- Comparación de Modelos: VINO BLANCO ---\n\n")

# Tabla comparativa de métricas
comparacion_white <- tibble(
  Modelo = c("M1: MRLS (alcohol)", 
             "M2: MRLM (completo)", 
             "M3: MRLM (reducido)"),
  R2 = c(M1_white_R2, M2_white_R2, M3_white_R2),
  R2_ajustado = c(M1_white_R2_adj, M2_white_R2_adj, M3_white_R2_adj),
  RMSE = c(M1_white_RMSE, M2_white_RMSE, M3_white_RMSE),
  MAE = c(M1_white_MAE, M2_white_MAE, M3_white_MAE),
  AIC = c(AIC(M1_white), M2_white_AIC, M3_white_AIC),
  BIC = c(BIC(M1_white), M2_white_BIC, M3_white_BIC)
)

print(comparacion_white, n = Inf)

# Test F para comparar modelos anidados (M1 vs M2)
cat("\n\nTest F: M1 (MRLS) vs M2 (MRLM completo)\n")
anova(M1_white, M2_white)

# Test F para comparar modelos anidados (M3 vs M2)
cat("\nTest F: M3 (MRLM reducido) vs M2 (MRLM completo)\n")
anova(M3_white, M2_white)

# Test RESET de Ramsey para evaluar especificación del modelo
cat("\n\nTest RESET de Ramsey - Vino Blanco:\n")
cat("M1 (MRLS):\n")
print(resettest(M1_white, power = 2:3, type = "fitted"))
cat("\nM2 (MRLM completo):\n")
print(resettest(M2_white, power = 2:3, type = "fitted"))
cat("\nM3 (MRLM reducido):\n")
print(resettest(M3_white, power = 2:3, type = "fitted"))


#-----------------------------
# 4.3 Diagnóstico completo de modelos
#-----------------------------

cat("\n\n===== DIAGNÓSTICO COMPLETO DE MODELOS =====\n\n")

#--- VINO TINTO ---
cat("--- DIAGNÓSTICO: VINO TINTO (Mejor modelo - M3) ---\n\n")

# 1. Test de Normalidad de Residuos: Jarque-Bera
cat("1. Test de Jarque-Bera (Normalidad de Residuos):\n")
cat("   H0: Los residuos siguen una distribución normal\n")
cat("   H1: Los residuos NO siguen una distribución normal\n\n")
jb_test_red <- jarqueberaTest(wine_data_red$resid3)
print(jb_test_red)
if (jb_test_red@test$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). Los residuos son aproximadamente normales.\n\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay evidencia de no normalidad en residuos.\n\n")
}

# 2. Test de Heterocedasticidad: Breusch-Pagan
cat("2. Test de Breusch-Pagan (Heterocedasticidad):\n")
cat("   H0: Homocedasticidad (varianza constante)\n")
cat("   H1: Heterocedasticidad (varianza NO constante)\n\n")
bp_test_red <- bptest(M3_red)
print(bp_test_red)
if (bp_test_red$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). Hay homocedasticidad.\n\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay heterocedasticidad presente.\n\n")
}

# 3. Test de Autocorrelación: Durbin-Watson
cat("3. Test de Durbin-Watson (Autocorrelación):\n")
cat("   H0: No hay autocorrelación en los residuos\n")
cat("   H1: Existe autocorrelación en los residuos\n\n")
dw_test_red <- dwtest(M3_red)
print(dw_test_red)
if (dw_test_red$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). No hay autocorrelación significativa.\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay autocorrelación presente.\n")
}
cat("   DW estadístico:", round(dw_test_red$statistic, 4), "(valores cercanos a 2 indican no autocorrelación)\n\n")

# 4. VIF (Multicolinealidad) - ya calculado antes, aquí resumen
cat("4. Factor de Inflación de Varianza (VIF) - Resumen:\n")
vif_red <- vif(M3_red)
print(vif_red)
if (max(vif_red) < 5) {
  cat("   Conclusión: No hay problemas de multicolinealidad (todos VIF < 5)\n\n")
} else if (max(vif_red) < 10) {
  cat("   Conclusión: Multicolinealidad moderada (algunos VIF entre 5-10)\n\n")
} else {
  cat("   Conclusión: Multicolinealidad severa (algunos VIF > 10)\n\n")
}

# 5. Estadísticas descriptivas de residuos
cat("5. Estadísticas Descriptivas de Residuos:\n")
cat("   Media:", round(mean(wine_data_red$resid3), 6), "(debe ser ≈ 0)\n")
cat("   Mediana:", round(median(wine_data_red$resid3), 4), "\n")
cat("   Desv. Est.:", round(sd(wine_data_red$resid3), 4), "\n")
cat("   Asimetría:", round(skewness(wine_data_red$resid3), 4), "(normal: ≈ 0)\n")
cat("   Curtosis:", round(kurtosis(wine_data_red$resid3), 4), "(normal: ≈ 3)\n\n")

# Histograma de residuos - Vino Tinto
ggplot(wine_data_red, aes(x = resid3)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "#8B0000", 
                 alpha = 0.7,
                 color = "black") +
  geom_density(color = "darkblue", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(wine_data_red$resid3), 
                           sd = sd(wine_data_red$resid3)),
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(title = "Distribución de Residuos - Vino Tinto (M3)",
       subtitle = "Línea azul: densidad observada | Línea roja: distribución normal teórica",
       x = "Residuos",
       y = "Densidad") +
  theme_minimal()

ggsave("residuos_red_hist.png", width = 10, height = 6, dpi = 300)


#--- VINO BLANCO ---
cat("\n--- DIAGNÓSTICO: VINO BLANCO (Mejor modelo - M3) ---\n\n")

# 1. Test de Normalidad de Residuos: Jarque-Bera
cat("1. Test de Jarque-Bera (Normalidad de Residuos):\n")
cat("   H0: Los residuos siguen una distribución normal\n")
cat("   H1: Los residuos NO siguen una distribución normal\n\n")
jb_test_white <- jarqueberaTest(wine_data_white$resid3)
print(jb_test_white)
if (jb_test_white@test$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). Los residuos son aproximadamente normales.\n\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay evidencia de no normalidad en residuos.\n\n")
}

# 2. Test de Heterocedasticidad: Breusch-Pagan
cat("2. Test de Breusch-Pagan (Heterocedasticidad):\n")
cat("   H0: Homocedasticidad (varianza constante)\n")
cat("   H1: Heterocedasticidad (varianza NO constante)\n\n")
bp_test_white <- bptest(M3_white)
print(bp_test_white)
if (bp_test_white$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). Hay homocedasticidad.\n\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay heterocedasticidad presente.\n\n")
}

# 3. Test de Autocorrelación: Durbin-Watson
cat("3. Test de Durbin-Watson (Autocorrelación):\n")
cat("   H0: No hay autocorrelación en los residuos\n")
cat("   H1: Existe autocorrelación en los residuos\n\n")
dw_test_white <- dwtest(M3_white)
print(dw_test_white)
if (dw_test_white$p.value > 0.05) {
  cat("   Conclusión: No rechazamos H0 (p > 0.05). No hay autocorrelación significativa.\n")
} else {
  cat("   Conclusión: Rechazamos H0 (p < 0.05). Hay autocorrelación presente.\n")
}
cat("   DW estadístico:", round(dw_test_white$statistic, 4), "(valores cercanos a 2 indican no autocorrelación)\n\n")

# 4. VIF (Multicolinealidad) - ya calculado antes, aquí resumen
cat("4. Factor de Inflación de Varianza (VIF) - Resumen:\n")
vif_white <- vif(M3_white)
print(vif_white)
if (max(vif_white) < 5) {
  cat("   Conclusión: No hay problemas de multicolinealidad (todos VIF < 5)\n\n")
} else if (max(vif_white) < 10) {
  cat("   Conclusión: Multicolinealidad moderada (algunos VIF entre 5-10)\n\n")
} else {
  cat("   Conclusión: Multicolinealidad severa (algunos VIF > 10)\n\n")
}

# 5. Estadísticas descriptivas de residuos
cat("5. Estadísticas Descriptivas de Residuos:\n")
cat("   Media:", round(mean(wine_data_white$resid3), 6), "(debe ser ≈ 0)\n")
cat("   Mediana:", round(median(wine_data_white$resid3), 4), "\n")
cat("   Desv. Est.:", round(sd(wine_data_white$resid3), 4), "\n")
cat("   Asimetría:", round(skewness(wine_data_white$resid3), 4), "(normal: ≈ 0)\n")
cat("   Curtosis:", round(kurtosis(wine_data_white$resid3), 4), "(normal: ≈ 3)\n\n")

# Histograma de residuos - Vino Blanco
ggplot(wine_data_white, aes(x = resid3)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "#FFD700", 
                 alpha = 0.7,
                 color = "black") +
  geom_density(color = "darkblue", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(wine_data_white$resid3), 
                           sd = sd(wine_data_white$resid3)),
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(title = "Distribución de Residuos - Vino Blanco (M3)",
       subtitle = "Línea azul: densidad observada | Línea roja: distribución normal teórica",
       x = "Residuos",
       y = "Densidad") +
  theme_minimal()

ggsave("residuos_white_hist.png", width = 10, height = 6, dpi = 300)


#-----------------------------
# 4.4 Visualización comparativa usando stargazer
#-----------------------------

cat("\n\n--- TABLA COMPARATIVA DE MODELOS (formato texto) ---\n\n")

# Comparación visual para Vino Tinto
cat("VINO TINTO:\n")
stargazer(M1_red, M2_red, M3_red, 
          type = "text",
          title = "Comparación de Modelos - Vino Tinto",
          column.labels = c("MRLS", "MRLM Completo", "MRLM Reducido"),
          model.numbers = FALSE,
          digits = 3)

# Comparación visual para Vino Blanco
cat("\n\nVINO BLANCO:\n")
stargazer(M1_white, M2_white, M3_white, 
          type = "text",
          title = "Comparación de Modelos - Vino Blanco",
          column.labels = c("MRLS", "MRLM Completo", "MRLM Reducido"),
          model.numbers = FALSE,
          digits = 3)


#-----------------------------
# 4.5 Predicción con nuevas observaciones
#-----------------------------

cat("\n\n--- PREDICCIÓN CON NUEVAS OBSERVACIONES ---\n\n")

# Ejemplo de predicción para vino tinto con valores típicos
nuevo_vino_red <- data.frame(
  fixed.acidity = mean(wine_data_red$fixed.acidity),
  volatile.acidity = 0.4,  # Valor bajo (buena calidad)
  citric.acid = mean(wine_data_red$citric.acid),
  residual.sugar = mean(wine_data_red$residual.sugar),
  chlorides = 0.05,  # Valor bajo (buena calidad)
  free.sulfur.dioxide = 15,
  total.sulfur.dioxide = 40,
  density = mean(wine_data_red$density),
  pH = 3.3,
  sulphates = 0.7,  # Valor alto (buena calidad)
  alcohol = 12  # Valor alto (buena calidad)
)

cat("Predicción para Vino Tinto (M3 - mejor modelo):\n")
pred_red <- predict(M3_red, newdata = nuevo_vino_red, interval = "prediction", level = 0.95)
print(pred_red)
cat("\n")

# Ejemplo de predicción para vino blanco con valores típicos
nuevo_vino_white <- data.frame(
  fixed.acidity = mean(wine_data_white$fixed.acidity),
  volatile.acidity = 0.2,  # Valor bajo (buena calidad)
  citric.acid = mean(wine_data_white$citric.acid),
  residual.sugar = 5,  # Valor moderado
  chlorides = 0.03,  # Valor bajo (buena calidad)
  free.sulfur.dioxide = 40,
  total.sulfur.dioxide = 120,
  density = mean(wine_data_white$density),
  pH = 3.2,
  sulphates = 0.5,
  alcohol = 11  # Valor alto (buena calidad)
)

cat("Predicción para Vino Blanco (M3 - mejor modelo):\n")
pred_white <- predict(M3_white, newdata = nuevo_vino_white, interval = "prediction", level = 0.95)
print(pred_white)


#//////////////////////////////////////////////////////////////////////////////////////
# 5. CONCLUSIONES Y EXPORTACIÓN DE RESULTADOS ----
#//////////////////////////////////////////////////////////////////////////////////////

cat("\n\n===== RESUMEN FINAL =====\n\n")

cat("VINO TINTO - Mejor Modelo (M3 - MRLM Reducido):\n")
cat("  R² ajustado:", round(M3_red_R2_adj, 4), "\n")
cat("  RMSE:", round(M3_red_RMSE, 4), "\n")
cat("  AIC:", round(M3_red_AIC, 2), "\n")
cat("  BIC:", round(M3_red_BIC, 2), "\n\n")

cat("VINO BLANCO - Mejor Modelo (M3 - MRLM Reducido):\n")
cat("  R² ajustado:", round(M3_white_R2_adj, 4), "\n")
cat("  RMSE:", round(M3_white_RMSE, 4), "\n")
cat("  AIC:", round(M3_white_AIC, 2), "\n")
cat("  BIC:", round(M3_white_BIC, 2), "\n\n")

# Guardar resultados finales
save(M1_red, M2_red, M3_red, 
     M1_white, M2_white, M3_white,
     comparacion_red, comparacion_white,
     file = "modelos_vinos_resultados.RData")

cat("¡Análisis completado! Resultados guardados en 'modelos_vinos_resultados.RData'\n")
