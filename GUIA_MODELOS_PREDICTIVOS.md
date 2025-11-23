# GUÍA DE MODELOS PREDICTIVOS - WINE QUALITY DATASET
# Documentación del Análisis

## ESTRUCTURA DEL ANÁLISIS

### 1. PREPARACIÓN DE DATOS
- Carga de datos separados para vino tinto y blanco
- IMPORTANTE: Análisis ESTRATIFICADO (separado por tipo de vino)
- No se combinan los datos porque tienen características diferentes
- Verificación de estructura y estadísticas descriptivas básicas

### 2. MODELOS DE REGRESIÓN LINEAL SIMPLE (MRLS)

#### Modelo 1 - Vino Tinto: quality ~ alcohol
Plantea la relación más básica: ¿el contenido de alcohol predice la calidad?

Ecuación: Quality = β₀ + β₁(Alcohol) + u

Análisis incluye:
- Gráfico de dispersión con recta de regresión
- Estimación por MCO (Mínimos Cuadrados Ordinarios)
- R², R² ajustado, RMSE, MAE
- Intervalos de confianza para coeficientes
- Gráficos de diagnóstico (normalidad, homocedasticidad)

#### Modelo 2 - Vino Blanco: quality ~ alcohol
Misma estructura pero para vino blanco
Permite comparar si la relación alcohol-calidad difiere entre tipos

### 3. MODELOS DE REGRESIÓN LINEAL MÚLTIPLE (MRLM)

#### Modelo 3 - Vino Tinto (Completo)
Incluye TODAS las variables fisicoquímicas:
- fixed.acidity
- volatile.acidity
- citric.acid
- residual.sugar
- chlorides
- free.sulfur.dioxide
- total.sulfur.dioxide
- density
- pH
- sulphates
- alcohol

Ecuación: Quality = β₀ + β₁X₁ + β₂X₂ + ... + β₁₁X₁₁ + u

Análisis incluye:
- Estimación por MCO
- Test F de significancia global
- VIF (Factor de Inflación de Varianza) para detectar multicolinealidad
- Gráficos de diagnóstico completos

#### Modelo 4 - Vino Blanco (Completo)
Misma estructura pero para vino blanco

#### Modelo 5 - Vino Tinto (Reducido)
Solo incluye variables que resultaron SIGNIFICATIVAS en el modelo completo
Este es típicamente el "mejor modelo" porque:
- Elimina variables irrelevantes (parsimonia)
- Reduce multicolinealidad
- Mejora el R² ajustado
- Reduce AIC y BIC

#### Modelo 6 - Vino Blanco (Reducido)
Versión reducida para vino blanco

### 4. EVALUACIÓN Y COMPARACIÓN DE MODELOS

#### Métricas utilizadas:

**R² (Coeficiente de determinación)**
- Rango: 0 a 1
- Interpretación: % de variabilidad de Y explicada por X
- LIMITACIÓN: Siempre aumenta al agregar variables

**R² ajustado**
- Ajusta por número de variables
- Mejor para comparar modelos con distinto número de predictores
- Penaliza la inclusión de variables irrelevantes

**RMSE (Root Mean Squared Error)**
- Error cuadrático medio en unidades de Y
- Más bajo = mejor ajuste
- Penaliza errores grandes más que pequeños

**MAE (Mean Absolute Error)**
- Error promedio absoluto
- Más bajo = mejor ajuste
- Más robusto a outliers que RMSE

**AIC (Akaike Information Criterion)**
- Más bajo = mejor modelo
- Balance entre bondad de ajuste y complejidad
- Fórmula: AIC = -2log(L) + 2k (donde k = # parámetros)

**BIC (Bayesian Information Criterion)**
- Más bajo = mejor modelo
- Penaliza más la complejidad que AIC
- Fórmula: BIC = -2log(L) + k·log(n)

#### Tests estadísticos:

**Test F (ANOVA)**
Para comparar modelos anidados (uno es subconjunto del otro)
H₀: El modelo simple es suficiente
H₁: El modelo complejo es mejor
Si p-value < 0.05 → Rechazamos H₀ (el modelo complejo es mejor)

**Test RESET de Ramsey**
Evalúa si la forma funcional es correcta
H₀: La especificación del modelo es correcta
H₁: Hay problemas de especificación (variables omitidas, forma funcional incorrecta)
Si p-value < 0.05 → Hay problemas de especificación

**Test de Jarque-Bera**
Evalúa normalidad de los residuos basándose en asimetría y curtosis
H₀: Los residuos siguen una distribución normal
H₁: Los residuos NO siguen una distribución normal
Si p-value < 0.05 → Rechazamos normalidad
Importante: La violación de normalidad afecta principalmente a la inferencia (intervalos de confianza, tests de hipótesis), no tanto a las estimaciones puntuales por MCO

**Test de Breusch-Pagan**
Evalúa heterocedasticidad (varianza no constante de los residuos)
H₀: Homocedasticidad (Var(u) = σ² constante)
H₁: Heterocedasticidad (Var(u) varía con X)
Si p-value < 0.05 → Hay heterocedasticidad
Consecuencias: Los estimadores MCO siguen siendo insesgados pero NO eficientes, y los errores estándar son incorrectos
Solución: Usar errores robustos (HC1, HC3) con vcovHC()

**Factor de Inflación de Varianza (VIF)**
Detecta multicolinealidad entre predictores
- VIF = 1: No hay correlación
- VIF < 5: Multicolinealidad aceptable
- VIF 5-10: Multicolinealidad moderada
- VIF > 10: Multicolinealidad severa (problema serio)
Consecuencias: Los estimadores siguen siendo insesgados pero tienen varianza inflada (poco precisos)
Solución: Eliminar variables redundantes, usar análisis de componentes principales, o recolectar más datos

#### Análisis de Residuos:

Los residuos deben cumplir los supuestos del Modelo de Regresión Lineal Clásico (MRLC):

**Supuestos clave:**
1. **Media cero**: E[u] = 0
   - Se cumple automáticamente con intercepto en el modelo
   
2. **Homocedasticidad**: Var(u) = σ² constante para todo i
   - Se evalúa con: Test de Breusch-Pagan, gráfico Residuals vs Fitted
   - Violación → Errores estándar incorrectos (solución: errores robustos)
   
3. **Normalidad**: u ~ N(0, σ²)
   - Se evalúa con: Test de Jarque-Bera, QQ-plot, histograma de residuos
   - Violación → Intervalos de confianza y tests pueden ser imprecisos
   - Nota: Con muestras grandes (n > 30), la normalidad es menos crítica por TCL
   
4. **No autocorrelación**: Cov(uᵢ, uⱼ) = 0 para i ≠ j
   - Más relevante en series temporales
   - Se evalúa con: Test de Durbin-Watson

**Herramientas de evaluación:**
- **Histograma de residuos**: Debe parecerse a una campana de Gauss
- **QQ-plot** (gráfico cuantil-cuantil): Puntos deben seguir la línea diagonal
- **Residuals vs Fitted**: 
  - Patrones → Indican no linealidad o forma funcional incorrecta
  - Forma de embudo → Indica heterocedasticidad
  - Debe verse como nube aleatoria alrededor de cero
- **Scale-Location**: Detecta heterocedasticidad (línea horizontal es ideal)
- **Residuals vs Leverage**: Detecta observaciones influyentes (Cook's distance)

### 5. PREDICCIÓN

Una vez seleccionado el mejor modelo, se puede usar para predecir:

**Intervalo de Confianza** (para la media condicional E[Y|X])
Más estrecho, para promedios

**Intervalo de Predicción** (para observaciones individuales)
Más amplio, incluye variabilidad individual

---

## INTERPRETACIÓN DE COEFICIENTES

### En modelos lineales simples:
β₁ = cambio en Y por cada unidad de cambio en X

Ejemplo: Si β₁ = 0.35 para alcohol
→ Por cada 1% más de alcohol, la calidad aumenta 0.35 puntos (en promedio)

### En modelos múltiples:
β₁ = cambio en Y por cada unidad de cambio en X₁, **manteniendo constantes** las demás variables

Ejemplo: Si β₁ = 0.30 para alcohol en modelo múltiple
→ Por cada 1% más de alcohol, la calidad aumenta 0.30 puntos,
   manteniendo constantes acidez, pH, sulfatos, etc.

### Significancia estadística:

**p-value < 0.05** → Variable significativa al 5%
**p-value < 0.01** → Variable muy significativa al 1%
**p-value > 0.05** → Variable NO significativa

---

## ESTRATEGIA DE SELECCIÓN DEL MEJOR MODELO

1. Comparar R² ajustado (más alto = mejor)
2. Comparar AIC y BIC (más bajo = mejor)
3. Comparar RMSE y MAE (más bajo = mejor)
4. Verificar que no haya problemas en:
   - Test RESET (p > 0.05 es deseable)
   - VIF (< 5 es deseable)
   - Análisis de residuos (cumplir supuestos)
5. Preferir modelos más simples si la diferencia es pequeña (parsimonia)

---

## DIFERENCIAS CLAVE CON EL PRÁCTICO DEL PROFESOR

El práctico del profesor usa:
- ECH 2019 (Encuesta Continua de Hogares)
- Ponderadores muestrales (bc_pesoan)
- Variable dependiente: alquiler (continua)
- Regresores contextuales y características de vivienda

Nuestro proyecto usa:
- Wine Quality Dataset (Kaggle)
- Sin ponderadores (muestra directa)
- Variable dependiente: quality (ordinal tratada como continua)
- Regresores fisicoquímicos del vino

Adaptaciones realizadas:
✓ Análisis estratificado por tipo de vino
✓ Selección de variables basada en significancia estadística
✓ Comparación sistemática entre modelos
✓ Énfasis en predicción de calidad

---

## PRÓXIMOS PASOS SUGERIDOS (OPCIONAL)

Si quieres extender el análisis:
1. Probar transformaciones logarítmicas (como en Tema 6)
2. Incluir términos cuadráticos para relaciones no lineales
3. Explorar interacciones entre variables
4. Considerar quality como variable categórica (regresión logística ordinal)
5. Validación cruzada para evaluar capacidad predictiva

---

## ARCHIVOS GENERADOS

1. `modelos_predictivos_vinos.R` - Script completo del análisis
2. `modelos_vinos_resultados.RData` - Resultados guardados
3. `mrls_red_scatter.png` - Gráfico MRLS vino tinto
4. `mrls_white_scatter.png` - Gráfico MRLS vino blanco
5. `residuos_red_hist.png` - Histograma residuos vino tinto (con curva normal)
6. `residuos_white_hist.png` - Histograma residuos vino blanco (con curva normal)

**Nota**: Los histogramas de residuos incluyen tres elementos:
- Barras: Distribución observada de residuos
- Línea azul sólida: Densidad empírica
- Línea roja punteada: Distribución normal teórica para comparación

---

## REFERENCIAS

- Tema 6: Modelo de Regresión Lineal Simple (MRLS)
- Tema 7: Modelo de Regresión Lineal Múltiple (MRLM)
- Wooldridge, J.M. (2015). Introductory Econometrics: A Modern Approach
- James, G. et al. (2021). An Introduction to Statistical Learning
