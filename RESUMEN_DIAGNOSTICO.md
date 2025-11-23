# RESUMEN DE TESTS DE DIAGNÓSTICO AGREGADOS
## Análisis Completo de Modelos de Regresión

### ✅ DIAGNÓSTICO COMPLETO IMPLEMENTADO

El código ahora incluye una sección exhaustiva de diagnóstico (Sección 4.3) que evalúa:

---

## 1️⃣ TEST DE JARQUE-BERA (Normalidad de Residuos)

**¿Qué evalúa?**
- Si los residuos siguen una distribución normal

**¿Por qué importa?**
- La normalidad es necesaria para que los intervalos de confianza y los tests de hipótesis sean válidos
- Con muestras grandes (n > 30), es menos crítico por el Teorema Central del Límite

**Interpretación:**
- p-value > 0.05 → No rechazamos normalidad ✅
- p-value < 0.05 → Rechazamos normalidad ❌

**Consecuencias de violación:**
- Estimadores MCO siguen siendo insesgados
- Intervalos de confianza y p-values pueden ser imprecisos
- En muestras grandes, el impacto es menor

---

## 2️⃣ TEST DE BREUSCH-PAGAN (Heterocedasticidad)

**¿Qué evalúa?**
- Si la varianza de los residuos es constante (homocedasticidad)
- O si varía con los valores de X (heterocedasticidad)

**¿Por qué importa?**
- La heterocedasticidad hace que los errores estándar sean INCORRECTOS
- Los tests de hipótesis y los intervalos de confianza NO son confiables

**Interpretación:**
- p-value > 0.05 → Homocedasticidad (varianza constante) ✅
- p-value < 0.05 → Heterocedasticidad detectada ❌

**Consecuencias de violación:**
- Estimadores MCO siguen siendo insesgados
- Estimadores MCO NO son eficientes (no tienen mínima varianza)
- Errores estándar, tests t y tests F son INVÁLIDOS

**Solución si hay heterocedasticidad:**
```r
# Usar errores robustos
library(sandwich)
library(lmtest)
coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))
```

---

## 3️⃣ VIF - FACTOR DE INFLACIÓN DE VARIANZA (Multicolinealidad)

**¿Qué evalúa?**
- Correlación entre las variables independientes

**¿Por qué importa?**
- La multicolinealidad hace difícil separar el efecto individual de cada variable
- Aumenta la varianza de los estimadores (coeficientes poco precisos)

**Interpretación:**
- VIF < 5: No hay problema ✅
- VIF 5-10: Multicolinealidad moderada ⚠️
- VIF > 10: Multicolinealidad severa ❌

**Fórmula:**
VIF_j = 1 / (1 - R²_j)

donde R²_j es el R² de regresar X_j contra todas las demás X

**Consecuencias de multicolinealidad alta:**
- Estimadores siguen siendo insesgados
- Varianza de los estimadores es grande (poco precisos)
- Difícil determinar importancia relativa de variables correlacionadas
- Cambios pequeños en datos pueden causar grandes cambios en coeficientes

**Soluciones:**
1. Eliminar una de las variables altamente correlacionadas
2. Combinar variables correlacionadas
3. Aumentar el tamaño muestral
4. Usar regularización (Ridge, Lasso)

---

## 4️⃣ ESTADÍSTICAS DESCRIPTIVAS DE RESIDUOS

El código calcula y reporta:

**Media de residuos:**
- Debe ser ≈ 0 (se cumple automáticamente con intercepto)

**Asimetría (Skewness):**
- Normal: ≈ 0
- Positiva > 0: Cola a la derecha
- Negativa < 0: Cola a la izquierda

**Curtosis (Kurtosis):**
- Normal: ≈ 3
- > 3: Colas más pesadas que la normal (leptocúrtica)
- < 3: Colas más livianas que la normal (platicúrtica)

---

## 5️⃣ VISUALIZACIÓN MEJORADA DE RESIDUOS

Los histogramas ahora incluyen:

1. **Barras**: Distribución observada de los residuos
2. **Línea azul sólida**: Curva de densidad empírica
3. **Línea roja punteada**: Distribución normal teórica

**Cómo interpretar:**
- Si la línea azul coincide con la roja → Residuos normales ✅
- Si se alejan → Hay desviaciones de normalidad ❌

---

## 📊 FLUJO DE DIAGNÓSTICO COMPLETO

```
Para cada modelo (Vino Tinto y Vino Blanco):

1. ¿Los residuos son normales?
   → Test de Jarque-Bera
   → Histograma con curva normal
   → QQ-plot (en gráficos de diagnóstico)

2. ¿La varianza es constante?
   → Test de Breusch-Pagan
   → Gráfico Residuals vs Fitted

3. ¿Hay multicolinealidad?
   → VIF para cada variable
   → Revisar matriz de correlación

4. ¿La forma funcional es correcta?
   → Test RESET de Ramsey
   → Gráficos de diagnóstico

5. Estadísticas de residuos
   → Media ≈ 0
   → Asimetría ≈ 0
   → Curtosis ≈ 3
```

---

## 🎯 DECISIONES BASADAS EN DIAGNÓSTICO

### Si TODO está bien:
✅ Usar el modelo con confianza
✅ Intervalos de confianza son válidos
✅ Tests de hipótesis son confiables

### Si HAY heterocedasticidad:
⚠️ Usar errores robustos (vcovHC)
⚠️ Los coeficientes siguen siendo válidos
⚠️ Solo los errores estándar necesitan corrección

### Si HAY multicolinealidad severa:
⚠️ Considerar eliminar variables correlacionadas
⚠️ Los coeficientes pueden ser inestables
⚠️ La predicción del modelo puede seguir siendo buena

### Si NO hay normalidad:
⚠️ Con n grande, no es muy problemático
⚠️ Considerar transformaciones (log, Box-Cox)
⚠️ Los estimadores MCO siguen siendo insesgados

### Si el test RESET rechaza:
❌ Revisar forma funcional
❌ Considerar términos cuadráticos
❌ Considerar interacciones
❌ Revisar variables omitidas

---

## 📝 EJEMPLO DE INTERPRETACIÓN

**Vino Tinto - Modelo M3:**

```
1. Test de Jarque-Bera: p-value = 0.03
   → Rechazamos normalidad (hay desviación)
   → PERO: n = 1599 es grande → TCL aplica → No muy problemático
   
2. Test de Breusch-Pagan: p-value = 0.08
   → No rechazamos H0 → Homocedasticidad ✅
   → Errores estándar son válidos
   
3. VIF máximo = 4.2
   → Todos < 5 → No hay multicolinealidad ✅
   
4. Test RESET: p-value = 0.12
   → No rechazamos H0 → Forma funcional correcta ✅
   
CONCLUSIÓN: Modelo robusto y confiable, a pesar de leve 
            desviación de normalidad (compensada por n grande)
```

---

## 🔍 COMPARACIÓN CON PRÁCTICO DEL PROFESOR

El práctico del profesor (Tema 7) incluye:

✅ Test de Jarque-Bera para normalidad
✅ Test RESET de Ramsey
✅ VIF para multicolinealidad
✅ Análisis visual de residuos

**Nuestro código implementa TODO esto + Breusch-Pagan**

Diferencias:
- Profesor: Test informal de heterocedasticidad visual
- Nosotros: Test formal de Breusch-Pagan + visual

Ambos enfoques son válidos y complementarios.

---

## 📚 REFERENCIAS

- Tema 7, sección "Análisis de los residuos" (línea 225)
- Tema 7, sección "Contraste RESET de Ramsey" (línea 236)
- Wooldridge (2015), Capítulo 8: Heteroskedasticity
- Wooldridge (2015), Capítulo 3: Multiple Regression Analysis - Estimation
