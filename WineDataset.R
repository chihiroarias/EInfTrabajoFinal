# ============================================================
# EDA completo para winequality (UCU)
# Basado en las ideas del práctico WDI, adaptado a este dataset
# ============================================================

# ------------------------------
# 0) Opciones y reproducibilidad
# ------------------------------
set.seed(1234)
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# ------------------------------
# 1) Paquetes
# ------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(readr)
library(forcats)
library(scales)
library(lubridate)
library(moments)
theme_set(theme_minimal(base_size = 12))

# ------------------------------
# 2) Lectura del dataset
# ------------------------------

# Dataset cargado por vos
wine_red <- read_csv("/mnt/data/winequality-red.csv", show_col_types = FALSE)

# Si luego tenés winequality-white.csv, haremos esto:
# wine_white <- read_csv("/mnt/data/winequality-white.csv", show_col_types = FALSE)

# Para este ejemplo, supongamos que solo tenés el rojo:
wine_red <- wine_red %>% mutate(tipo_vino = "tinto")

# Si tuvieras ambos, la unión sería:
# wine_white <- wine_white %>% mutate(tipo_vino = "blanco")
# wine <- bind_rows(wine_red, wine_white)

wine <- wine_red %>% clean_names()

# ------------------------------
# 3) Estructura y primeras miradas
# ------------------------------
glimpse(wine)
skim(wine)

# Convertir tipo_vino en factor ordenado
wine <- wine %>% mutate(tipo_vino = factor(tipo_vino))

# ------------------------------
# 4) Tabla de frecuencia para quality
# ------------------------------

tabla_frec <- wine %>%
  count(quality, name = "f_j") %>%
  mutate(
    F_j = cumsum(f_j),
    h_j = f_j / sum(f_j),
    H_j = cumsum(h_j)
  )

print(tabla_frec)
knitr::kable(tabla_frec, digits = 3)

# Gráfico PMF
ggplot(tabla_frec, aes(x = factor(quality), y = h_j)) +
  geom_segment(aes(xend = factor(quality), yend = 0), linewidth = 1) +
  labs(title = "Función de cuantía de quality", x = "Calidad", y = "h_j")

# ------------------------------
# 5) Estadísticos descriptivos
# ------------------------------

desc <- wine %>%
  summarise(
    n = n(),
    minimo = min(quality),
    maximo = max(quality),
    moda = quality %>% table() %>% which.max() %>% names() %>% as.numeric(),
    media = mean(quality),
    mediana = median(quality),
    q1 = quantile(quality, 0.25),
    q3 = quantile(quality, 0.75),
    rango = max(quality) - min(quality),
    IQR = IQR(quality),
    varianza = var(quality),
    sd = sd(quality),
    cv = sd(quality) / mean(quality),
    asimetria = skewness(quality),
    curtosis = kurtosis(quality)
  ) %>%
  pivot_longer(everything(), names_to = "Medida", values_to = "Valor")

knitr::kable(desc, digits = 3)

# ------------------------------
# 6) Histogramas y densidades
# ------------------------------

ggplot(wine, aes(x = quality)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(title = "Histograma de calidad", x = "Quality", y = "Frecuencia")

ggplot(wine, aes(x = quality)) +
  geom_density(fill = "gray80") +
  labs(title = "Densidad suavizada de calidad", x = "Quality")

# ------------------------------
# 7) Boxplot por tipo de vino
# ------------------------------

ggplot(wine, aes(x = tipo_vino, y = quality, fill = tipo_vino)) +
  geom_boxplot(alpha = 0.7) +
  theme(legend.position = "none") +
  labs(title = "Distribución de calidad por tipo de vino",
       x = "Tipo", y = "Quality")

# ------------------------------
# 8) Correlaciones y scatterplots
# ------------------------------

# Correlación alcohol – quality
cor(wine$alcohol, wine$quality, use = "complete.obs")

ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Alcohol vs Calidad", x = "Alcohol", y = "Quality")

# Facetas por tipo de vino (si hubiera dos)
ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ tipo_vino) +
  labs(title = "Alcohol vs Calidad por tipo")

# ------------------------------
# 9) Frecuencias para variable categórica nueva
# ------------------------------

wine %>% count(tipo_vino) %>% knitr::kable()

ggplot(wine %>% count(tipo_vino),
       aes(x = tipo_vino, y = n, fill = tipo_vino)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "Cantidad de observaciones por tipo de vino")

# ------------------------------
# 10) Exportaciones
# ------------------------------

dir.create("output", showWarnings = FALSE)

write_csv(tabla_frec, "output/tabla_frecuencias_quality.csv")

ggsave("output/histograma_quality.png", width = 7, height = 5, dpi = 300)

# ------------------------------
# 11) Guardar dataset limpio
# ------------------------------

saveRDS(wine, file = "output/wine_limpio.rds")

sessionInfo()


################################################################################
# Estadística Inferencial - Curso 2025 - UCU
#
# Práctico 2 – Muestreo con población conocida (WINEQUALITY)
#
# Dataset: /mnt/data/winequality-red.csv + columna tipo_vino
################################################################################

##------------------------------------------------------------------------------
## 0) PAQUETES Y CONFIGURACIÓN -------------------------------------------------
##------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(ggplot2)
library(scales)
set.seed(2145)
theme_set(theme_minimal(base_size = 12))


##------------------------------------------------------------------------------
## 1) CARGA DE LA “POBLACIÓN” ---------------------------------------------------
##------------------------------------------------------------------------------

# Cargamos el dataset del usuario (solo vino tinto)
wine_red <- read_csv("/mnt/data/winequality-red.csv", show_col_types = FALSE)

# Añadimos la columna de tipo (si luego cargás el blanco, simplemente bind_rows)
wine <- wine_red %>% 
  mutate(tipo_vino = "tinto") %>% 
  clean_names()

# VARIABLE BINARIA EQUIVALENTE A “internet”:
# Consideremos “vino bueno” si quality >= 7
wine <- wine %>%
  mutate(
    vino_bueno = as.integer(quality >= 7)
  )

# VARIABLE CONTINUA EQUIVALENTE A “ppr”:
wine <- wine %>%
  mutate(acidez_por_alcohol = fixed_acidity / alcohol)

# Estrato = tipo de vino (factor)
wine <- wine %>%
  mutate(tipo_vino = factor(tipo_vino))

# Para simular conglomerados:
wine <- wine %>%
  mutate(cluster = factor(ntile(quality, 3)))  # 3 conglomerados artificiales

# Esta es toda la población:
pobl <- wine
Npop <- nrow(pobl)


##------------------------------------------------------------------------------
## 2) PARÁMETROS POBLACIONALES VERDADEROS ---------------------------------------
##------------------------------------------------------------------------------

p_verdadera  <- mean(pobl$vino_bueno)
mu_verdadera <- mean(pobl$acidez_por_alcohol)
sd_ap        <- sd(pobl$acidez_por_alcohol)

cat("Parámetros verdaderos:\n")
cat("  p(vino bueno) =", percent(p_verdadera, 0.1), "\n")
cat("  μ(acidez/alcohol) =", round(mu_verdadera, 3), "\n\n")


##------------------------------------------------------------------------------
## 3) MUESTREO 1 – SRS ----------------------------------------------------------
##------------------------------------------------------------------------------

n <- 300   # Tamaño de muestra (modificar)

m_srs <- pobl %>%
  slice_sample(n = n, replace = FALSE) %>%
  mutate(fpc = Npop)

d_srs <- svydesign(ids = ~1, fpc = ~fpc, data = m_srs)

est_p_srs  <- svymean(~vino_bueno, d_srs)
ci_p_srs   <- confint(est_p_srs)

est_mu_srs <- svymean(~acidez_por_alcohol, d_srs)
ci_mu_srs  <- confint(est_mu_srs)


##------------------------------------------------------------------------------
## 4) MUESTREO 2 – ESTRATIFICADO PROPORCIONAL -----------------------------------
##------------------------------------------------------------------------------

Nh_tab <- pobl %>% count(tipo_vino, name = "Nh")
nh_tab <- Nh_tab %>% mutate(nh = round(n * Nh / sum(Nh)))

# División en estratos
grupos <- pobl %>% arrange(tipo_vino) %>% group_split(tipo_vino)
nh_ord <- nh_tab %>% arrange(tipo_vino)

m_str <- purrr::map2_dfr(
  grupos, nh_ord$nh,
  ~ slice_sample(.x, n = pmin(.y, nrow(.x)))
) %>%
  left_join(Nh_tab, by = "tipo_vino")

d_str <- svydesign(ids = ~1, strata = ~tipo_vino, fpc = ~Nh, data = m_str)

est_p_str  <- svymean(~vino_bueno, d_str)
ci_p_str   <- confint(est_p_str)

est_mu_str <- svymean(~acidez_por_alcohol, d_str)
ci_mu_str  <- confint(est_mu_str)


##------------------------------------------------------------------------------
## 5) ESTRATIFICADO NEYMAN ------------------------------------------------------
##------------------------------------------------------------------------------

nh_tab_n <- pobl %>%
  group_by(tipo_vino) %>%
  summarise(
    Nh = n(),
    Sh = sd(acidez_por_alcohol)
  ) %>%
  mutate(nh = pmax(2, round(n * Nh * Sh / sum(Nh * Sh))))

nh_tab_n_ord <- nh_tab_n %>% arrange(tipo_vino)
grupos_n <- pobl %>% arrange(tipo_vino) %>% group_split(tipo_vino)

m_str_n <- purrr::map2_dfr(
  grupos_n, nh_tab_n_ord$nh,
  ~ slice_sample(.x, n = pmin(.y, nrow(.x)))
) %>%
  left_join(nh_tab_n %>% select(tipo_vino, Nh), by = "tipo_vino")

d_str_n <- svydesign(ids = ~1, strata = ~tipo_vino, fpc = ~Nh, data = m_str_n)

est_p_str_n  <- svymean(~vino_bueno, d_str_n);  ci_p_str_n  <- confint(est_p_str_n)
est_mu_str_n <- svymean(~acidez_por_alcohol, d_str_n);  ci_mu_str_n <- confint(est_mu_str_n)


##------------------------------------------------------------------------------
## 6) MUESTREO 3 – CONGLOMERADOS -----------------------------------------------
##------------------------------------------------------------------------------

m <- 2   # número de conglomerados a seleccionar

clu_sel <- pobl %>%
  distinct(cluster) %>%
  slice_sample(n = m) %>%
  pull(cluster)

m_clu <- pobl %>% filter(cluster %in% clu_sel)

d_clu <- svydesign(ids = ~cluster, data = m_clu)

est_p_clu  <- svymean(~vino_bueno, d_clu);  ci_p_clu  <- confint(est_p_clu)
est_mu_clu <- svymean(~acidez_por_alcohol, d_clu);  ci_mu_clu <- confint(est_mu_clu)


##------------------------------------------------------------------------------
## 7) COMPARACIÓN ENTRE DISEÑOS -------------------------------------------------
##------------------------------------------------------------------------------

res <- tibble(
  diseno = c("SRS",
             "Estratificado Prop.",
             "Estratificado Neyman",
             "Conglomerados"),
  p_hat  = c(coef(est_p_srs), coef(est_p_str), coef(est_p_str_n), coef(est_p_clu)),
  se_p   = c(SE(est_p_srs),   SE(est_p_str),   SE(est_p_str_n),   SE(est_p_clu)),
  lo_p   = c(ci_p_srs[1], ci_p_str[1], ci_p_str_n[1], ci_p_clu[1]),
  hi_p   = c(ci_p_srs[2], ci_p_str[2], ci_p_str_n[2], ci_p_clu[2]),
  mu_hat = c(coef(est_mu_srs), coef(est_mu_str), coef(est_mu_str_n), coef(est_mu_clu)),
  se_mu  = c(SE(est_mu_srs),   SE(est_mu_str),   SE(est_mu_str_n),   SE(est_mu_clu)),
  lo_mu  = c(ci_mu_srs[1], ci_mu_str[1], ci_mu_str_n[1], ci_mu_clu[1]),
  hi_mu  = c(ci_mu_srs[2], ci_mu_str[2], ci_mu_str_n[2], ci_mu_clu[2])
)

print(res)

cat("\nParámetros verdaderos:",
    "\np =", percent(p_verdadera, 0.1),
    "\nμ =", round(mu_verdadera, 3), "\n\n")

# Gráficos de comparación (proporción)
ggplot(res, aes(x = diseno, y = p_hat)) +
  geom_hline(yintercept = p_verdadera, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo_p, ymax = hi_p)) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Estimación de p(vino bueno) por diseño")

# Gráficos de comparación (media)
ggplot(res, aes(x = diseno, y = mu_hat)) +
  geom_hline(yintercept = mu_verdadera, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo_mu, ymax = hi_mu)) +
  coord_flip() +
  labs(title = "Estimación de μ(acidez/alcohol) por diseño")


##------------------------------------------------------------------------------
## 8) TAMAÑO DE MUESTRA ---------------------------------------------------------
##------------------------------------------------------------------------------

z_alpha <- function(conf = 0.95) qnorm(1 - (1-conf)/2)

n_media <- function(E, sd, conf = 0.95, deff = 1, N = Inf){
  n0 <- (z_alpha(conf)*sd/E)^2 * deff
  if(is.finite(N)) (n0*N)/(n0 + N - 1) else n0
}

n_prop <- function(E, p = 0.5, conf = 0.95, deff = 1, N = Inf){
  n0 <- (z_alpha(conf)^2 * p*(1-p)) / (E^2) * deff
  if(is.finite(N)) (n0*N)/(n0 + N - 1) else n0
}

cat("Ejemplo n para MEDIA:",  ceiling(n_media(0.01, sd_ap, conf=0.95, deff=1.2, N=Npop)), "\n")
cat("Ejemplo n para PROPORCIÓN:", ceiling(n_prop(0.02, p_verdadera, conf=0.95, deff=1.2, N=Npop)), "\n\n")


##------------------------------------------------------------------------------
## 9) TCL – SIMULACIÓN ----------------------------------------------------------
##------------------------------------------------------------------------------

B  <- 1500
ns <- c(20, 80, 300)

# TCL para la media
sim_mu <- tibble()
for(n_i in ns){
  medias <- replicate(B, {
    s <- pobl %>% slice_sample(n = n_i)
    mean(s$acidez_por_alcohol)
  })
  sim_mu <- bind_rows(sim_mu, tibble(xbar = medias, n = n_i))
}

ggplot(sim_mu, aes(x = xbar)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35) +
  facet_wrap(~n, scales = "free") +
  labs(title = "TCL para la media (acidez/alcohol)")

# TCL para proporción
sim_p <- tibble()
for(n_i in ns){
  props <- replicate(B, {
    s <- pobl %>% slice_sample(n = n_i)
    mean(s$vino_bueno)
  })
  sim_p <- bind_rows(sim_p, tibble(phat = props, n = n_i))
}

ggplot(sim_p, aes(x = phat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35) +
  facet_wrap(~n, scales = "free") +
  scale_x_continuous(labels = percent) +
  labs(title = "TCL para la proporción p(vino bueno)")


################################################################################
# PRÁCTICO R — Temas 3, 4 y 5 
# Dataset: winequality (vinos) con columna tipo_vino (tinto/blanco)
################################################################################

# Dataset winequality:
# --------------------
# Conjunto real de vinos con mediciones físico-químicas y una evaluación de
# calidad sensorial.
#
# Suposición de trabajo:
# - tipo_vino: factor que identifica si el vino es "tinto" o "blanco"
# - quality: puntaje de calidad (numérico)
# - alcohol: grado alcohólico (%)
# - fixed_acidity, volatile_acidity: acidez fija y volátil
#
# En este práctico filtramos casos con NA en variables clave y usamos:
#  1) alcohol (media/mediana, IC, QQ plot, por tipo_vino),
#  2) alto_alcohol (indicadora binaria) para proporciones,
#  3) tests de medias y proporciones (t de Welch, proporciones 2x2).

#------------------------------------------------------------------------------
# Preámbulo
#------------------------------------------------------------------------------

set.seed(2145)
options(digits = 4)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(broom)
  library(readr)
})

say <- function(...) cat("\n", paste0(...), "\n")

#------------------------------------------------------------------------------
# Datos
#------------------------------------------------------------------------------

say("=== Cargando y preparando datos: winequality ===")

# Ruta al archivo (ajustar si hace falta)
dat0 <- read_csv("/mnt/data/winequality-red.csv", show_col_types = FALSE)

# Si la columna tipo_vino NO existe, asumimos que este archivo es sólo tinto
if (!"tipo_vino" %in% names(dat0)) {
  dat0 <- dat0 |>
    mutate(tipo_vino = "tinto")
}

# Preparamos dataset:
# - nos quedamos con variables clave
# - creamos alto_alcohol como Bernoulli (1 si alcohol >= 11)
dat <- dat0 |>
  dplyr::select(tipo_vino, alcohol, fixed_acidity, volatile_acidity, quality) |>
  dplyr::filter(!is.na(tipo_vino),
                !is.na(alcohol),
                !is.na(quality)) |>
  mutate(
    tipo_vino    = factor(tipo_vino),
    alto_alcohol = as.integer(alcohol >= 11)
  ) |>
  droplevels()

summary(dat)

#------------------------------------------------------------------------------
# A) Tema 3: ESTIMACIÓN
#------------------------------------------------------------------------------

# Usamos alcohol como variable cuantitativa principal
x    <- dat$alcohol
xbar <- mean(x)
xmed <- median(x)
n    <- length(x)

## A1. Media muestral como minimizadora de SCE (constante)

say("\n=== A1. OLS de una constante: alcohol ===")
SCE <- function(mu) sum((x - mu)^2)
opt <- optimize(SCE, interval = range(x))
mu_hat_optim <- opt$minimum
say("mu_hat (óptimo) = ", round(mu_hat_optim, 6), " | media = ", round(xbar, 6))


## A2. Propiedad: media de los residuos es ~ 0

say("Media de residuos ~ 0 => ", round(mean(x - xbar), 10))


## A3. La mediana minimiza la suma de desviaciones absolutas

sabs <- function(mu) sum(abs(x - mu))
opt_L1 <- optimize(sabs, interval = range(x))
mu_hat_L1_optim <- opt_L1$minimum
c(mu_hat_L1_optim = mu_hat_L1_optim, median = xmed)


## A4. Bernoulli: p = P(alto_alcohol = 1) en vinos tintos (tipo_vino == 'tinto')

subT <- subset(dat, tipo_vino == "tinto")
yT   <- subT$alto_alcohol
sumxT <- sum(yT == 1)
nT    <- length(yT)

p_MM <- sumxT / nT     # Método de Momentos
p_ML <- sumxT / nT     # Máxima Verosimilitud (coinciden en Bernoulli)

# Optimización de la log-verosimilitud
loglik <- function(p) {
  if (p <= 0 || p >= 1) return(-Inf)
  sumxT * log(p) + (nT - sumxT) * log(1 - p)
}
opt <- optimize(function(p) -loglik(p), interval = c(1e-9, 1 - 1e-9))
p_opt <- opt$minimum

c(p_MM = p_MM, p_ML = p_ML, p_opt_numerico = p_opt)


#------------------------------------------------------------------------------
# B) Tema 4: INTERVALOS DE CONFIANZA
#------------------------------------------------------------------------------

alpha <- 0.05

## B1. IC 95% para la media de alcohol por tipo de vino

say("\n=== B1. IC 95% para la media de alcohol por tipo_vino ===")
ic_t_by <- dat |>
  dplyr::group_by(tipo_vino) |>
  dplyr::summarise(
    n     = dplyr::n(),
    media = mean(alcohol),
    sd    = sd(alcohol),
    se    = sd / sqrt(n),
    tcrit = qt(1 - alpha/2, df = n - 1),
    L     = media - tcrit * se,
    U     = media + tcrit * se,
    .groups = "drop"
  ) |>
  dplyr::select(tipo_vino, n, media, L, U)

print(dplyr::mutate(ic_t_by, dplyr::across(where(is.numeric), ~ round(., 3))))


## B2. IC 95% para la proporción de alto_alcohol en tintos (Wilson + exacto)

say("\n=== B2. IC 95% para proporción de alto_alcohol en tintos (Wilson y exacto) ===")
pT <- p_MM
z  <- qnorm(1 - alpha/2)

# Wilson (score)
den   <- 1 + z^2 / nT
cent  <- (pT + z^2 / (2 * nT)) / den
half  <- (z / den) * sqrt((pT * (1 - pT) / nT) + (z^2 / (4 * nT^2)))
wilL  <- cent - half
wilU  <- cent + half

# Clopper–Pearson (exacto)
cp    <- binom.test(sumxT, nT, conf.level = 1 - alpha)$conf.int

say("p̂_tintos = " , round(pT, 4),
    " | Wilson 95%: [", round(wilL, 4), ", ", round(wilU, 4), "]",
    " | Exacto 95%: [", round(cp[1], 4),  ", ", round(cp[2], 4), "]")


## B3. Bootstrap percentil 95% para p = P(alto_alcohol = 1) en tintos

say("\n=== B3. Bootstrap percentil (B=5000) para p = P(alto_alcohol=1) en tintos ===")
B <- 5000
boot_p <- replicate(B, mean(sample(yT, size = nT, replace = TRUE)))
ic_b   <- quantile(boot_p, c(alpha/2, 1 - alpha/2))
say("p̂_tintos = ", round(pT, 4),
    " | IC bootstrap 95%: [", round(ic_b[1], 4), ", ", round(ic_b[2], 4), "]")


## B4. QQ plot (diagnóstico visual) para alcohol

say("\n=== B4. QQ plot de alcohol (global y por tipo_vino) — se abren gráficos ===")
try({
  # Global
  p1 <- ggplot(dat, aes(sample = alcohol)) +
    stat_qq() + stat_qq_line() +
    labs(title = "QQ plot global: alcohol",
         x = "Cuantiles teóricos", y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 11)
  print(p1)
  
  # Por tipo de vino
  p2 <- ggplot(dat, aes(sample = alcohol)) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ tipo_vino, scales = "free") +
    labs(title = "QQ plot por tipo_vino: alcohol",
         x = "Cuantiles teóricos", y = "Cuantiles de la muestra") +
    theme_minimal(base_size = 11)
  print(p2)
}, silent = TRUE)


#------------------------------------------------------------------------------
# C) Tema 5: PRUEBAS DE HIPÓTESIS
#------------------------------------------------------------------------------

## C1. Una media (t de 1 muestra): alcohol en tintos vs mu0
# H0: mu = mu0
# H1: mu != mu0

say("\n=== C1. t-test 1 muestra: alcohol en tintos vs mu0 ===")
mu0 <- 10.5  # probar otros valores

print(
  subT |>
    dplyr::summarise(
      n = dplyr::n(),
      media = mean(alcohol),
      sd = sd(alcohol)
    )
)
print(t.test(subT$alcohol, mu = mu0, alternative = "two.sided"))


## C2. Una proporción (una cola): p = P(alto_alcohol=1) en tintos vs p0
# H0: p = p0      
# H1: p > p0

say("\n=== C2. Test de proporción 1 muestra (una cola): P(alto_alcohol=1 en tintos) > 0.5 ===")
p0 <- 0.5
say(paste0("sum(x) = ", sumxT, ", n = ", nT, ", p̂ = ", round(mean(yT), 4)))

say("- prop.test (aprox z con corrección de continuidad, alternativa: greater):")
print(prop.test(sumxT, nT, p = p0, alternative = "greater", correct = TRUE))

say("- binom.test (exacto Clopper-Pearson, alternativa: greater):")
print(binom.test(sumxT, nT, p = p0, alternative = "greater"))


## C3. t-test de Welch: alcohol ~ tipo_vino
say("\n=== C3. Welch t-test: alcohol ~ tipo_vino ===")
print(
  dat |>
    dplyr::group_by(tipo_vino) |>
    dplyr::summarise(
      n = dplyr::n(),
      media = mean(alcohol),
      sd = sd(alcohol),
      .groups = "drop"
    )
)
print(t.test(alcohol ~ tipo_vino, data = dat, var.equal = FALSE))


## C4. ANOVA 1 factor: alcohol ~ tipo_vino
say("\n=== C4. ANOVA: alcohol ~ tipo_vino ===")
fit_aov <- aov(alcohol ~ tipo_vino, data = dat)
print(summary(fit_aov))
say("Tukey HSD (si ANOVA rechaza):")
print(TukeyHSD(fit_aov))


## C5. 2x2 proporciones: ¿difiere la proporción de alto_alcohol entre tintos y blancos?
say("\n=== C5. Proporciones 2x2: alto_alcohol entre tintos y blancos ===")

# Nos quedamos con los dos tipos si existen ambos niveles
subTB <- subset(dat, tipo_vino %in% c("tinto", "blanco"))
subTB <- droplevels(subTB)

subTB <- subTB |>
  mutate(
    alto_factor = factor(
      ifelse(alto_alcohol == 1, "alto", "no_alto"),
      levels = c("no_alto", "alto")
    )
  )

tab <- table(tipo_vino = subTB$tipo_vino, alto_alcohol = subTB$alto_factor)
print(tab)

say("- fisher.test (exacto):")
print(fisher.test(tab))

# Efecto simple: diferencia de proporciones (RD) y OR
p_tinto  <- tab["tinto",  "alto"] / sum(tab["tinto",  ])
p_blanco <- if ("blanco" %in% rownames(tab)) tab["blanco", "alto"] / sum(tab["blanco", ]) else NA

RD <- p_tinto - p_blanco

tab_cc <- tab
if (any(tab_cc == 0)) tab_cc <- tab_cc + 0.5
if (all(c("tinto","blanco") %in% rownames(tab_cc))) {
  OR <- (tab_cc["tinto","alto"]  / tab_cc["tinto","no_alto"]) /
    (tab_cc["blanco","alto"] / tab_cc["blanco","no_alto"])
} else {
  OR <- NA
}

say("RD (p_tinto - p_blanco) = ", round(RD, 4), " | OR ≈ ", round(OR, 4))




################################################################################
# PRÁCTICO — Tema 6 (MRLS)  
# Dataset: winequality + tipo_vino (tinto/blanco)
################################################################################

library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
library(xtable)
library(performance)

#-----------------------------------------------------------------------
# 1) CARGA DE DATOS
#-----------------------------------------------------------------------

wine <- read_csv("/mnt/data/winequality-red.csv", show_col_types = FALSE)

# Si no existe columna "tipo_vino" la creamos (dataset rojo)
if(!"tipo_vino" %in% names(wine)){
  wine <- wine |> mutate(tipo_vino = "tinto")
}

# Creamos pesos artificiales (para replicar estructura del práctico)
wine <- wine |> mutate(peso = 1)

# Variables renombradas siguiendo la lógica del práctico:
# Y = quality
# X = alcohol
# Controles opcionales: fixed_acidity, volatile_acidity, sulphates

datos <- wine |>
  select(quality, alcohol, fixed_acidity, volatile_acidity, sulphates,
         tipo_vino, peso)

#-----------------------------------------------------------------------
# 2) DESCRIPTIVA BÁSICA
#-----------------------------------------------------------------------

dim(datos)
head(datos)
summary(datos$quality)

## Hist y boxplot de quality
hist(datos$quality,
     main="Histograma de la calidad del vino",
     xlab="Quality score",
     ylab="Frecuencia",
     col="orange")

boxplot(datos$quality, main="Boxplot quality")

var(datos)

## Tabla descriptiva básica
vars_desc <- c("quality","alcohol","fixed_acidity","volatile_acidity","sulphates")

tabla <- datos |>
  select(all_of(vars_desc), peso) |>
  pivot_longer(all_of(vars_desc), names_to="Variable", values_to="Valor") |>
  group_by(Variable) |>
  summarise(
    Media = round(weighted.mean(Valor, peso, na.rm=TRUE), 3),
    Min   = round(min(Valor, na.rm=TRUE), 3),
    Max   = round(max(Valor, na.rm=TRUE), 3),
    .groups="drop"
  ) |>
  arrange(Variable)

print(tabla, n = Inf)


#-----------------------------------------------------------------------
# MODELO 1: MRLS SIMPLE quality ~ alcohol
#-----------------------------------------------------------------------

# Eliminamos un atípico extremo de alcohol > 15.5
datos <- datos |> filter(alcohol <= 15.5)

#-------------------------------------------------
# Dispersión + recta MCO
#-------------------------------------------------

ggplot(datos, aes(x = alcohol, y = quality)) +
  geom_point(alpha=.45) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Calidad vs Alcohol",
       x="Alcohol (%)", y="Quality score") +
  theme_light()

#-------------------------------------------------
# Estimación MCO simple
#-------------------------------------------------

M1 <- lm(quality ~ alcohol, data = datos)
summary(M1)
Modelo1_resumen <- summary(M1)

datos <- datos |>
  mutate(pred1 = predict(M1),
         resid1 = resid(M1))

# Visual ajuste + residuos
ggplot(datos, aes(x = alcohol, y = quality)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend = alcohol, yend = pred1),
               col="red", lty="dashed", linewidth=.3, na.rm=TRUE) +
  geom_point(alpha=.45) +
  geom_point(aes(y=pred1), col="red", size=1, na.rm=TRUE) +
  labs(title="Ajuste lineal y residuos",
       x="Alcohol (%)",
       y="Quality") +
  theme_light()

# Métricas
M1_LL    <- as.numeric(logLik(M1))
M1_R2    <- summary(M1)$r.squared
M1_sigma <- summary(M1)$sigma
M1_SCR   <- deviance(M1)
M1_SCT   <- M1_SCR / (1 - M1_R2)
M1_ahat  <- coef(M1)[1]
M1_bhat  <- coef(M1)[2]
M1_MSE   <- mean(datos$resid1^2)
M1_MAE   <- mean(abs(datos$resid1))
M1_RMSE  <- sqrt(M1_MSE)

#-------------------------------------------------
# “Modelo ponderado” (pero peso = 1 siempre)
#-------------------------------------------------

M1w <- lm(quality ~ alcohol, data = datos, weights = peso)
summary(M1w)

datos <- datos |> mutate(
  pred1w = predict(M1w),
  resid1w = resid(M1w)
)

# Métricas ponderadas
M1w_LL    <- as.numeric(logLik(M1w))
M1w_R2    <- summary(M1w)$r.squared
M1w_sigma <- summary(M1w)$sigma
M1w_SCR   <- deviance(M1w)
M1w_SCT   <- M1w_SCR / (1 - M1w_R2)
M1w_ahat  <- coef(M1w)[1]
M1w_bhat  <- coef(M1w)[2]
M1w_MSE   <- mean(datos$resid1w^2)
M1w_MAE   <- mean(abs(datos$resid1w))
M1w_RMSE  <- sqrt(M1w_MSE)

resumen_modelos <- tibble::tibble(
  modelo   = c("M1 (sin pesos)", "M1w (con pesos artificiales)"),
  intercep = c(M1_ahat, M1w_ahat),
  beta_x   = c(M1_bhat, M1w_bhat),
  R2       = c(M1_R2, M1w_R2),
  sigma    = c(round(M1_sigma,3), round(M1w_sigma,3)),
  SCR      = c(round(M1_SCR,1),   round(M1w_SCR,1)),
  SCT      = c(round(M1_SCT,1),   round(M1w_SCT,1)),
  RMSE     = c(round(M1_RMSE,1),  round(M1w_RMSE,1)),
  MAE      = c(round(M1_MAE,1),   round(M1w_MAE,1))
)
print(resumen_modelos, n = Inf)


# Propiedades numéricas
sum(resid(M1))
sum(resid(M1) * model.matrix(M1)[,"alcohol"])

Sy2  <- var(datos$quality)
Sx2  <- var(datos$alcohol)
Sxy  <- cov(datos$quality, datos$alcohol)
r_xy <- cor(datos$quality, datos$alcohol)

b_cov_var <- Sxy / Sx2
b_r_sy_sx <- r_xy * sqrt(Sy2 / Sx2)

c(beta_lm = coef(M1)[2],
  b_cov_var = b_cov_var,
  b_r_sy_sx = b_r_sy_sx)

#-------------------------------------------------
# Intervalos de confianza
#-------------------------------------------------

cat("\nIC 95% de coeficientes (M1):\n")
print(round(confint(M1, level=0.95),4))

# IC condicional en puntos seleccionados
qx <- quantile(datos$alcohol, probs=c(.25,.50,.75))
newX <- tibble(alcohol = as.numeric(qx),
               punto   = c("P25","P50","P75"))

IC_M1 <- bind_cols(newX |> select(punto, alcohol),
                   as_tibble(predict(M1, newX, interval="confidence", level=0.95)))

cat("\nIC 95% para la media condicional (M1):\n")
print(IC_M1 |> rename(fit_media=fit, lwr_media=lwr, upr_media=upr))

PI_M1 <- bind_cols(newX |> select(punto, alcohol),
                   as_tibble(predict(M1, newX, interval="prediction", level=0.95)))

cat("\nIP 95% para observación individual (M1):\n")
print(PI_M1 |> rename(fit_obs=fit, lwr_obs=lwr, upr_obs=upr))


#-----------------------------------------------------------------------
# Gráficos de diagnóstico
#-----------------------------------------------------------------------

par(mfrow=c(2,2))
plot(M1)
par(mfrow=c(1,1))


#////////////////////////////////////////////////////////////////////
# MODELO 2: MRLS EN LOGS ----
#--------------------------------------------------------------------

datos <- datos |>
  mutate(
    lquality = if_else(quality > 0, log(quality), NA_real_),
    lalcohol = if_else(alcohol > 0, log(alcohol), NA_real_)
  )

ggplot(datos, aes(x=lalcohol, y=lquality)) +
  geom_point() +
  ggtitle("Diagrama de dispersión (logs)") +
  xlab("log(alcohol)") +
  ylab("log(quality)") +
  geom_smooth(method=lm, se=FALSE)

M2 <- lm(lquality ~ lalcohol, data = datos)
summary(M2)

# Errores robustos HC1
coeftest(M2, vcov = vcovHC(M2, type="HC1"))

M2_LL <- logLik(M2)
M2_R2 <- summary(M2)$r.squared



library(readr)
library(car)        ## Para pruebas de significación conjunta
library(gmodels)    ## Para restricciones lineales
library(tidyverse)
library(haven)
library(xtable)
library(jtools)
library(stargazer)
library(fBasics)
library(lmtest)
library(caret)
library(ivreg)
library(aod)        ## para contrastes asintóticos
library(mfx)
library(memisc)     ## para comparar modelos binarios
library(performance)
library(broom)

#' ============================================================================
#' Práctico – Tema 7 (MRLM) adaptado a winequality
#'   - Y (alquiler)        -> quality
#'   - ingresos_zona       -> alcohol
#'   - banos               -> fixed_acidity
#'   - alcobas             -> volatile_acidity
#'   - resto: dummies derivados de variables físico-químicas
#' ============================================================================

#--------------------------------------------------------------------
# CARGA Y CONSTRUCCIÓN DE VARIABLES
#--------------------------------------------------------------------

datos0 <- read_csv("/mnt/data/winequality-red.csv", show_col_types = FALSE)

# Si no existe tipo_vino, asumimos que este archivo es solo tinto
if (!"tipo_vino" %in% names(datos0)) {
  datos0 <- datos0 |> mutate(tipo_vino = "tinto")
}

# Creamos pesos "de encuesta" artificiales
datos <- datos0 |>
  mutate(
    bc_pesoan = 1,
    # Mapeo a la notación del práctico
    alquiler       = quality,          # Y
    ingresos_zona  = alcohol,          # X principal
    banos          = fixed_acidity,
    alcobas        = volatile_acidity,
    # Dummies (0/1) basadas en cuantiles/condiciones de las variables
    casa           = as.integer(residual_sugar >
                                  median(residual_sugar, na.rm = TRUE)),
    derrumbe       = as.integer(chlorides >
                                  median(chlorides, na.rm = TRUE)),
    luz_solar      = as.integer(free_sulfur_dioxide >
                                  median(free_sulfur_dioxide, na.rm = TRUE)),
    grietas        = as.integer(total_sulfur_dioxide >
                                  median(total_sulfur_dioxide, na.rm = TRUE)),
    asentamiento   = as.integer(density >
                                  median(density, na.rm = TRUE)),
    humedades      = as.integer(pH <
                                  median(pH, na.rm = TRUE)),
    goteras        = as.integer(sulphates >
                                  median(sulphates, na.rm = TRUE)),
    ventilacion    = as.integer(alcohol <
                                  median(alcohol, na.rm = TRUE))
  ) |>
  # Para secciones en logs y cuadrados:
  mutate(
    sq_ingresos_zona = ingresos_zona^2,
    lalquiler        = if_else(alquiler > 0, log(alquiler), NA_real_),
    lingresos_zona   = if_else(ingresos_zona > 0, log(ingresos_zona), NA_real_)
  )

#--------------------------------------------------------------------
# MODELO 3: MRLM (múltiple lineal)
#--------------------------------------------------------------------

Modelo3 <- lm(
  alquiler ~ ingresos_zona + banos + alcobas + casa + derrumbe +
    luz_solar + grietas + asentamiento + humedades + goteras + ventilacion,
  data = datos, x = TRUE, y = TRUE
)
summary(Modelo3)
Modelo3_resumen <- summary(Modelo3)

## Guardamos algunos resultados
M3_LL    <- logLik(Modelo3)
M3_R2    <- summary(Modelo3)$r.squared
M3_sigma <- summary(Modelo3)$sigma
M3_SCR   <- summary(Modelo3)$df[2] * M3_sigma^2
M3_SCT   <- M3_SCR / (1 - M3_R2)

## Predicción en una “nueva observación” hipotética
new_obs <- data.frame(
  ingresos_zona = 12,          # alcohol aprox 12%
  banos         = 7,           # fixed_acidity ~ 7
  alcobas       = 0.5,         # volatile_acidity
  casa          = 0,
  derrumbe      = 0,
  luz_solar     = 0,
  grietas       = 0,
  asentamiento  = 0,
  humedades     = 0,
  goteras       = 0,
  ventilacion   = 0
)

predict.lm(Modelo3, newdata = new_obs, interval = "confidence", level = 0.95)
predict.lm(Modelo3, newdata = new_obs, interval = "prediction", level = 0.95)

# Valor manual de la predicción
coef3 <- coef(Modelo3)
cB <- coef3["(Intercept)"] +
  coef3["ingresos_zona"] * new_obs$ingresos_zona +
  coef3["banos"]         * new_obs$banos +
  coef3["alcobas"]       * new_obs$alcobas
cB  # resto de dummies están en 0


#--------------------------------------------------------------------
# MODELO 4: MRLM con regresor al cuadrado
#--------------------------------------------------------------------

Modelo4 <- lm(
  alquiler ~ ingresos_zona + sq_ingresos_zona + banos + alcobas + casa +
    derrumbe + luz_solar + grietas + asentamiento + humedades +
    goteras + ventilacion,
  data = datos, weights = bc_pesoan, x = TRUE, y = TRUE
)

summary(Modelo4)
Modelo4_resumen <- summary(Modelo4)

## Efecto parcial de ingresos_zona en distintos niveles
b1 <- Modelo4_resumen$coefficients["ingresos_zona", "Estimate"]
b2 <- Modelo4_resumen$coefficients["sq_ingresos_zona", "Estimate"]

IZ90 <- 12    # alcohol ≈ 12
IZ55 <- 10    # alcohol ≈ 10
M4_EP_iz90 <- b1 + 2 * b2 * IZ90
M4_EP_iz55 <- b1 + 2 * b2 * IZ55

M4_EP_iz90
M4_EP_iz55

IZ <- 12
e.parcial <- b1 + 2 * b2 * IZ
e.parcial

## Valores ajustados y residuos
datos$pred4  <- predict(Modelo4)
datos$resid4 <- resid(Modelo4)

## Resultados de la estimación
M4_LL    <- logLik(Modelo4)
M4_R2    <- summary(Modelo4)$r.squared
M4_sigma <- summary(Modelo4)$sigma
M4_SCR   <- summary(Modelo4)$df[2] * M4_sigma^2
M4_SCE   <- sum((Modelo4$fitted.values - mean(datos$alquiler))^2)
M4_SCT   <- M4_SCR / (1 - M4_R2)
M4_MSE   <- mean(datos$resid4^2)
M4_MAE   <- mean(abs(datos$resid4))
M4_RMSE  <- sqrt(M4_MSE)

## Matriz (X'X)^-1
invXX4 <- vcov(Modelo4) / M4_sigma^2
invXX4

## Estimación “a mano”
datos <- datos %>% mutate(const = 1)
X <- as.matrix(datos[, c(
  "const", "ingresos_zona", "sq_ingresos_zona", "banos", "alcobas", "casa",
  "derrumbe", "luz_solar", "grietas", "asentamiento", "humedades",
  "goteras", "ventilacion"
)])
y <- as.vector(datos[, "alquiler"])
x_t   <- t(X)
xx    <- x_t %*% X
inv_xx <- solve(xx)
xy    <- x_t %*% y
beta  <- inv_xx %*% xy
beta


#--------------------------------------------------------------------
# INFERENCIA
#--------------------------------------------------------------------

## Significación individual a mano de banos
t.stat <- Modelo4_resumen$coefficients["banos", "Estimate"] /
  Modelo4_resumen$coefficients["banos", "Std. Error"]
critical.t <- qt(0.975, Modelo4$df.residual)
if (abs(t.stat) >= critical.t) {
  print("Se Rechaza H_0")
} else {
  print("No se rechaza H_0")
}

## p-valor
Modelo4_resumen$coefficients["banos", "Pr(>|t|)"]

## Intervalos
confint(Modelo4, parm = "banos", level = 0.95)
confint(Modelo4, level = 0.95)

## Significación global (F)
Modelo4_resumen$fstatistic[1]

F.stat <- (M4_SCE / (length(Modelo4$coefficients) - 1)) /
  (M4_SCR / Modelo4$df.residual)
critical.F <- qf(0.95, length(Modelo4$coefficients) - 1, Modelo4$df.residual)
if (F.stat >= critical.F) {
  print("Se rechaza H_0")
} else {
  print("No se rechaza H_0")
}

## Restricción conjunta: ingresos_zona y sq_ingresos_zona
f.conj <- linearHypothesis(
  Modelo4,
  c("ingresos_zona=0", "sq_ingresos_zona=0")
)$F[2]
v.crit.conj <- qf(0.95, 2, Modelo4$df.residual, lower.tail = FALSE)
if (abs(f.conj) >= v.crit.conj) {
  print("Se rechaza H_0")
} else {
  print("No se rechaza H_0")
}

## Modelo restringido (sin ingresos_zona ni su cuadrado)
ModeloR <- lm(
  alquiler ~ banos + alcobas + casa + derrumbe + luz_solar + grietas +
    asentamiento + humedades + goteras + ventilacion,
  data = datos, weights = bc_pesoan, x = TRUE, y = TRUE
)
summary(ModeloR)

SCR_R  <- deviance(ModeloR)
SCR_NR <- deviance(Modelo4)
f.conj2 <- ((SCR_R - SCR_NR) / 2) / (SCR_NR / Modelo4$df.residual)

R2_NR <- summary(Modelo4)$r.squared
R2_R  <- summary(ModeloR)$r.squared
f.conj3 <- ((R2_NR - R2_R) / 2) / ((1 - R2_NR) / Modelo4$df.residual)


#--------------------------------------------------------------------
# MODELO 5: MRLM con alcobas semiparamétrica
#--------------------------------------------------------------------

Modelo5 <- lm(
  alquiler ~ ingresos_zona + sq_ingresos_zona + banos + as.factor(alcobas) +
    casa + derrumbe + luz_solar + grietas + asentamiento +
    humedades + goteras + ventilacion,
  data = datos, weights = bc_pesoan, x = TRUE, y = TRUE
)
summary(Modelo5)


#--------------------------------------------------------------------
# MODELOS 6 y 7: Interacciones
#--------------------------------------------------------------------

Modelo6 <- lm(
  alquiler ~ ingresos_zona + banos + alcobas + casa + derrumbe +
    luz_solar + grietas + asentamiento + humedades + goteras +
    humedades:goteras + ventilacion,
  data = datos, weights = bc_pesoan, x = TRUE, y = TRUE
)
summary(Modelo6)

Modelo7 <- lm(
  alquiler ~ ingresos_zona + ingresos_zona:casa + banos + alcobas + casa +
    derrumbe + luz_solar + grietas + asentamiento + humedades +
    goteras + ventilacion,
  data = datos, weights = bc_pesoan, x = TRUE, y = TRUE
)
summary(Modelo7)


#--------------------------------------------------------------------
# OVR e IVI (usando Modelo1 simple y variantes)
#--------------------------------------------------------------------

# Definimos Modelo1 simple aquí (equivalente al del práctico anterior)
Modelo1 <- lm(alquiler ~ ingresos_zona, data = datos, weights = bc_pesoan)
summ(Modelo1)

# Modelo con omisión de banos
Modelo1B <- lm(alquiler ~ ingresos_zona + banos, data = datos)
summ(Modelo1B)

cor_x1.x2 <- cor(datos$ingresos_zona, datos$banos)

Modelo1aux <- lm(banos ~ ingresos_zona, data = datos)
summ(Modelo1aux)

delta  <- Modelo1aux$coefficients[2]
gamma1 <- Modelo1$coefficients[2]
beta1  <- Modelo1B$coefficients[2]
beta2  <- Modelo1B$coefficients[3]

sesgo_esperado <- beta2 * delta
diferencia     <- gamma1 - beta1

## Inclusión de variables irrelevantes
Modelo1C <- lm(alquiler ~ ingresos_zona + asentamiento, data = datos)
summ(Modelo1C)

set.seed(123)
datos$random <- runif(nrow(datos), min = 200, max = 1200)
Modelo1C <- lm(alquiler ~ ingresos_zona + random, data = datos)
summ(Modelo1C)


#--------------------------------------------------------------------
# MULTICOLINEALIDAD
#--------------------------------------------------------------------

vif(Modelo4)
summ(Modelo4, vifs = TRUE)

Modelo4aux <- lm(
  ingresos_zona ~ sq_ingresos_zona + banos + alcobas + casa + derrumbe +
    luz_solar + grietas + asentamiento + humedades + goteras + ventilacion,
  data = datos
)
vif_iz <- 1 / (1 - summary(Modelo4aux)$r.squared)
vif_iz

vif_iz2 <- vif(Modelo4)["ingresos_zona"]
vif_iz2


#--------------------------------------------------------------------
# ANÁLISIS DE RESIDUOS
#--------------------------------------------------------------------

datos$resid4 <- residuals(Modelo4)
hist(residuals(Modelo4), main = "Histogram of Residuals", xlab = "Residuals")
plot(density(datos$resid4), main = "Densidad de residuos M4")

jarqueberaTest(residuals(Modelo4))


#--------------------------------------------------------------------
# CONTRASTE RESET de Ramsey
#--------------------------------------------------------------------

resettest(Modelo4, power = 2:3, type = "fitted")
resettest(Modelo4, power = 2,   type = "fitted")

datos$sq_banos   <- datos$banos^2
datos$sq_alcobas <- datos$alcobas^2

Modelo9 <- lm(
  lalquiler ~ ingresos_zona + sq_ingresos_zona + banos + sq_banos +
    alcobas + sq_alcobas + casa + derrumbe + luz_solar + grietas +
    asentamiento + humedades + goteras + ventilacion,
  data = datos
)
summary(Modelo9)
resettest(Modelo9, power = 2:3, type = "fitted")

datos$pred9   <- predict(Modelo9)
datos$pred9_2 <- datos$pred9^2
datos$pred9_3 <- datos$pred9^3

Modelo10 <- lm(
  lalquiler ~ ingresos_zona + sq_ingresos_zona + banos + sq_banos +
    alcobas + sq_alcobas + casa + derrumbe + luz_solar + grietas +
    asentamiento + humedades + goteras + ventilacion +
    pred9_2 + pred9_3,
  data = datos
)
summary(Modelo10)
linearHypothesis(Modelo10, c("pred9_2=0", "pred9_3=0"))


#--------------------------------------------------------------------
# COMPARACIÓN Y SELECCIÓN DE MODELOS
#--------------------------------------------------------------------

# Métricas de Modelo1
datos$pred1_M1  <- predict(Modelo1)
datos$resid1_M1 <- resid(Modelo1)
M1_MAE  <- mean(abs(datos$resid1_M1))
M1_MSE  <- mean(datos$resid1_M1^2)
M1_RMSE <- sqrt(M1_MSE)

compR2aj <- cbind(
  summary(Modelo1)$adj.r.squared,
  summary(Modelo4)$adj.r.squared
)
colnames(compR2aj) <- c("Modelo 1", "Modelo 4")
rownames(compR2aj) <- c("R^2 ajustado")

compAIC <- cbind(AIC(Modelo1), AIC(Modelo4))
colnames(compAIC) <- c("Modelo 1", "Modelo 4")
rownames(compAIC) <- c("Akaike Information Criterion")

compBIC <- cbind(BIC(Modelo1), BIC(Modelo4))
colnames(compBIC) <- c("Modelo 1", "Modelo 4")
rownames(compBIC) <- c("Bayesian Information Criterion")

compMAE <- cbind(M1_MAE, M4_MAE)
colnames(compMAE) <- c("Modelo 1", "Modelo 4")
rownames(compMAE) <- c("Mean Absolute Error")

compRMSE <- cbind(M1_RMSE, M4_RMSE)
colnames(compRMSE) <- c("Modelo 1", "Modelo 4")
rownames(compRMSE) <- c("Root Mean Square Error")

compR2aj
compAIC
compBIC
compMAE
compRMSE

glance(Modelo1) %>% dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
glance(Modelo4) %>% dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

effect_plot(Modelo4, pred = ingresos_zona,
            interval = TRUE, plot.points = TRUE, jitter = 0.05)

summ(Modelo4, confint = TRUE, digits = 3)
plot_summs(Modelo4)
plot_summs(Modelo4, plot.distributions = TRUE, rescale.distributions = TRUE)

stargazer(
  Modelo3, Modelo4,
  type = "text",
  title = "Comparación de modelos",
  column.labels = c("Modelo 3", "Modelo 4"),
  model.numbers = FALSE
)

plot_summs(Modelo3, Modelo4)
export_summs(Modelo3, Modelo4, scale = FALSE)







