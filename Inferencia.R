# ============================================================
# INFERENCIA: EFECTO DE LAS PROPIEDADES QUÍMICAS SEGÚN LA CALIDAD
# ============================================================

library(tidyverse)
library(ggplot2)
library(broom)

say <- function(...) cat(paste0(...), "\n")

# -------------------------------------------------------------
# 1. Cargar datos
# -------------------------------------------------------------
datos <- wine_data

# -------------------------------------------------------------
# 2. Seleccionar todas las propiedades químicas relevantes
# -------------------------------------------------------------

quimicas <- c(
  "fixed.acidity","volatile.acidity","citric.acid","residual.sugar",
  "chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density",
  "pH","sulphates","alcohol"
)

dat <- datos |>
  select(type, quality, all_of(quimicas)) |>
  filter(!is.na(type), !is.na(quality)) |>
  mutate(type = factor(type),
         quality = factor(quality))   # calidad como factor

# Separar tinto y blanco
subT <- dat |> filter(type == "tinto")
subB <- dat |> filter(type == "blanco")

# ============================================================
# 3. IC 95% para medias de cada propiedad por nivel de calidad
# ============================================================

say("\n=== B1. Intervalos de confianza por calidad — TINTOS ===")

ic_T <- subT |>
  group_by(quality) |>
  summarise(
    n = n(),
    across(all_of(quimicas),
           list(
             mean = ~mean(.x),
             L    = ~mean(.x) - qt(.975, df=n()-1) * sd(.x)/sqrt(n()),
             U    = ~mean(.x) + qt(.975, df=n()-1) * sd(.x)/sqrt(n())
           ),
           .names="{.col}_{.fn}"
    )
  )

print(ic_T)


say("\n=== B1.bis Intervalos de confianza por calidad — BLANCOS ===")

ic_B <- subB |>
  group_by(quality) |>
  summarise(
    n = n(),
    across(all_of(quimicas),
           list(
             mean = ~mean(.x),
             L    = ~mean(.x) - qt(.975, df=n()-1) * sd(.x)/sqrt(n()),
             U    = ~mean(.x) + qt(.975, df=n()-1) * sd(.x)/sqrt(n())
           ),
           .names="{.col}_{.fn}"
    )
  )

print(ic_B)


# ============================================================
# 4. ANOVA PARA CADA PROPIEDAD QUÍMICA (tinto y blanco)
# ============================================================

run_anova_table <- function(data, propiedades) {
  map_df(propiedades, function(v) {
    modelo <- aov(reformulate("quality", v), data=data)
    tibble(
      variable = v,
      df = df.residual(modelo),
      statistic = summary(modelo)[[1]][["F value"]][1],
      p.value = summary(modelo)[[1]][["Pr(>F)"]][1]
    )
  })
}

say("\n=== B2. ANOVA por propiedad — TINTOS ===")
anova_T <- run_anova_table(subT, quimicas)
print(anova_T)

say("\n=== B2.bis ANOVA por propiedad — BLANCOS ===")
anova_B <- run_anova_table(subB, quimicas)
print(anova_B)


# ============================================================
# 5. TURKEY HSD PARA CADA VARIABLE QUÍMICA SIGNIFICATIVA
# ============================================================

run_tukey_sig <- function(data, propiedades, tabla_anova) {
  
  sig <- tabla_anova |> filter(p.value < 0.05)
  
  if (nrow(sig) == 0) return(NULL)
  
  map(sig$variable, function(v) {
    say(paste0("\n>>> Tukey HSD para ", v, " ~ quality"))
    modelo <- aov(reformulate("quality", v), data=data)
    print(TukeyHSD(modelo))
  })
}

say("\n=== B3. Tukey HSD — TINTOS ===")
tukey_T <- run_tukey_sig(subT, quimicas, anova_T)

say("\n=== B3.bis Tukey HSD — BLANCOS ===")
tukey_B <- run_tukey_sig(subB, quimicas, anova_B)

# ============================================================
# 6. BOXPLOTS PARA VISUALIZAR DIFERENCIAS
# ============================================================

plot_box <- function(data, title) {
  data_long <- data |>
    pivot_longer(cols = all_of(quimicas),
                 names_to = "variable",
                 values_to = "valor")
  
  ggplot(data_long, aes(x = quality, y = valor, fill = quality)) +
    geom_boxplot() +
    facet_wrap(~variable, scales="free_y") +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = title, x="Calidad", y="Valor")
}

say("\n=== B4. Graficando diferencias — TINTOS ===")
print(plot_box(subT, "Propiedades químicas según calidad — Tintos"))

say("\n=== B4.bis Graficando diferencias — BLANCOS ===")
print(plot_box(subB, "Propiedades químicas según calidad — Blancos"))