# ==============================================================================
# SENSITIVITY ANALYSIS: WINE QUALITY VARIANCE TESTING
# Testing how changes in physicochemical properties affect quality predictions
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP AND DATA LOADING
# ------------------------------------------------------------------------------

required_packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "MASS")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

rm(list = ls())
setwd("C:/Users/arias/Desktop")

wine_data <- read.csv("wine+quality/winequality-white.csv", sep = ";")

cat("\n=== SENSITIVITY ANALYSIS INITIALIZATION ===\n")
cat("Dataset loaded:", nrow(wine_data), "observations\n\n")

# ------------------------------------------------------------------------------
# 2. CORRELATION ANALYSIS WITH QUALITY
# ------------------------------------------------------------------------------

cat("=== CORRELATION ANALYSIS: IDENTIFYING KEY PREDICTORS ===\n\n")

numeric_vars <- wine_data[, sapply(wine_data, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

cor_with_quality <- cor_matrix[, "quality"]
cor_with_quality_sorted <- sort(cor_with_quality, decreasing = TRUE)

cat("Correlation with Quality (all variables):\n")
cor_df <- data.frame(
  Variable = names(cor_with_quality_sorted),
  Correlation = cor_with_quality_sorted,
  Abs_Correlation = abs(cor_with_quality_sorted),
  Direction = ifelse(cor_with_quality_sorted > 0, "Positive", "Negative")
)
cor_df <- cor_df[cor_df$Variable != "quality", ]
cor_df <- cor_df[order(cor_df$Abs_Correlation, decreasing = TRUE), ]
print(cor_df)

cat("\n--- KEY FINDINGS ---\n")
cat("Strongest positive predictor:", cor_df$Variable[cor_df$Direction == "Positive"][1],
    "| r =", round(cor_df$Correlation[cor_df$Direction == "Positive"][1], 4), "\n")
cat("Strongest negative predictor:", cor_df$Variable[cor_df$Direction == "Negative"][1],
    "| r =", round(cor_df$Correlation[cor_df$Direction == "Negative"][1], 4), "\n")

variables_above_alcohol <- cor_df$Variable[cor_df$Abs_Correlation > abs(cor_df$Correlation[cor_df$Variable == "alcohol"])]
if(length(variables_above_alcohol) > 0) {
  cat("\nVariables with STRONGER correlation than alcohol:\n")
  for(v in variables_above_alcohol) {
    cat(sprintf("  - %s: r = %.4f (vs alcohol: r = %.4f)\n", 
                v, 
                cor_df$Correlation[cor_df$Variable == v],
                cor_df$Correlation[cor_df$Variable == "alcohol"]))
  }
} else {
  cat("\nAlcohol is the STRONGEST single predictor of quality\n")
  cat(sprintf("  Alcohol correlation: r = %.4f\n", cor_df$Correlation[cor_df$Variable == "alcohol"]))
  cat("\nNext strongest predictors:\n")
  top_after_alcohol <- head(cor_df[cor_df$Variable != "alcohol", ], 3)
  for(i in 1:nrow(top_after_alcohol)) {
    cat(sprintf("  %d. %s: r = %.4f\n", i, top_after_alcohol$Variable[i], top_after_alcohol$Correlation[i]))
  }
}

cat("\n--- COMPARISON: Alcohol vs Other Predictors ---\n")
alcohol_cor <- abs(cor_df$Correlation[cor_df$Variable == "alcohol"])
for(var in head(cor_df$Variable[cor_df$Variable != "alcohol"], 5)) {
  var_cor <- abs(cor_df$Correlation[cor_df$Variable == var])
  ratio <- (var_cor / alcohol_cor) * 100
  cat(sprintf("%s is %.1f%% as strong as alcohol (|r| = %.4f vs %.4f)\n",
              var, ratio, var_cor, alcohol_cor))
}

# ------------------------------------------------------------------------------
# 4. BUILD BASELINE REGRESSION MODEL
# ------------------------------------------------------------------------------

cat("\n=== BUILDING BASELINE REGRESSION MODEL ===\n")

baseline_model <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
                     residual.sugar + chlorides + free.sulfur.dioxide + 
                     total.sulfur.dioxide + density + pH + sulphates + alcohol,
                     data = wine_data)

cat("\nBaseline Model Summary:\n")
print(summary(baseline_model))

baseline_r2 <- summary(baseline_model)$r.squared
baseline_rmse <- sqrt(mean(baseline_model$residuals^2))

cat("\nBaseline Performance Metrics:\n")
cat("R-squared:", round(baseline_r2, 4), "\n")
cat("RMSE:", round(baseline_rmse, 4), "\n")
cat("Adjusted R-squared:", round(summary(baseline_model)$adj.r.squared, 4), "\n")

# ------------------------------------------------------------------------------
# 5. IDENTIFY KEY VARIABLES FOR SENSITIVITY ANALYSIS
# ------------------------------------------------------------------------------

cat("\n=== IDENTIFYING KEY VARIABLES FOR SENSITIVITY ANALYSIS ===\n")

coefficients_df <- data.frame(
  Variable = names(coef(baseline_model))[-1],
  Coefficient = coef(baseline_model)[-1],
  Std_Error = summary(baseline_model)$coefficients[-1, "Std. Error"],
  t_value = summary(baseline_model)$coefficients[-1, "t value"],
  p_value = summary(baseline_model)$coefficients[-1, "Pr(>|t|)"]
)

coefficients_df$Abs_Coefficient <- abs(coefficients_df$Coefficient)
coefficients_df <- coefficients_df[order(coefficients_df$Abs_Coefficient, decreasing = TRUE), ]

cat("\nRegression Coefficients (ordered by absolute magnitude):\n")
print(coefficients_df)

top_variables <- head(coefficients_df$Variable, 5)
cat("\nTop 5 variables selected for sensitivity analysis:\n")
print(top_variables)

# ------------------------------------------------------------------------------
# 6. SENSITIVITY ANALYSIS: SYSTEMATIC PERTURBATION
# ------------------------------------------------------------------------------

cat("\n=== RUNNING SENSITIVITY ANALYSIS ===\n")

perturbation_levels <- c(-20, -15, -10, -5, 0, 5, 10, 15, 20)
sensitivity_results <- list()

for(var in top_variables) {
  
  cat("\n--- Analyzing variable:", var, "---\n")
  
  var_mean <- mean(wine_data[[var]], na.rm = TRUE)
  var_sd <- sd(wine_data[[var]], na.rm = TRUE)
  
  results_var <- data.frame(
    Variable = character(),
    Perturbation_Pct = numeric(),
    Perturbation_Value = numeric(),
    Mean_Predicted_Quality = numeric(),
    SD_Predicted_Quality = numeric(),
    Min_Quality = numeric(),
    Max_Quality = numeric(),
    Variance_Change = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(pct in perturbation_levels) {
    
    wine_perturbed <- wine_data
    perturbation <- var_mean * (pct / 100)
    wine_perturbed[[var]] <- wine_data[[var]] + perturbation
    
    predictions <- predict(baseline_model, newdata = wine_perturbed)
    
    mean_pred <- mean(predictions, na.rm = TRUE)
    sd_pred <- sd(predictions, na.rm = TRUE)
    min_pred <- min(predictions, na.rm = TRUE)
    max_pred <- max(predictions, na.rm = TRUE)
    
    baseline_predictions <- predict(baseline_model, newdata = wine_data)
    variance_change <- var(predictions, na.rm = TRUE) - var(baseline_predictions, na.rm = TRUE)
    
    results_var <- rbind(results_var, data.frame(
      Variable = var,
      Perturbation_Pct = pct,
      Perturbation_Value = perturbation,
      Mean_Predicted_Quality = mean_pred,
      SD_Predicted_Quality = sd_pred,
      Min_Quality = min_pred,
      Max_Quality = max_pred,
      Variance_Change = variance_change
    ))
  }
  
  sensitivity_results[[var]] <- results_var
  
  cat("Original mean:", round(var_mean, 4), "\n")
  cat("Original SD:", round(var_sd, 4), "\n")
  cat("Quality change range:", 
      round(min(results_var$Mean_Predicted_Quality) - 
            results_var$Mean_Predicted_Quality[results_var$Perturbation_Pct == 0], 4),
      "to",
      round(max(results_var$Mean_Predicted_Quality) - 
            results_var$Mean_Predicted_Quality[results_var$Perturbation_Pct == 0], 4), "\n")
}

sensitivity_combined <- do.call(rbind, sensitivity_results)

# ------------------------------------------------------------------------------
# 7. CALCULATE SENSITIVITY METRICS
# ------------------------------------------------------------------------------

cat("\n=== CALCULATING SENSITIVITY METRICS ===\n")

sensitivity_metrics <- data.frame(
  Variable = character(),
  Elasticity = numeric(),
  Max_Quality_Change = numeric(),
  Quality_Range = numeric(),
  Variance_Sensitivity = numeric(),
  stringsAsFactors = FALSE
)

for(var in top_variables) {
  
  var_results <- sensitivity_results[[var]]
  baseline_quality <- var_results$Mean_Predicted_Quality[var_results$Perturbation_Pct == 0]
  
  pos_10_quality <- var_results$Mean_Predicted_Quality[var_results$Perturbation_Pct == 10]
  neg_10_quality <- var_results$Mean_Predicted_Quality[var_results$Perturbation_Pct == -10]
  
  elasticity <- ((pos_10_quality - baseline_quality) / baseline_quality) / 0.10
  
  max_quality_change <- max(abs(var_results$Mean_Predicted_Quality - baseline_quality))
  
  quality_range <- max(var_results$Mean_Predicted_Quality) - 
                   min(var_results$Mean_Predicted_Quality)
  
  variance_sensitivity <- max(abs(var_results$Variance_Change))
  
  sensitivity_metrics <- rbind(sensitivity_metrics, data.frame(
    Variable = var,
    Elasticity = elasticity,
    Max_Quality_Change = max_quality_change,
    Quality_Range = quality_range,
    Variance_Sensitivity = variance_sensitivity
  ))
}

sensitivity_metrics <- sensitivity_metrics[order(sensitivity_metrics$Max_Quality_Change, 
                                                 decreasing = TRUE), ]

cat("\nSensitivity Metrics Summary:\n")
print(sensitivity_metrics)

# ------------------------------------------------------------------------------
# 8. VISUALIZATION: SENSITIVITY CURVES
# ------------------------------------------------------------------------------

cat("\n=== GENERATING SENSITIVITY VISUALIZATIONS ===\n")

pdf("wine_sensitivity_curves.pdf", width = 14, height = 10)

par(mfrow = c(2, 3))
for(var in top_variables) {
  var_results <- sensitivity_results[[var]]
  
  plot(var_results$Perturbation_Pct, 
       var_results$Mean_Predicted_Quality,
       type = "b",
       pch = 19,
       col = "steelblue",
       lwd = 2,
       main = paste("Sensitivity Analysis:", var),
       xlab = "Perturbation (%)",
       ylab = "Mean Predicted Quality",
       ylim = c(min(var_results$Mean_Predicted_Quality) - 0.1,
                max(var_results$Mean_Predicted_Quality) + 0.1))
  
  grid()
  
  baseline_idx <- which(var_results$Perturbation_Pct == 0)
  abline(h = var_results$Mean_Predicted_Quality[baseline_idx], 
         col = "red", lty = 2, lwd = 2)
  
  arrows(var_results$Perturbation_Pct,
         var_results$Mean_Predicted_Quality - var_results$SD_Predicted_Quality,
         var_results$Perturbation_Pct,
         var_results$Mean_Predicted_Quality + var_results$SD_Predicted_Quality,
         length = 0.05, angle = 90, code = 3, col = "gray50")
  
  legend("topright",
         legend = c("Predicted Quality", "Baseline", "±1 SD"),
         col = c("steelblue", "red", "gray50"),
         lty = c(1, 2, 1),
         lwd = 2,
         cex = 0.8,
         bty = "n")
}

dev.off()

# ------------------------------------------------------------------------------
# 9. VARIANCE CHANGE ANALYSIS
# ------------------------------------------------------------------------------

pdf("wine_variance_sensitivity.pdf", width = 12, height = 8)

par(mfrow = c(2, 3))
for(var in top_variables) {
  var_results <- sensitivity_results[[var]]
  
  plot(var_results$Perturbation_Pct,
       var_results$Variance_Change,
       type = "b",
       pch = 19,
       col = "darkgreen",
       lwd = 2,
       main = paste("Variance Change:", var),
       xlab = "Perturbation (%)",
       ylab = "Change in Prediction Variance")
  
  grid()
  abline(h = 0, col = "red", lty = 2, lwd = 2)
}

dev.off()

# ------------------------------------------------------------------------------
# 10. HEATMAP: MULTI-VARIABLE SENSITIVITY
# ------------------------------------------------------------------------------

cat("\n=== CREATING SENSITIVITY HEATMAP ===\n")

sensitivity_matrix <- matrix(NA, 
                             nrow = length(perturbation_levels),
                             ncol = length(top_variables))
rownames(sensitivity_matrix) <- paste0(perturbation_levels, "%")
colnames(sensitivity_matrix) <- top_variables

for(i in seq_along(top_variables)) {
  var <- top_variables[i]
  var_results <- sensitivity_results[[var]]
  baseline_quality <- var_results$Mean_Predicted_Quality[var_results$Perturbation_Pct == 0]
  sensitivity_matrix[, i] <- var_results$Mean_Predicted_Quality - baseline_quality
}

pdf("wine_sensitivity_heatmap.pdf", width = 10, height = 8)

par(mar = c(8, 8, 4, 2))
image(1:ncol(sensitivity_matrix), 
      1:nrow(sensitivity_matrix),
      t(sensitivity_matrix),
      col = colorRampPalette(c("red", "white", "blue"))(100),
      xlab = "",
      ylab = "",
      xaxt = "n",
      yaxt = "n",
      main = "Sensitivity Heatmap: Quality Change by Variable Perturbation")

axis(1, at = 1:ncol(sensitivity_matrix), labels = colnames(sensitivity_matrix), 
     las = 2, cex.axis = 0.8)
axis(2, at = 1:nrow(sensitivity_matrix), labels = rownames(sensitivity_matrix), 
     las = 2, cex.axis = 0.8)

for(i in 1:ncol(sensitivity_matrix)) {
  for(j in 1:nrow(sensitivity_matrix)) {
    text(i, j, sprintf("%.3f", sensitivity_matrix[j, i]), cex = 0.6)
  }
}

dev.off()

# ------------------------------------------------------------------------------
# 11. INTERACTION EFFECTS: TWO-VARIABLE PERTURBATION
# ------------------------------------------------------------------------------

cat("\n=== ANALYZING INTERACTION EFFECTS ===\n")

interaction_variables <- head(top_variables, 2)
cat("Testing interaction between:", interaction_variables[1], "and", 
    interaction_variables[2], "\n")

perturbation_grid <- expand.grid(
  Var1_Pct = c(-10, 0, 10),
  Var2_Pct = c(-10, 0, 10)
)

interaction_results <- data.frame(
  Var1_Pct = numeric(),
  Var2_Pct = numeric(),
  Mean_Quality = numeric(),
  SD_Quality = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(perturbation_grid)) {
  
  wine_perturbed <- wine_data
  
  var1_mean <- mean(wine_data[[interaction_variables[1]]], na.rm = TRUE)
  var2_mean <- mean(wine_data[[interaction_variables[2]]], na.rm = TRUE)
  
  wine_perturbed[[interaction_variables[1]]] <- 
    wine_data[[interaction_variables[1]]] + var1_mean * (perturbation_grid$Var1_Pct[i] / 100)
  
  wine_perturbed[[interaction_variables[2]]] <- 
    wine_data[[interaction_variables[2]]] + var2_mean * (perturbation_grid$Var2_Pct[i] / 100)
  
  predictions <- predict(baseline_model, newdata = wine_perturbed)
  
  interaction_results <- rbind(interaction_results, data.frame(
    Var1_Pct = perturbation_grid$Var1_Pct[i],
    Var2_Pct = perturbation_grid$Var2_Pct[i],
    Mean_Quality = mean(predictions, na.rm = TRUE),
    SD_Quality = sd(predictions, na.rm = TRUE)
  ))
}

cat("\nInteraction Effects Results:\n")
print(interaction_results)

pdf("wine_interaction_effects.pdf", width = 10, height = 8)

interaction_matrix <- matrix(interaction_results$Mean_Quality, 
                             nrow = length(unique(interaction_results$Var1_Pct)),
                             ncol = length(unique(interaction_results$Var2_Pct)))

contour(unique(interaction_results$Var1_Pct),
        unique(interaction_results$Var2_Pct),
        interaction_matrix,
        xlab = paste(interaction_variables[1], "Perturbation (%)"),
        ylab = paste(interaction_variables[2], "Perturbation (%)"),
        main = "Interaction Effect: Quality Response Surface",
        col = "blue",
        lwd = 2)

filled.contour(unique(interaction_results$Var1_Pct),
               unique(interaction_results$Var2_Pct),
               interaction_matrix,
               xlab = paste(interaction_variables[1], "Perturbation (%)"),
               ylab = paste(interaction_variables[2], "Perturbation (%)"),
               main = "Interaction Effect: Quality Response Surface (Filled)",
               color.palette = colorRampPalette(c("red", "yellow", "green", "blue")))

dev.off()

# ------------------------------------------------------------------------------
# 12. EXPORT RESULTS
# ------------------------------------------------------------------------------

cat("\n=== EXPORTING SENSITIVITY ANALYSIS RESULTS ===\n")

write.csv(cor_df, "correlation_with_quality.csv", row.names = FALSE)
cat("Exported: correlation_with_quality.csv\n")

write.csv(sensitivity_combined, "sensitivity_analysis_detailed.csv", row.names = FALSE)
cat("Exported: sensitivity_analysis_detailed.csv\n")

write.csv(sensitivity_metrics, "sensitivity_metrics_summary.csv", row.names = FALSE)
cat("Exported: sensitivity_metrics_summary.csv\n")

write.csv(interaction_results, "interaction_effects_results.csv", row.names = FALSE)
cat("Exported: interaction_effects_results.csv\n")

write.csv(coefficients_df, "regression_coefficients.csv", row.names = FALSE)
cat("Exported: regression_coefficients.csv\n")

# ------------------------------------------------------------------------------
# 13. FINAL REPORT
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("SENSITIVITY ANALYSIS FINAL REPORT\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n1. CORRELATION ANALYSIS SUMMARY:\n")
cat(sprintf("   - Strongest predictor: %s (r = %.4f)\n", 
            cor_df$Variable[1], cor_df$Correlation[1]))
cat(sprintf("   - Second strongest: %s (r = %.4f)\n", 
            cor_df$Variable[2], cor_df$Correlation[2]))
cat(sprintf("   - Third strongest: %s (r = %.4f)\n", 
            cor_df$Variable[3], cor_df$Correlation[3]))
cat(sprintf("   - Alcohol ranking: #%d (r = %.4f)\n",
            which(cor_df$Variable == "alcohol"),
            cor_df$Correlation[cor_df$Variable == "alcohol"]))

cat("\n2. BASELINE MODEL PERFORMANCE:\n")
cat("   - R-squared:", round(baseline_r2, 4), "\n")
cat("   - RMSE:", round(baseline_rmse, 4), "\n")
cat("   - Variables analyzed:", length(top_variables), "\n")

cat("\n3. MOST SENSITIVE VARIABLES (by max quality change):\n")
for(i in 1:nrow(sensitivity_metrics)) {
  cat(sprintf("   %d. %s: Δ = %.4f (elasticity: %.4f)\n",
              i,
              sensitivity_metrics$Variable[i],
              sensitivity_metrics$Max_Quality_Change[i],
              sensitivity_metrics$Elasticity[i]))
}

cat("\n4. VARIANCE SENSITIVITY:\n")
for(i in 1:nrow(sensitivity_metrics)) {
  cat(sprintf("   - %s: Variance change = %.6f\n",
              sensitivity_metrics$Variable[i],
              sensitivity_metrics$Variance_Sensitivity[i]))
}

cat("\n5. KEY FINDINGS:\n")
most_sensitive <- sensitivity_metrics$Variable[1]
cat("   - Most sensitive variable:", most_sensitive, "\n")
cat("   - A 10% increase in", most_sensitive, "changes quality by approximately",
    round(sensitivity_metrics$Max_Quality_Change[1] / 2, 4), "points\n")

alcohol_ranking <- which(sensitivity_metrics$Variable == "alcohol")
if(length(alcohol_ranking) > 0) {
  cat(sprintf("   - Alcohol sensitivity ranking: #%d out of %d variables\n",
              alcohol_ranking, nrow(sensitivity_metrics)))
}

cat("\n6. FILES GENERATED:\n")
cat("   - correlation_with_quality.csv\n")
cat("   - wine_sensitivity_curves.pdf\n")
cat("   - wine_variance_sensitivity.pdf\n")
cat("   - wine_sensitivity_heatmap.pdf\n")
cat("   - wine_interaction_effects.pdf\n")
cat("   - sensitivity_analysis_detailed.csv\n")
cat("   - sensitivity_metrics_summary.csv\n")
cat("   - interaction_effects_results.csv\n")
cat("   - regression_coefficients.csv\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("SENSITIVITY ANALYSIS COMPLETED\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("SESSION INFO:\n")
print(sessionInfo())
