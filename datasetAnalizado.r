# ==============================================================================
# SECOND DELIVERY: DESCRIPTIVE AND EXPLORATORY DATA ANALYSIS
# Dataset: Wine Quality (White Wine)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. INITIAL SETUP AND PACKAGES
# ------------------------------------------------------------------------------

# Install required packages if not already installed
required_packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "moments", 
                       "gridExtra", "knitr", "psych", "car")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Clear workspace
rm(list = ls())

# Set working directory
setwd("C:/Users/arias/Desktop")

# ------------------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL STRUCTURE
# ------------------------------------------------------------------------------

# Load the dataset
wine_data <- read.csv("wine+quality/winequality-white.csv", sep = ";")

# Display basic structure
cat("\n=== GENERAL DATABASE CHARACTERISTICS ===\n")
cat("\nDataset Dimensions:\n")
cat("Number of observations (rows):", nrow(wine_data), "\n")
cat("Number of variables (columns):", ncol(wine_data), "\n")

# Variable names and types
cat("\nVariable Structure:\n")
str(wine_data)

# First rows preview
cat("\nFirst 6 observations:\n")
print(head(wine_data))

# Summary statistics for all variables
cat("\nGeneral Summary Statistics:\n")
print(summary(wine_data))

# ------------------------------------------------------------------------------
# 3. DATA QUALITY ASSESSMENT
# ------------------------------------------------------------------------------

cat("\n=== DATA QUALITY AND LIMITATIONS ===\n")

# Check for missing values
missing_values <- colSums(is.na(wine_data))
cat("\nMissing values per variable:\n")
print(missing_values)
cat("\nTotal missing values:", sum(missing_values), "\n")
cat("Percentage of complete cases:", 
    round(mean(complete.cases(wine_data)) * 100, 2), "%\n")

# Check for duplicates
duplicates <- sum(duplicated(wine_data))
cat("\nNumber of duplicate rows:", duplicates, "\n")
if(duplicates > 0) {
  cat("Percentage of duplicates:", round(duplicates/nrow(wine_data)*100, 2), "%\n")
}

# Check for potential data entry errors
cat("\n--- Range Validation ---\n")
for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
    cat(sprintf("%s: Min = %.3f, Max = %.3f\n", 
                col, min(wine_data[[col]], na.rm = TRUE), 
                max(wine_data[[col]], na.rm = TRUE)))
  }
}

# Check for outliers using IQR method
cat("\n--- Outlier Detection (IQR Method) ---\n")
outlier_summary <- data.frame(
  Variable = character(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  N_Outliers = integer(),
  Pct_Outliers = numeric(),
  stringsAsFactors = FALSE
)

for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
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
}

print(outlier_summary)

# ------------------------------------------------------------------------------
# 4. UNIVARIATE ANALYSIS - QUANTITATIVE VARIABLES
# ------------------------------------------------------------------------------

cat("\n=== UNIVARIATE ANALYSIS - QUANTITATIVE VARIABLES ===\n")

# Detailed descriptive statistics
descriptive_stats <- data.frame(
  Variable = character(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  Variance = numeric(),
  Min = numeric(),
  Max = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  IQR = numeric(),
  Skewness = numeric(),
  Kurtosis = numeric(),
  stringsAsFactors = FALSE
)

for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
    var_data <- wine_data[[col]]
    
    descriptive_stats <- rbind(descriptive_stats,
                              data.frame(
                                Variable = col,
                                Mean = mean(var_data, na.rm = TRUE),
                                Median = median(var_data, na.rm = TRUE),
                                SD = sd(var_data, na.rm = TRUE),
                                Variance = var(var_data, na.rm = TRUE),
                                Min = min(var_data, na.rm = TRUE),
                                Max = max(var_data, na.rm = TRUE),
                                Q1 = quantile(var_data, 0.25, na.rm = TRUE),
                                Q3 = quantile(var_data, 0.75, na.rm = TRUE),
                                IQR = IQR(var_data, na.rm = TRUE),
                                Skewness = skewness(var_data, na.rm = TRUE),
                                Kurtosis = kurtosis(var_data, na.rm = TRUE)
                              ))
  }
}

cat("\nDescriptive Statistics Table:\n")
print(round(descriptive_stats, 3))

# Create histograms for all quantitative variables
pdf("wine_histograms.pdf", width = 14, height = 10)
par(mfrow = c(3, 4))
for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
    hist(wine_data[[col]], 
         main = paste("Histogram:", col),
         xlab = col,
         col = "lightblue",
         border = "black",
         breaks = 30)
    abline(v = mean(wine_data[[col]], na.rm = TRUE), 
           col = "red", lwd = 2, lty = 2)
    abline(v = median(wine_data[[col]], na.rm = TRUE), 
           col = "blue", lwd = 2, lty = 2)
    legend("topright", 
           legend = c("Mean", "Median"), 
           col = c("red", "blue"), 
           lty = 2, lwd = 2, cex = 0.7)
  }
}
dev.off()

# Create boxplots for all quantitative variables
pdf("wine_boxplots.pdf", width = 14, height = 10)
par(mfrow = c(3, 4))
for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
    boxplot(wine_data[[col]], 
            main = paste("Boxplot:", col),
            ylab = col,
            col = "lightgreen",
            border = "black",
            outline = TRUE)
  }
}
dev.off()

# Create density plots
pdf("wine_density_plots.pdf", width = 14, height = 10)
par(mfrow = c(3, 4))
for(col in names(wine_data)) {
  if(is.numeric(wine_data[[col]])) {
    plot(density(wine_data[[col]], na.rm = TRUE),
         main = paste("Density Plot:", col),
         xlab = col,
         col = "blue",
         lwd = 2)
    polygon(density(wine_data[[col]], na.rm = TRUE), 
            col = rgb(0, 0, 1, 0.3))
  }
}
dev.off()

# ------------------------------------------------------------------------------
# 5. ANALYSIS OF QUALITY VARIABLE (TARGET)
# ------------------------------------------------------------------------------

cat("\n=== ANALYSIS OF QUALITY VARIABLE (TARGET) ===\n")

# Frequency table for quality
quality_freq <- table(wine_data$quality)
quality_prop <- prop.table(quality_freq) * 100

cat("\nQuality Distribution:\n")
quality_table <- data.frame(
  Quality = as.numeric(names(quality_freq)),
  Frequency = as.numeric(quality_freq),
  Percentage = round(as.numeric(quality_prop), 2)
)
print(quality_table)

# Create quality bar plot
pdf("wine_quality_distribution.pdf", width = 10, height = 6)
barplot(quality_freq,
        main = "Distribution of Wine Quality Scores",
        xlab = "Quality Score",
        ylab = "Frequency",
        col = "steelblue",
        border = "black")
text(x = seq_along(quality_freq) * 1.2 - 0.5, 
     y = quality_freq + max(quality_freq) * 0.05,
     labels = quality_freq,
     cex = 0.9)
dev.off()

# Create quality categories for analysis
wine_data$quality_category <- cut(wine_data$quality,
                                  breaks = c(0, 5, 6, 10),
                                  labels = c("Low (3-5)", "Medium (6)", "High (7-9)"),
                                  include.lowest = TRUE)

cat("\nQuality Categories:\n")
print(table(wine_data$quality_category))
print(prop.table(table(wine_data$quality_category)) * 100)

# ------------------------------------------------------------------------------
# 6. BIVARIATE ANALYSIS - CORRELATION ANALYSIS
# ------------------------------------------------------------------------------

cat("\n=== BIVARIATE ANALYSIS - CORRELATIONS ===\n")

# Select only numeric variables
numeric_vars <- wine_data[, sapply(wine_data, is.numeric)]

# Correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

# Create correlation plot
pdf("wine_correlation_matrix.pdf", width = 12, height = 12)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Correlation Matrix - Wine Quality Dataset",
         mar = c(0,0,2,0))
dev.off()

# Identify strongest correlations with quality
cor_with_quality <- cor_matrix[, "quality"]
cor_with_quality <- sort(abs(cor_with_quality), decreasing = TRUE)
cat("\nVariables most correlated with Quality (absolute values):\n")
print(round(cor_with_quality, 3))

# Identify highly correlated variable pairs (potential multicollinearity)
cat("\n--- Highly Correlated Variable Pairs (|r| > 0.7) ---\n")
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  high_cor_unique <- high_cor[high_cor[,1] < high_cor[,2], , drop = FALSE]
  for(i in 1:nrow(high_cor_unique)) {
    var1 <- rownames(cor_matrix)[high_cor_unique[i,1]]
    var2 <- colnames(cor_matrix)[high_cor_unique[i,2]]
    cor_val <- cor_matrix[high_cor_unique[i,1], high_cor_unique[i,2]]
    cat(sprintf("%s vs %s: r = %.3f\n", var1, var2, cor_val))
  }
}

# ------------------------------------------------------------------------------
# 7. BIVARIATE ANALYSIS - SCATTER PLOTS
# ------------------------------------------------------------------------------

cat("\n=== CREATING SCATTER PLOTS ===\n")

# Top variables correlated with quality
top_vars <- names(sort(abs(cor_matrix[, "quality"]), decreasing = TRUE)[2:7])

pdf("wine_scatter_quality.pdf", width = 14, height = 10)
par(mfrow = c(2, 3))
for(var in top_vars) {
  plot(wine_data[[var]], wine_data$quality,
       main = paste("Quality vs", var),
       xlab = var,
       ylab = "Quality",
       pch = 19,
       col = rgb(0, 0, 1, 0.3),
       cex = 0.8)
  
  # Add regression line
  abline(lm(wine_data$quality ~ wine_data[[var]]), col = "red", lwd = 2)
  
  # Add correlation coefficient
  cor_val <- cor(wine_data[[var]], wine_data$quality, use = "complete.obs")
  legend("topright", 
         legend = sprintf("r = %.3f", cor_val),
         bty = "n",
         cex = 1.2)
}
dev.off()

# Create scatter plot matrix for key variables
key_vars <- c("alcohol", "density", "chlorides", "volatile.acidity", 
              "total.sulfur.dioxide", "quality")
pdf("wine_scatterplot_matrix.pdf", width = 14, height = 14)
pairs(wine_data[, key_vars],
      main = "Scatter Plot Matrix - Key Variables",
      pch = 19,
      col = rgb(0, 0, 1, 0.2),
      cex = 0.5)
dev.off()

# ------------------------------------------------------------------------------
# 8. BIVARIATE ANALYSIS - QUALITY CATEGORIES
# ------------------------------------------------------------------------------

cat("\n=== ANALYSIS BY QUALITY CATEGORIES ===\n")

# Boxplots by quality category
pdf("wine_boxplots_by_quality.pdf", width = 14, height = 10)
par(mfrow = c(3, 4))
for(col in names(numeric_vars)) {
  if(col != "quality") {
    boxplot(wine_data[[col]] ~ wine_data$quality,
            main = paste(col, "by Quality"),
            xlab = "Quality Score",
            ylab = col,
            col = rainbow(length(unique(wine_data$quality))),
            border = "black")
  }
}
dev.off()

# Summary statistics by quality category
cat("\nMean values by Quality Category:\n")
aggregate(. ~ quality, data = numeric_vars, FUN = mean) %>% print()

# ANOVA tests to check significant differences
cat("\n--- ANOVA Tests (p-values) ---\n")
anova_results <- data.frame(
  Variable = character(),
  F_statistic = numeric(),
  p_value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for(col in names(numeric_vars)) {
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
                            Significant = ifelse(p_val < 0.05, "Yes (p<0.05)", "No")
                          ))
  }
}

print(anova_results)

# ------------------------------------------------------------------------------
# 9. ADDITIONAL TRANSFORMATIONS AND ANALYSIS
# ------------------------------------------------------------------------------

cat("\n=== VARIABLE TRANSFORMATIONS ===\n")

# Check normality for key variables
cat("\nShapiro-Wilk Normality Tests (sample of 5000 obs if n > 5000):\n")
normality_results <- data.frame(
  Variable = character(),
  W_statistic = numeric(),
  p_value = numeric(),
  Normal = character(),
  stringsAsFactors = FALSE
)

for(col in names(numeric_vars)) {
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
                                            "Yes (p>0.05)", "No (p<0.05)")
                            ))
}

print(normality_results)

# Create log transformations for skewed variables
cat("\n--- Log Transformations for Highly Skewed Variables ---\n")
skewed_vars <- descriptive_stats$Variable[abs(descriptive_stats$Skewness) > 1]
cat("Variables with |skewness| > 1:", paste(skewed_vars, collapse = ", "), "\n")

for(var in skewed_vars) {
  if(all(wine_data[[var]] > 0, na.rm = TRUE)) {
    log_var_name <- paste0("log_", var)
    wine_data[[log_var_name]] <- log(wine_data[[var]])
    cat(sprintf("Created %s (skewness = %.3f)\n", 
                log_var_name, 
                skewness(wine_data[[log_var_name]], na.rm = TRUE)))
  }
}

# ------------------------------------------------------------------------------
# 10. MULTICOLLINEARITY CHECK (VIF)
# ------------------------------------------------------------------------------

cat("\n=== MULTICOLLINEARITY ANALYSIS (VIF) ===\n")

# Prepare data for VIF calculation
model_data <- numeric_vars[complete.cases(numeric_vars), ]

# Calculate VIF for each predictor
vif_results <- data.frame(
  Variable = character(),
  VIF = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

for(col in names(model_data)) {
  if(col != "quality") {
    formula <- as.formula(paste(col, "~ ."))
    lm_model <- lm(formula, data = model_data[, names(model_data) != "quality"])
    r_squared <- summary(lm_model)$r.squared
    vif_value <- 1 / (1 - r_squared)
    
    interpretation <- if(vif_value < 5) {
      "Low"
    } else if(vif_value < 10) {
      "Moderate"
    } else {
      "High (potential problem)"
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
cat("\nVariance Inflation Factor (VIF) Results:\n")
print(vif_results)

# ------------------------------------------------------------------------------
# 11. EXPORT SUMMARY TABLES
# ------------------------------------------------------------------------------

cat("\n=== EXPORTING SUMMARY TABLES ===\n")

# Export descriptive statistics
write.csv(descriptive_stats, "descriptive_statistics.csv", row.names = FALSE)
cat("Exported: descriptive_statistics.csv\n")

# Export correlation matrix
write.csv(cor_matrix, "correlation_matrix.csv")
cat("Exported: correlation_matrix.csv\n")

# Export outlier summary
write.csv(outlier_summary, "outlier_summary.csv", row.names = FALSE)
cat("Exported: outlier_summary.csv\n")

# Export quality distribution
write.csv(quality_table, "quality_distribution.csv", row.names = FALSE)
cat("Exported: quality_distribution.csv\n")

# Export ANOVA results
write.csv(anova_results, "anova_results.csv", row.names = FALSE)
cat("Exported: anova_results.csv\n")

# Export VIF results
write.csv(vif_results, "vif_results.csv", row.names = FALSE)
cat("Exported: vif_results.csv\n")

# ------------------------------------------------------------------------------
# 12. FINAL SUMMARY REPORT
# ------------------------------------------------------------------------------

cat("\n" , rep("=", 80), "\n", sep = "")
cat("FINAL SUMMARY REPORT\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n1. DATABASE CHARACTERISTICS:\n")
cat("   - Observations:", nrow(wine_data), "\n")
cat("   - Variables:", ncol(wine_data) - 1, "(original)\n")
cat("   - Complete cases:", sum(complete.cases(wine_data)), "\n")
cat("   - Missing values:", sum(is.na(wine_data)), "\n")
cat("   - Duplicates:", duplicates, "\n")

cat("\n2. QUALITY VARIABLE:\n")
cat("   - Range:", min(wine_data$quality), "-", max(wine_data$quality), "\n")
cat("   - Mean:", round(mean(wine_data$quality), 2), "\n")
cat("   - Median:", median(wine_data$quality), "\n")
cat("   - Most frequent:", names(which.max(quality_freq)), 
    "(", max(quality_freq), "observations )\n")

cat("\n3. TOP CORRELATIONS WITH QUALITY:\n")
top_cor_quality <- sort(cor_matrix[, "quality"], decreasing = TRUE)[2:6]
for(i in 1:length(top_cor_quality)) {
  cat(sprintf("   - %s: r = %.3f\n", names(top_cor_quality)[i], top_cor_quality[i]))
}

cat("\n4. POTENTIAL DATA LIMITATIONS:\n")
cat("   - Variables with >10% outliers:", 
    sum(outlier_summary$Pct_Outliers > 10), "\n")
cat("   - Variables with high VIF (>10):", 
    sum(vif_results$VIF > 10), "\n")
cat("   - Non-normal variables (p<0.05):", 
    sum(normality_results$p_value < 0.05), "\n")

cat("\n5. FILES GENERATED:\n")
cat("   - wine_histograms.pdf\n")
cat("   - wine_boxplots.pdf\n")
cat("   - wine_density_plots.pdf\n")
cat("   - wine_quality_distribution.pdf\n")
cat("   - wine_correlation_matrix.pdf\n")
cat("   - wine_scatter_quality.pdf\n")
cat("   - wine_scatterplot_matrix.pdf\n")
cat("   - wine_boxplots_by_quality.pdf\n")
cat("   - descriptive_statistics.csv\n")
cat("   - correlation_matrix.csv\n")
cat("   - outlier_summary.csv\n")
cat("   - quality_distribution.csv\n")
cat("   - anova_results.csv\n")
cat("   - vif_results.csv\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(rep("=", 80), "\n\n", sep = "")

# Print R session information for citation
cat("R SESSION INFORMATION FOR CITATION:\n")
print(sessionInfo())
