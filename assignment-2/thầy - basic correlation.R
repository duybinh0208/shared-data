# 1. VISUAL EXPLORATION - "Let's see what correlates with sales"

# Select numeric columns only
numeric_cols <- retail_data[, 2:16]

# Create correlation matrix
correlations <- cor(numeric_cols)

# Find correlations with sales
sales_correlations <- correlations["monthly_sales", -ncol(correlations)]
sales_correlations <- sort(abs(sales_correlations), decreasing = TRUE)

# Visualize top correlations
par(mar = c(8, 4, 4, 2))
barplot(sales_correlations[1:8], 
        main = "Which Factors Correlate Most with Sales?",
        las = 2,
        col = "steelblue",
        ylab = "Correlation Strength")

# Business interpretation
cat("\nTOP 5 BUSINESS DRIVERS (by correlation):\n")
for(i in 1:5) {
  cat(i, ". ", names(sales_correlations)[i], 
      " (", round(sales_correlations[i], 3), ")\n", sep = "")
}

# 2. REGRESSION ANALYSIS - "What predicts sales when considering everything together?"

# Standardize predictors for fair comparison
predictors <- names(retail_data)[2:15]
retail_scaled <- retail_data
retail_scaled[predictors] <- scale(retail_data[predictors])

# Build model with all factors
full_model <- lm(monthly_sales ~ . - store_id, data = retail_scaled)

# Extract importance (absolute t-values)
importance <- abs(coef(summary(full_model))[, "t value"])
importance <- sort(importance[-1], decreasing = TRUE)  # Remove intercept

# Visualize statistical importance
par(mar = c(8, 4, 4, 2))
barplot(importance[1:8],
        main = "Statistical Importance of Business Factors",
        las = 2,
        col = "darkgreen",
        ylab = "Importance Score")

# Business interpretation
cat("\nKEY BUSINESS DRIVERS (by statistical significance):\n")
for(i in 1:5) {
  cat(i, ". ", names(importance)[i], "\n", sep = "")
}

# Show R-squared
cat("\nModel explains", round(summary(full_model)$r.squared * 100, 1), "% of sales variation\n")


