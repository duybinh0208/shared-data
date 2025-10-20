# Load required libraries
library(tidyverse)
library(car)
library(broom)
library(MASS)
library(ggplot2)
library(gridExtra)

# Read the data
hotel_data <- read.csv("hotel.csv")

# Remove the first unnamed column (index)
hotel_data <- hotel_data[, -1]

# Build initial full model with all predictors
full_model <- lm(guest_satisfaction ~ ., data = hotel_data)

# Display model summary
cat("=== FULL MODEL SUMMARY ===\n")
summary(full_model)

# Extract model statistics
model_stats <- glance(full_model)
cat("\nModel R-squared:", round(model_stats$r.squared, 4))
cat("\nAdjusted R-squared:", round(model_stats$adj.r.squared, 4), "\n")

# Check for multicollinearity using VIF
cat("\n=== VARIANCE INFLATION FACTORS (VIF) ===\n")
vif_values <- vif(full_model)
vif_df <- data.frame(
  variable = names(vif_values),
  VIF = vif_values
) %>%
  arrange(desc(VIF))
print(vif_df)

# Identify variables with high multicollinearity (VIF > 5)
high_vif <- vif_df %>% filter(VIF > 5)
if(nrow(high_vif) > 0) {
  cat("\nVariables with high multicollinearity (VIF > 5):\n")
  print(high_vif)
}

# Perform stepwise regression (backward selection)
cat("\n=== STEPWISE REGRESSION (BACKWARD SELECTION) ===\n")
step_model <- step(full_model, direction = "backward", trace = 0)
summary(step_model)

# Compare models
cat("\n=== MODEL COMPARISON ===\n")
cat("Full Model AIC:", AIC(full_model), "\n")
cat("Stepwise Model AIC:", AIC(step_model), "\n")

# Calculate standardized coefficients for comparison
# Standardize all variables
hotel_data_std <- as.data.frame(scale(hotel_data))
std_model <- lm(guest_satisfaction ~ ., data = hotel_data_std)

# Extract standardized coefficients
std_coef <- data.frame(
  variable = names(coef(std_model))[-1],
  std_coefficient = coef(std_model)[-1]
) %>%
  mutate(abs_std_coef = abs(std_coefficient)) %>%
  arrange(desc(abs_std_coef))

cat("\n=== STANDARDIZED COEFFICIENTS (Effect Size) ===\n")
cat("Top 10 most important factors by standardized coefficient:\n")
print(head(std_coef, 10))

# Visualization 1: Coefficient plot with confidence intervals
coef_data <- tidy(step_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = reorder(term, estimate)
  )

p1 <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Regression Coefficients with 95% Confidence Intervals",
       subtitle = "Stepwise Model Results",
       x = "Coefficient Estimate",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(p1)

# Visualization 2: Standardized coefficients (effect size)
std_coef_plot <- head(std_coef, 10)
p2 <- ggplot(std_coef_plot, aes(x = reorder(variable, abs_std_coef), 
                                y = std_coefficient,
                                fill = std_coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive"),
                    name = "Effect Direction") +
  labs(title = "Top 10 Most Important Factors (Standardized Coefficients)",
       subtitle = "Larger absolute values indicate stronger effects",
       x = "Variable",
       y = "Standardized Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(std_coefficient, 3)), 
            hjust = ifelse(std_coef_plot$std_coefficient > 0, -0.1, 1.1),
            size = 3.5)

print(p2)

# Model diagnostics
par(mfrow = c(2, 2))
plot(step_model)
par(mfrow = c(1, 1))

# Calculate relative importance - only if package is available
if(require(relaimpo, quietly = TRUE)) {
  calc.relimp(step_model, type = c("lmg"))
  relimp_results <- calc.relimp(step_model, type = c("lmg"))
  
  # Plot relative importance
  relimp_df <- data.frame(
    variable = names(relimp_results$lmg),
    importance = relimp_results$lmg * 100
  ) %>%
    arrange(desc(importance))
  
  p4 <- ggplot(head(relimp_df, 10), aes(x = reorder(variable, importance), y = importance)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    labs(title = "Relative Importance of Predictors",
         subtitle = "Percentage of R² explained by each predictor",
         x = "Variable",
         y = "Relative Importance (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_text(aes(label = paste0(round(importance, 1), "%")), 
              hjust = -0.1, size = 3.5)
  
  print(p4)
} else {
  cat("\nNote: Install 'relaimpo' package for relative importance analysis\n")
}

# Final summary
cat("\n=== FINAL MODEL SUMMARY ===\n")
cat("Total predictors in final model:", length(coef(step_model)) - 1, "\n")
cat("Model R-squared:", round(summary(step_model)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(step_model)$adj.r.squared, 4), "\n")
cat("\nTop 5 Key Drivers (by effect size):\n")
# Show top drivers by standardized coefficient instead
print(head(std_coef[, c("variable", "std_coefficient", "abs_std_coef")], 5))