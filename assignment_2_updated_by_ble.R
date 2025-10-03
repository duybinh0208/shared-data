# Import library
library(dplyr)
library(tidyverse)
library(imputeTS)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(scales)
library(car)
library(broom)
library(MASS)


# Part A: Data pre-processing
# Read data
hotel_bookings <- read.csv("hotel_bookings.csv")

# Check if there is duplication
total_dup_rows <- sum(duplicated(hotel_bookings))
print(sprintf("Total dup rows detected = %s", total_dup_rows))

# Remove duplication
hotel_bookings_nodup <- distinct(hotel_bookings)
total_dup_rows <- sum(duplicated(hotel_bookings_nodup))
print(sprintf("Total dup rows after dedup = %s, data are good now", total_dup_rows))

# Check if there are any abnormal/missing data
summary(hotel_bookings_nodup)

# Replace N/A children with 0
hotel_bookings_nodup$children[is.na(hotel_bookings_nodup$children)] <- 0

# Replace adult = 0 with median (not use average as decimal doesn't make sense for person)
hotel_bookings_nodup$adults[hotel_bookings_nodup$adults == 0] <- median(hotel_bookings_nodup$adults[hotel_bookings_nodup$adults > 0], na.rm = TRUE)

# Remove invalid records with adr < 0 OR not cancelled but adr = 0
hotel_bookings_nodup <- hotel_bookings_nodup[
  !(
    (hotel_bookings_nodup$is_canceled == 0 & hotel_bookings_nodup$adr == 0) # not cancelled but ADR = 0
    | (hotel_bookings_nodup$adr < 0) # negative ADR
  ),
]

View(hotel_bookings_nodup)

# Remove outlier
adr_bounds <- quantile(hotel_bookings_nodup$adr, probs = c(0.025, 0.975), na.rm = TRUE)

hotel_bookings_nodup <- hotel_bookings_nodup[
  hotel_bookings_nodup$adr >= adr_bounds[1] & hotel_bookings_nodup$adr <= adr_bounds[2],
]


# Create data frame that only contain numeric columns
hotel_bookings_numeric_raw <- hotel_bookings_nodup[sapply(hotel_bookings_nodup, is.numeric)]
View(hotel_bookings_numeric_raw)

# Normalize numeric columns for analysis
hotel_bookings_numeric_scaled <- as.data.frame(scale(hotel_bookings_numeric_raw))
View(hotel_bookings_numeric_scaled)


# Visualize distribution


# 1. Distribution of cancellations
ggplot(hotel_bookings_nodup, aes(x = factor(is_canceled,
  levels = c(0, 1),
  labels = c("Active", "Cancelled")
))) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Reservation Type", y = "Count",
    title = "Distribution of Active vs Cancelled Reservations"
  )


# 2. ADR distribution

ggplot(hotel_bookings_nodup, aes(x = adr)) +
  geom_histogram(bins = 50, fill = "orange", color = "black") +
  labs(
    title = "Distribution of ADR (Raw Values)",
    x = "ADR (€)", y = "Count"
  )

ggplot(hotel_bookings_numeric_scaled, aes(x = adr)) +
  geom_histogram(bins = 50, fill = "orange", color = "black") +
  labs(
    title = "Distribution of ADR (Normalized)",
    x = "ADR (€)", y = "Count"
  )


boxplot(hotel_bookings_nodup$adr,
  main = "Distribution of ADR (Boxplot)",
  ylab = "ADR (€)",
  col = "lightblue",
  outline = TRUE
) # Show outlier points

# 3. Hotel types
# Compute percentages directly from hotel_bookings_nodup
hotel_type_dist <- hotel_bookings_nodup %>%
  count(hotel) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1),
    label = paste0(perc, "%")
  )

# Plot with percentages
ggplot(hotel_type_dist, aes(x = "", y = n, fill = hotel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 5
  ) +
  labs(title = "Distribution of Bookings by Hotel Type", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())

# 4. Market segment
ggplot(hotel_bookings_nodup, aes(x = market_segment)) +
  geom_bar(fill = "purple") +
  labs(title = "Bookings by Market Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Bookings by month
ggplot(
  hotel_bookings_nodup,
  aes(x = factor(arrival_date_month, levels = month.name))
) +
  geom_bar(fill = "skyblue") +
  labs(title = "Bookings by Arrival Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6. Lead time distribution
ggplot(hotel_bookings_nodup, aes(x = lead_time)) +
  geom_histogram(bins = 50, fill = "red", color = "black") +
  labs(title = "Distribution of Lead Time")

# 7. Create total nights column
hotel_bookings_nodup$total_nights <- hotel_bookings_nodup$stays_in_weekend_nights +
  hotel_bookings_nodup$stays_in_week_nights

# For active bookings, there must be at least 1 total nights, even checkout in same date
hotel_bookings_nodup <- hotel_bookings_nodup %>%
  mutate(
    total_nights = ifelse(is_canceled == 0 & total_nights == 0, 1, total_nights)
  )

# Histogram of total nights
ggplot(hotel_bookings_nodup, aes(x = total_nights)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(
    title = "Distribution of Total Nights Stayed",
    x = "Total Nights", y = "Count"
  ) +
  coord_cartesian(xlim = c(0, 30))

# PART B
# Correlation

# Compute correlations with cancellation
correlations_cancel <- cor(
  hotel_bookings_numeric_scaled[, -which(names(hotel_bookings_numeric_scaled) == "is_canceled")], # (exclude is_cancelled itself)
  hotel_bookings_numeric_scaled$is_canceled,
  use = "complete.obs"
)

# Create correlation results dataframe
cor_df_cancel <- data.frame(
  variable = rownames(correlations_cancel),
  correlation = correlations_cancel[, 1]
) %>%
  arrange(desc(abs(correlation)))

# Get top 10 factors by absolute correlation
top_10_cancel <- cor_df_cancel %>% head(10)

# Print the top 10 factors
cat("Top 10 Factors Correlated with Cancellations:\n")
print(top_10_cancel)

# Visualization: Bar plot of top 10 factors correlated with cancellations
p_cancel <- ggplot(
  top_10_cancel,
  aes(
    x = reorder(variable, abs(correlation)),
    y = correlation,
    fill = correlation > 0
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c("FALSE" = "coral", "TRUE" = "steelblue"),
    labels = c("Negative", "Positive"),
    name = "Correlation Type"
  ) +
  labs(
    title = "Top 10 Factors Correlated with Cancellations",
    x = "Variables",
    y = "Correlation Coefficient",
    subtitle = "Ordered by absolute correlation strength"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(
    aes(
      label = round(correlation, 3),
      hjust = ifelse(correlation > 0, -0.1, 1.1)
    ),
    size = 3.5
  )

print(p_cancel)


# Compute correlations with ADR
correlations_adr <- cor(
  hotel_bookings_numeric_scaled[, -which(names(hotel_bookings_numeric_scaled) == "adr")], # (exclude adr itself)
  hotel_bookings_numeric_scaled$adr,
  use = "complete.obs"
)

# Create a data frame with ADR correlation results
cor_df_adr <- data.frame(
  variable = rownames(correlations_adr),
  correlation = correlations_adr[, 1]
) %>%
  arrange(desc(abs(correlation)))

# Get top 10 factors by absolute correlation
top_10_adr <- cor_df_adr %>% head(10)

# Print the top 10 factors
cat("Top 10 Factors Correlated with ADR:\n")
print(top_10_adr)

# Visualization: Bar plot of top 10 factors correlated with ADR

top_10_adr_plot <- top_10_adr %>%
  dplyr::mutate(
    corr_sign = factor(
      ifelse(correlation >= 0, "Positive", "Negative"),
      levels = c("Negative", "Positive")
    )
  )

p_adr <- ggplot(
  top_10_adr_plot,
  aes(
    x = reorder(variable, abs(correlation)),
    y = correlation,
    fill = corr_sign
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(Negative = "coral", Positive = "steelblue"),
    name = "Correlation Type"
  ) + # <- no drop=FALSE
  labs(
    title = "Top 10 Factors Correlated with ADR",
    x = "Variables",
    y = "Correlation Coefficient",
    subtitle = "Ordered by absolute correlation strength"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(
    aes(
      label = round(correlation, 3),
      hjust = ifelse(correlation > 0, -0.1, 1.1)
    ),
    size = 3.5
  )

print(p_adr)


# Regression Analysis
# ===== Cancellation: REGRESSION =====
# Convert required_car_parking_spaces to binary yes/no
hotel_bookings_numeric_raw$required_car_parking_spaces <- ifelse(
  hotel_bookings_numeric_raw$required_car_parking_spaces > 0, 1, 0
)


# Convert is_canceled to factor directly in the dataset
hotel_bookings_numeric_raw$is_canceled <- factor(hotel_bookings_numeric_raw$is_canceled,
  levels = c(0, 1)
)
# Logistic regression (all predictors)


logit_model <- glm(is_canceled ~ .,
  data = hotel_bookings_numeric_raw,
  family = binomial
)

# Model summary (coefficients in log-odds)
summary(logit_model)


# Odds ratios
odds_ratios <- exp(coef(logit_model))

# Create a simple dataframe with just coefficients and odds ratios
impact_df <- data.frame(
  variable    = rownames(summary(logit_model)$coefficients),
  coefficient = summary(logit_model)$coefficients[, "Estimate"],
  odds_ratio  = exp(summary(logit_model)$coefficients[, "Estimate"]),
  p_value     = summary(logit_model)$coefficients[, "Pr(>|z|)"]
) %>%
  filter(variable != "(Intercept)") %>% # Remove intercept
  arrange(desc(abs(coefficient))) # Sort by absolute coefficient value

# Get top 10
top_10_impact <- impact_df %>% head(10)

# Print results
cat("Top 10 Factors with Strongest Impact on Cancellation:\n")
print(top_10_impact)



# ===== ADR: REGRESSION =====
# 1) Full model adr
full_model_adr <- lm(adr ~ ., data = hotel_bookings_numeric_scaled)

# Display model summary adr
cat("=== FULL MODEL SUMMARY ===\n")
summary(full_model_adr)

# Extract model statistics
model_stats_adr <- glance(full_model_adr)
cat("\nModel R-squared:", round(model_stats_adr$r.squared, 4))
cat("\nAdjusted R-squared:", round(model_stats_adr$adj.r.squared, 4), "\n")

# Check for multicollinearity using VIF
cat("\n=== VARIANCE INFLATION FACTORS (VIF) ===\n")
vif_values_adr <- vif(full_model_adr)
vif_df_adr <- data.frame(
  variable = names(vif_values_adr),
  VIF = vif_values_adr
) %>%
  arrange(desc(VIF))
print(vif_df_adr)

# Identify variables with high multicollinearity (VIF > 5)
high_vif_adr <- vif_df_adr %>% filter(VIF > 5)
if (nrow(high_vif_adr) > 0) {
  cat("\nVariables with high multicollinearity (VIF > 5):\n")
  print(high_vif_adr)
}

# Perform stepwise regression (backward selection)
cat("\n=== STEPWISE REGRESSION (BACKWARD SELECTION) ===\n")
step_model_adr <- step(full_model_adr, direction = "backward", trace = 0)
summary(step_model_adr)

# Compare models
cat("\n=== MODEL COMPARISON ===\n")
cat("Full Model AIC:", AIC(full_model_adr), "\n")
cat("Stepwise Model AIC:", AIC(step_model_adr), "\n")

# Calculate standardized coefficients for comparison
std_model_adr <- lm(adr ~ ., data = hotel_bookings_numeric_scaled)

# Extract standardized coefficients
std_coef_adr <- data.frame(
  variable        = names(coef(std_model_adr))[-1],
  std_coefficient = coef(std_model_adr)[-1],
  p_value         = summary(full_model_adr)$coefficients[-1, "Pr(>|t|)"]
) %>%
  mutate(abs_std_coef_adr = abs(std_coefficient)) %>%
  arrange(desc(abs_std_coef_adr))

cat("\n=== STANDARDIZED COEFFICIENTS ADR (Effect Size) ===\n")
cat("Top 10 most important factors by standardized coefficient adr:\n")
print(head(std_coef_adr, 10))

# Visualization: Standardized coefficients (effect size) for ADR
std_coef_plot_adr <- head(std_coef_adr, 10)
p2 <- ggplot(std_coef_plot_adr, aes(
  x = reorder(variable, abs_std_coef_adr),
  y = std_coefficient,
  fill = std_coefficient > 0
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c("FALSE" = "coral", "TRUE" = "steelblue"),
    labels = c("Negative", "Positive"),
    name = "Effect Direction"
  ) +
  labs(
    title = "Top 10 Most Important Factors for ADR (Standardized Coefficients)",
    subtitle = "Larger absolute values indicate stronger effects",
    x = "Variable",
    y = "Standardized Coefficient"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = round(std_coefficient, 3)),
    hjust = ifelse(std_coef_plot_adr$std_coefficient > 0, -0.1, 1.1),
    size = 3.5
  )

print(p2)


# Feature Importance for cancellation

print("=== FEATURE SELECTION: CORRELATION vs REGRESSION ===")

# ---- CANCELLATION ----
top10_corr_cancel <- head(cor_df_cancel$variable, 10)
top10_reg_cancel <- head(impact_df$variable, 10)

# Intersection, then take top 5
agreed_cancel <- intersect(top10_corr_cancel, top10_reg_cancel)[1:5]

cat("\nTop 10 by Correlation (Cancellation):\n")
print(top10_corr_cancel)
cat("\nTop 10 by Regression (Cancellation):\n")
print(top10_reg_cancel)
cat("\nTop 5 Agreed Factors (Cancellation):\n")
print(agreed_cancel)




# Feature Importance for adr

print("=== FEATURE SELECTION: ADR (Correlation vs Regression) ===")

# Take top 10 by correlation
top10_corr_adr <- head(cor_df_adr$variable, 10)

# Take top 10 by regression standardized coefficients
top10_reg_adr <- head(std_coef_adr$variable, 10)

# Intersection, then top 5
agreed_adr <- intersect(top10_corr_adr, top10_reg_adr)[1:5]

cat("\nTop 10 by Correlation (ADR):\n")
print(top10_corr_adr)

cat("\nTop 10 by Regression (ADR):\n")
print(top10_reg_adr)

cat("\nTop 5 Agreed Factors (ADR):\n")
print(agreed_adr)



# PART C: Financial impact of actionable drivers

# =====================================================
# WHAT-IF & ROI ANALYSIS FOR CANCELLATION
# =====================================================
print("=== ROI Analysis for Cancellation ===")

# Potential revenue (all bookings, canceled or not)
potential_revenue <- hotel_bookings_nodup$adr * hotel_bookings_nodup$total_nights

# Total potential revenue (if no cancellations happened)
total_potential_revenue <- sum(potential_revenue, na.rm = TRUE)

# Revenue lost from cancellations
lost_revenue_total <- sum(potential_revenue[hotel_bookings_nodup$is_canceled == 1], na.rm = TRUE)

# Actual realized revenue (only active bookings)
actual_revenue <- sum(potential_revenue[hotel_bookings_nodup$is_canceled == 0], na.rm = TRUE)

# Print results
cat("Total potential revenue (no cancellations): $", format(round(total_potential_revenue, 0), big.mark = ","), "\n")
cat("Revenue lost due to cancellations:        $", format(round(lost_revenue_total, 0), big.mark = ","), "\n")
cat("Actual realized revenue:                  $", format(round(actual_revenue, 0), big.mark = ","), "\n")

# -----------------------
# 1. Special Requests
# -----------------------
cancel_lowreq <- mean(subset(hotel_bookings_nodup, total_of_special_requests == 0)$is_canceled)
cancel_highreq <- mean(subset(hotel_bookings_nodup, total_of_special_requests >= 3)$is_canceled)

improvement_requests <- cancel_lowreq - cancel_highreq
expected_savings_requests <- improvement_requests * sum(potential_revenue, na.rm = TRUE)

# Assume $10 handling cost per booking for extra requests (20% adoption)
investment_requests <- nrow(hotel_bookings_nodup) * 0.2 * 10
ROI_requests <- (expected_savings_requests - investment_requests) / investment_requests * 100

# -----------------------
# 2. Booking Changes
# -----------------------
cancel_nochange <- mean(subset(hotel_bookings_nodup, booking_changes == 0)$is_canceled)
cancel_withchange <- mean(subset(hotel_bookings_nodup, booking_changes > 0)$is_canceled)

improvement_changes <- cancel_withchange - cancel_nochange
expected_savings_changes <- improvement_changes * sum(potential_revenue, na.rm = TRUE)

# Assume $15 admin cost per booking to better manage changes (15% adoption)
investment_changes <- nrow(hotel_bookings_nodup) * 0.15 * 15
ROI_changes <- (expected_savings_changes - investment_changes) / investment_changes * 100

# -----------------------
# 3. Adults (Solo/Couple Focus Policy)
# -----------------------
cancel_solo_couple <- mean(subset(hotel_bookings_nodup, adults <= 2)$is_canceled)
cancel_groups <- mean(subset(hotel_bookings_nodup, adults > 2)$is_canceled)

# Improvement is the reduction in cancellations when targeting solos/couples
improvement_adults <- cancel_groups - cancel_solo_couple
expected_savings_adults <- improvement_adults * sum(potential_revenue, na.rm = TRUE)

# Assume $20 marketing cost per booking to promote solo/couple offers (10% adoption)
investment_adults <- nrow(hotel_bookings_nodup) * 0.1 * 20
ROI_adults <- (expected_savings_adults - investment_adults) / investment_adults * 100


# -----------------------
# ROI Summary
# -----------------------
roi_summary <- data.frame(
  Factor = c("Special Requests", "Booking Changes", "Adults (Solo/Couple Focus)"),
  CancelRateDiff = round(c(improvement_requests, improvement_changes, improvement_adults) * 100, 2),
  Expected_Savings = round(c(expected_savings_requests, expected_savings_changes, expected_savings_adults), 0),
  Investment = round(c(investment_requests, investment_changes, investment_adults), 0),
  ROI_percent = round(c(ROI_requests, ROI_changes, ROI_adults), 1),
  PercentLostRevenueSaved = round(c(expected_savings_requests, expected_savings_changes, expected_savings_adults) / lost_revenue_total * 100, 2)
)

print("=== WHAT-IF & ROI RESULTS FOR CANCELLATION ===")
print(roi_summary)


# =====================================================
# WHAT-IF & ROI ANALYSIS FOR ADR
# =====================================================

# -----------------------
# 1. Special Requests
# -----------------------
adr_lowreq <- mean(subset(hotel_bookings_nodup, total_of_special_requests == 0)$adr, na.rm = TRUE)
adr_highreq <- mean(subset(hotel_bookings_nodup, total_of_special_requests >= 3)$adr, na.rm = TRUE)

improvement_requests_adr <- adr_highreq - adr_lowreq
expected_gain_requests <- improvement_requests_adr * nrow(hotel_bookings_nodup)

# Assume $10 handling cost per booking (20% adoption)
investment_requests_adr <- nrow(hotel_bookings_nodup) * 0.2 * 10
ROI_requests_adr <- (expected_gain_requests - investment_requests_adr) / investment_requests_adr * 100

# -----------------------
# 2. Repeat Guests
# -----------------------
adr_new <- mean(subset(hotel_bookings_nodup, is_repeated_guest == 0)$adr, na.rm = TRUE)
adr_repeat <- mean(subset(hotel_bookings_nodup, is_repeated_guest == 1)$adr, na.rm = TRUE)

improvement_repeat_adr <- adr_repeat - adr_new
expected_gain_repeat <- improvement_repeat_adr * nrow(hotel_bookings_nodup)

# Assume $30 loyalty perk cost per guest (10% adoption)
investment_repeat_adr <- nrow(hotel_bookings_nodup) * 0.1 * 30
ROI_repeat_adr <- (expected_gain_repeat - investment_repeat_adr) / investment_repeat_adr * 100

# -----------------------
# 3. Families with Children (Family Package Opportunity)
# -----------------------
adr_nochild <- mean(subset(hotel_bookings_nodup, children == 0)$adr, na.rm = TRUE)
adr_child <- mean(subset(hotel_bookings_nodup, children > 0)$adr, na.rm = TRUE)

improvement_child_adr <- adr_child - adr_nochild
expected_gain_child <- improvement_child_adr * nrow(hotel_bookings_nodup)

# Assume $50 family package promotion cost (5% adoption)
investment_child_adr <- nrow(hotel_bookings_nodup) * 0.05 * 50
ROI_child_adr <- (expected_gain_child - investment_child_adr) / investment_child_adr * 100

# -----------------------
# ROI Summary
# -----------------------
roi_summary_adr <- data.frame(
  Factor = c(
    "Special Requests",
    "Repeat Guests (Loyalty Program)",
    "Families with Children (Family Package Opportunity)"
  ),
  ADR_Difference = round(c(improvement_requests_adr, improvement_repeat_adr, improvement_child_adr), 2),
  Expected_Gain = round(c(expected_gain_requests, expected_gain_repeat, expected_gain_child), 0),
  Investment = round(c(investment_requests_adr, investment_repeat_adr, investment_child_adr), 0),
  ROI_percent = round(c(ROI_requests_adr, ROI_repeat_adr, ROI_child_adr), 1)
)

print("=== WHAT-IF & ROI RESULTS FOR ADR DRIVERS ===")
print(roi_summary_adr)
