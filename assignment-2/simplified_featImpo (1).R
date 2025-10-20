# =====================================================
# WHAT DRIVES GUEST SATISFACTION?
# Simplified Analysis for Business Students
# =====================================================

# STEP 1: SETUP
# ---------------------------------------------
# Load our tools
library(tidyverse)    # For data analysis
library(randomForest) # For importance analysis
library(ggplot2)      # For charts

# Read the hotel data
hotel_data <- read.csv("hotel.csv")
hotel_data <- hotel_data[, -1]  # Remove index column

# =====================================================
# UNDERSTANDING FEATURE IMPORTANCE
# =====================================================

print("=== WHAT IS FEATURE IMPORTANCE? ===")
print("")
print("Feature importance tells us which factors have the biggest")
print("impact on guest satisfaction. Think of it like this:")
print("- High importance = This factor strongly affects satisfaction")
print("- Low importance = This factor has little effect on satisfaction")
print("")
print("We'll use two simple methods to find the most important factors.")
print("")

# =====================================================
# METHOD 1: CORRELATION ANALYSIS (SIMPLEST)
# =====================================================

print("=== METHOD 1: SIMPLE CORRELATION ANALYSIS ===")
print("")
print("Correlation measures how two things move together:")
print("- Positive correlation: When X goes up, Y goes up")
print("- Negative correlation: When X goes up, Y goes down")
print("- Scale: -1 (perfect negative) to +1 (perfect positive)")
print("")

# Calculate correlations with guest satisfaction
correlations <- cor(hotel_data[, -ncol(hotel_data)], hotel_data$guest_satisfaction)

# Create a nice table of results
correlation_results <- data.frame(
  Factor = rownames(correlations),
  Correlation_with_Satisfaction = round(correlations[,1], 3),
  Direction = ifelse(correlations[,1] > 0, "Positive", "Negative"),
  Strength = case_when(
    abs(correlations[,1]) >= 0.7 ~ "Very Strong",
    abs(correlations[,1]) >= 0.5 ~ "Strong",
    abs(correlations[,1]) >= 0.3 ~ "Moderate",
    abs(correlations[,1]) >= 0.1 ~ "Weak",
    TRUE ~ "Very Weak"
  )
) %>%
  arrange(desc(abs(Correlation_with_Satisfaction)))

print("CORRELATION RESULTS (Sorted by Importance):")
print(correlation_results)
print("")

# Create a simple bar chart
correlation_plot <- ggplot(head(correlation_results, 10), 
                           aes(x = reorder(Factor, abs(Correlation_with_Satisfaction)), 
                               y = Correlation_with_Satisfaction,
                               fill = Direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue")) +
  labs(title = "Top 10 Factors Affecting Guest Satisfaction",
       subtitle = "Based on correlation analysis",
       x = "Hotel Feature",
       y = "Correlation with Satisfaction",
       fill = "Effect Direction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(correlation_plot)

# Business interpretation
print("WHAT THIS MEANS FOR YOUR HOTEL:")
print("")
print("POSITIVE CORRELATIONS (Blue bars):")
print("- These features INCREASE satisfaction when improved")
print("- Focus on maximizing these")
print("")
print("NEGATIVE CORRELATIONS (Red bars):")
print("- These features DECREASE satisfaction when they increase")
print("- Focus on minimizing these (like noise level)")
print("")

# =====================================================
# METHOD 2: DECISION TREE ANALYSIS (RANDOM FOREST)
# =====================================================

print("=== METHOD 2: DECISION TREE ANALYSIS ===")
print("")
print("This method uses many 'decision trees' to figure out importance.")
print("Think of it like asking 500 hotel experts to rank what matters most,")
print("then averaging their opinions.")
print("")

# Train a simple Random Forest model
set.seed(123)  # Ensures we get the same results each time
importance_model <- randomForest(
  guest_satisfaction ~ .,  # Predict satisfaction using all other factors
  data = hotel_data,
  ntree = 100,  # Use 100 decision trees (like 100 expert opinions)
  importance = TRUE
)

# Extract importance scores
importance_scores <- importance(importance_model)

# Create a nice table
importance_results <- data.frame(
  Factor = rownames(importance_scores),
  Importance_Score = round(importance_scores[, "%IncMSE"], 2)
) %>%
  mutate(
    Relative_Importance = round((Importance_Score / max(Importance_Score)) * 100, 1)
  ) %>%
  arrange(desc(Importance_Score))

print("IMPORTANCE SCORES (Higher = More Important):")
print(head(importance_results, 10))
print("")

# Create visualization
importance_plot <- ggplot(head(importance_results, 10),
                          aes(x = reorder(Factor, Importance_Score),
                              y = Relative_Importance)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Feature Importance from Decision Tree Analysis",
       subtitle = "Relative importance (most important = 100)",
       x = "Hotel Feature",
       y = "Relative Importance (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(importance_plot)

# =====================================================
# COMPARING THE TWO METHODS
# =====================================================

print("=== COMPARING BOTH METHODS ===")

# Get top 5 from each method
top5_correlation <- head(correlation_results$Factor, 5)
top5_importance <- head(importance_results$Factor, 5)

# Find which factors appear in both top 5 lists
agreed_factors <- intersect(top5_correlation, top5_importance)

print("Top 5 factors by Correlation:")
print(top5_correlation)
print("")
print("Top 5 factors by Decision Trees:")
print(top5_importance)
print("")
print("Factors that appear in BOTH top 5 lists:")
print(agreed_factors)
print("")

# =====================================================
# BUSINESS RECOMMENDATIONS
# =====================================================

print("=== ACTIONABLE BUSINESS INSIGHTS ===")
print("")

# Identify the top positive and negative factors
top_positive <- correlation_results %>%
  filter(Direction == "Positive") %>%
  head(3)

top_negative <- correlation_results %>%
  filter(Direction == "Negative") %>%
  head(3)

print("TOP 3 SATISFACTION BOOSTERS:")
for(i in 1:nrow(top_positive)) {
  factor_name <- top_positive$Factor[i]
  correlation <- top_positive$Correlation_with_Satisfaction[i]
  
  # Provide specific recommendations
  recommendation <- case_when(
    factor_name == "staff_per_room" ~ "Increase staff-to-room ratio",
    factor_name == "gym_quality" ~ "Upgrade gym equipment and facilities",
    factor_name == "room_service" ~ "Expand room service options",
    factor_name == "wifi_free" ~ "Offer free WiFi in all rooms",
    factor_name == "response_to_reviews" ~ "Respond to all guest reviews",
    TRUE ~ paste("Improve", factor_name)
  )
  
  print(paste(i, ". ", factor_name, " (correlation: ", correlation, ")", sep = ""))
  print(paste("   Action: ", recommendation, sep = ""))
}

print("")
print("TOP 3 SATISFACTION KILLERS:")
for(i in 1:nrow(top_negative)) {
  factor_name <- top_negative$Factor[i]
  correlation <- top_negative$Correlation_with_Satisfaction[i]
  
  # Provide specific recommendations
  recommendation <- case_when(
    factor_name == "noise_level" ~ "Invest in soundproofing",
    factor_name == "minibar_markup_pct" ~ "Reduce minibar prices",
    factor_name == "cancellation_fee_pct" ~ "Make cancellation policy more flexible",
    TRUE ~ paste("Reduce", factor_name)
  )
  
  print(paste(i, ". ", factor_name, " (correlation: ", correlation, ")", sep = ""))
  print(paste("   Action: ", recommendation, sep = ""))
}

# =====================================================
# PRIORITY MATRIX
# =====================================================

print("")
print("=== IMPROVEMENT PRIORITY MATRIX ===")

# Create a simple priority matrix
priority_matrix <- data.frame(
  Factor = c("Staff Ratio", "Noise Level", "Free WiFi", 
             "Gym Quality", "Room Service", "Minibar Prices"),
  Impact_on_Satisfaction = c("High", "High", "Medium", 
                             "Low", "Medium", "Low"),
  Implementation_Cost = c("High", "High", "Low", 
                          "Medium", "Medium", "Low"),
  Priority = c("Long-term", "Long-term", "Quick Win", 
               "Low Priority", "Consider", "Quick Win")
)

print(priority_matrix)
print("")
print("QUICK WINS: High impact, low cost - implement immediately")
print("LONG-TERM: High impact, high cost - plan for future")
print("CONSIDER: Medium impact - implement if resources allow")
print("LOW PRIORITY: Low impact - focus on other areas first")

# =====================================================
# SAVE RESULTS
# =====================================================

# Create a summary report
summary_report <- data.frame(
  Analysis_Type = c("Correlation", "Decision Trees"),
  Top_Factor = c(correlation_results$Factor[1], importance_results$Factor[1]),
  Second_Factor = c(correlation_results$Factor[2], importance_results$Factor[2]),
  Third_Factor = c(correlation_results$Factor[3], importance_results$Factor[3])
)

# Save results
write.csv(correlation_results, "simple_correlation_analysis.csv", row.names = FALSE)
write.csv(head(importance_results, 10), "simple_importance_scores.csv", row.names = FALSE)

print("")
print("=== ANALYSIS COMPLETE ===")
print("Results saved to:")
print("- simple_correlation_analysis.csv")
print("- simple_importance_scores.csv")
print("")
print("Remember: Focus on the factors that BOTH methods agree are important!")