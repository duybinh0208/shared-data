# Load required libraries
library(tidyverse)
library(corrplot)
library(ggplot2)

# Read the data
hotel_data <- read.csv("hotel.csv")

# Remove the first unnamed column (appears to be an index)
hotel_data <- hotel_data[, -1]

# Calculate correlations with guest_satisfaction
# Remove guest_satisfaction itself from the correlation calculation
correlations <- cor(hotel_data[, -ncol(hotel_data)], 
                    hotel_data$guest_satisfaction, 
                    use = "complete.obs")

# Create a data frame with correlation results
cor_df <- data.frame(
  variable = rownames(correlations),
  correlation = correlations[, 1]
) %>%
  arrange(desc(abs(correlation)))

# Get top 5 factors by absolute correlation
top_5_factors <- cor_df %>%
  head(5)

# Print the top 5 factors
cat("Top 5 Factors Correlated with Guest Satisfaction:\n")
print(top_5_factors)

# Visualization 1: Bar plot of top 5 factors
p1 <- ggplot(top_5_factors, aes(x = reorder(variable, abs(correlation)), 
                                y = correlation, 
                                fill = correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive"),
                    name = "Correlation Type") +
  labs(title = "Top 5 Factors Correlated with Guest Satisfaction",
       x = "Variables",
       y = "Correlation Coefficient",
       subtitle = "Ordered by absolute correlation strength") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(correlation, 3)), 
            hjust = ifelse(top_5_factors$correlation > 0, -0.1, 1.1),
            size = 3.5)

print(p1)

# Visualization 2: Correlation matrix heatmap of top factors + guest_satisfaction
top_vars <- c(top_5_factors$variable, "guest_satisfaction")
cor_matrix <- cor(hotel_data[, top_vars], use = "complete.obs")

# Create a more detailed correlation plot
par(mfrow = c(1, 1))
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("darkred", "white", "darkblue"))(100),
         title = "Correlation Matrix: Top 5 Factors & Guest Satisfaction",
         mar = c(0, 0, 2, 0))

# Visualization 3: Scatter plots of top 3 factors vs guest satisfaction
top_3 <- top_5_factors$variable[1:3]

# Create scatter plots for top 3 factors
plots <- list()
for(i in 1:3) {
  var_name <- top_3[i]
  cor_val <- top_5_factors$correlation[i]
  
  p <- ggplot(hotel_data, aes_string(x = var_name, y = "guest_satisfaction")) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste(var_name, "vs Guest Satisfaction"),
         subtitle = paste("Correlation:", round(cor_val, 3)),
         x = gsub("_", " ", str_to_title(var_name)),
         y = "Guest Satisfaction") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  plots[[i]] <- p
}

# Arrange the scatter plots
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2, 
             top = "Relationship between Top 3 Factors and Guest Satisfaction")

# Additional analysis: Show all correlations in descending order
cat("\n\nAll Variables Ranked by Correlation with Guest Satisfaction:\n")
print(cor_df %>% mutate(correlation = round(correlation, 4)))

# Create a comprehensive correlation plot for all variables
p4 <- ggplot(cor_df, aes(x = reorder(variable, correlation), 
                         y = correlation,
                         fill = correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive"),
                    name = "Correlation Type") +
  labs(title = "All Factors Correlated with Guest Satisfaction",
       x = "Variables",
       y = "Correlation Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

print(p4)