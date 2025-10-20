# =====================================================
# WEEK 8 REVIEW EXERCISES - HOTEL DATA ANALYSIS
# For Business Analytics Students
# =====================================================

# INSTRUCTIONS:
# Work through each exercise step by step
# Most code is started for you - just fill in the blanks
# Focus on understanding what the results mean for business

# -----------------------------------------------------
# SETUP (Run this first)
# -----------------------------------------------------
library(tidyverse)
library(ggplot2)

# Load the hotel data
hotel_data <- read.csv("hotel.csv")
hotel_data <- hotel_data[, -1]  # Remove index column

print("Data loaded successfully! You have data for 300 hotels.")

# -----------------------------------------------------
# EXERCISE 1: QUICK DATA CHECK (10 minutes)
# -----------------------------------------------------
# Your manager asks: "Give me a 2-minute overview of our hotels"

# Q1.1: What is the average guest satisfaction score?
# Hint: use mean() function
avg_satisfaction <- mean(hotel_data$guest_satisfaction)

# Q1.2: What percentage of hotels offer free WiFi?
# Hint: wifi_free is 1 for yes, 0 for no
hotels_with_wifi <- sum(hotel_data$wifi_free == 1)
total_hotels <- nrow(hotel_data)
pct_free_wifi <- (hotels_with_wifi / total_hotels) * 100

# Q1.3: What's the average room rate?
avg_room_rate <- mean(hotel_data$avg_room_rate)

# Print your findings
print(paste("Average satisfaction score:", round(avg_satisfaction, 2), "out of 5"))
print(paste("Hotels with free WiFi:", round(pct_free_wifi, 1), "%"))
print(paste("Average room rate: $", round(avg_room_rate, 2)))

# Q1.4: Create a simple bar chart showing these three metrics
# This code is complete - just run it!
summary_data <- data.frame(
  Metric = c("Satisfaction\n(out of 5)", "Free WiFi\n(% of hotels)", "Avg Rate\n($ divided by 50)"),
  Value = c(avg_satisfaction, pct_free_wifi/20, avg_room_rate/50)  # Scaled for comparison
)

ggplot(summary_data, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  labs(title = "Hotel Overview Dashboard",
       subtitle = "Key metrics at a glance",
       y = "Scaled Value") +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------------------------
# EXERCISE 2: FINDING KEY DRIVERS (15 minutes)  
# -----------------------------------------------------
# Your manager asks: "What makes guests happy?"

# Q2.1: Check correlation between staff_per_room and guest_satisfaction
# Fill in the correlation calculation
correlation_staff <- cor(hotel_data$staff_per_room, hotel_data$guest_satisfaction)
print(paste("Staff correlation with satisfaction:", round(correlation_staff, 3)))

# Q2.2: Check correlation between noise_level and guest_satisfaction
correlation_noise <- cor(hotel_data$noise_level, hotel_data$guest_satisfaction)
print(paste("Noise correlation with satisfaction:", round(correlation_noise, 3)))

# Q2.3: Which factor is MORE important? (Complete the if statement)
if(abs(correlation_staff) > abs(correlation_noise)) {
  print("Staff ratio is more important than noise level")
} else {
  print("Noise level is more important than staff ratio")
}

# Q2.4: Create a scatter plot to visualize the strongest relationship
# This code is mostly complete - just run it!
ggplot(hotel_data, aes(x = staff_per_room, y = guest_satisfaction)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "More Staff = Happier Guests",
       subtitle = paste("Correlation:", round(correlation_staff, 3)),
       x = "Staff per Room",
       y = "Guest Satisfaction") +
  theme_minimal()

# Business Question: Should we hire more staff? (Answer as comment)
# YOUR ANSWER: 

# -----------------------------------------------------
# EXERCISE 3: SIMPLE PREDICTION (10 minutes)
# -----------------------------------------------------
# Can we predict guest satisfaction using just 2 factors?

# Q3.1: Build a simple model using staff_per_room and noise_level
# Complete the model formula
simple_model <- lm(guest_satisfaction ~ staff_per_room + noise_level, 
                   data = hotel_data)

# Look at the results
summary(simple_model)

# Q3.2: What do the results tell us?
# The R-squared value tells us how well our model predicts satisfaction
r_squared <- summary(simple_model)$r.squared
print(paste("Our model explains", round(r_squared * 100, 1), "% of satisfaction scores"))

# Q3.3: Make a prediction for a new hotel
# Hotel A has: 0.6 staff per room, noise level of 2.5
new_hotel <- data.frame(
  staff_per_room = 0.6,
  noise_level = 2.5
)

predicted_satisfaction <- predict(simple_model, new_hotel)
print(paste("Predicted satisfaction for Hotel A:", round(predicted_satisfaction, 2)))

# Is this good or bad? (The average is 3.5)
if(predicted_satisfaction > 3.5) {
  print("This hotel should perform ABOVE average")
} else {
  print("This hotel should perform BELOW average")
}

# -----------------------------------------------------
# EXERCISE 4: BUSINESS CASE - ROI CALCULATION (15 minutes)
# -----------------------------------------------------
# Your hotel: 150 rooms, $120 average rate, 70% occupancy

# Q4.1: Calculate current annual revenue
rooms <- 150
occupancy_rate <- 0.70
room_rate <- 120
days_in_year <- 365

# Fill in the calculation
rooms_sold_per_day <- rooms * occupancy_rate
daily_revenue <- rooms_sold_per_day * room_rate
annual_revenue <- daily_revenue * days_in_year

print(paste("Current annual revenue: $", format(annual_revenue, big.mark=",")))

# Q4.2: OPTION A - Add Free WiFi
# Cost: $30,000 setup + $15,000 per year
# Expected benefit: 3% revenue increase

wifi_setup_cost <- 30000
wifi_annual_cost <- 15000
revenue_increase_percent <- 0.03  # 3%

# Calculate the benefit
additional_revenue <- annual_revenue * revenue_increase_percent
first_year_profit <- additional_revenue - wifi_setup_cost - wifi_annual_cost
second_year_profit <- additional_revenue - wifi_annual_cost  # No setup cost

print("=== WiFi Investment Analysis ===")
print(paste("Year 1 profit:", "$", format(first_year_profit, big.mark=",")))
print(paste("Year 2 profit:", "$", format(second_year_profit, big.mark=",")))

# Q4.3: OPTION B - Hire 2 More Staff
# Cost: $70,000 per year (salary + benefits)
# Expected benefit: 4% revenue increase

staff_annual_cost <- 70000
revenue_increase_staff <- 0.04  # 4%

# Calculate the benefit
additional_revenue_staff <- annual_revenue * revenue_increase_staff
annual_profit_staff <- additional_revenue_staff - staff_annual_cost

print("=== Staff Investment Analysis ===")
print(paste("Annual profit from more staff:", "$", format(annual_profit_staff, big.mark=",")))

# Q4.4: Which option is better?
print("=== RECOMMENDATION ===")
if(second_year_profit > annual_profit_staff) {
  print("Invest in WiFi - better long-term return")
} else {
  print("Hire more staff - better returns")
}

# -----------------------------------------------------
# EXERCISE 5: CREATE A SIMPLE REPORT (10 minutes)
# -----------------------------------------------------
# Create visualizations for a board meeting

# Plot 1: Compare satisfaction for hotels WITH and WITHOUT free WiFi
wifi_comparison <- hotel_data %>%
  group_by(wifi_free) %>%
  summarise(avg_satisfaction = mean(guest_satisfaction))

wifi_comparison$wifi_status <- ifelse(wifi_comparison$wifi_free == 1, "Free WiFi", "No Free WiFi")

ggplot(wifi_comparison, aes(x = wifi_status, y = avg_satisfaction, fill = wifi_status)) +
  geom_bar(stat = "identity") +
  labs(title = "Free WiFi Increases Guest Satisfaction",
       y = "Average Satisfaction Score",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = round(avg_satisfaction, 2)), vjust = -0.5)

# Plot 2: Show relationship between room rate and satisfaction
ggplot(hotel_data, aes(x = avg_room_rate, y = guest_satisfaction)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Higher Satisfaction Allows Higher Room Rates",
       x = "Average Room Rate ($)",
       y = "Guest Satisfaction Score") +
  theme_minimal()

# -----------------------------------------------------
# BONUS QUESTION: CRITICAL THINKING (5 minutes)
# -----------------------------------------------------
# The CEO says: "I heard that having a gym is crucial for satisfaction.
# Should we invest $500,000 in a new gym?"

# Let's check the data
gym_correlation <- cor(hotel_data$gym_quality_score, hotel_data$guest_satisfaction)
print(paste("Gym quality correlation with satisfaction:", round(gym_correlation, 3)))

# Compare to other factors we've seen
print("=== Correlation Comparison ===")
print(paste("Staff ratio:", round(correlation_staff, 3)))
print(paste("Noise level:", round(correlation_noise, 3)))
print(paste("Gym quality:", round(gym_correlation, 3)))

# Your recommendation to the CEO (write as a comment):
# CEO RESPONSE: Based on the data, gym quality has a correlation of only [X]
# with satisfaction, which is much lower than staff ratio ([Y]). 
# I recommend focusing on staff training instead of gym renovation.
# This would cost less and have a bigger impact on guest satisfaction.

# -----------------------------------------------------
# WRAP-UP QUESTIONS (Answer as comments)
# -----------------------------------------------------
# 1. What surprised you most about the analysis results?
# ANSWER:

# 2. If you were the hotel manager, what would you do first?
# ANSWER:

# 3. What other data would help make better decisions?
# ANSWER:

print("=== EXERCISES COMPLETE ===")
print("Great job! You've reviewed all the key concepts from the past 3 weeks.")
print("Now you're ready for the assignment consultation!")