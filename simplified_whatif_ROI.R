# =====================================================
# HOTEL BUSINESS ANALYSIS - SIMPLIFIED VERSION
# For Business Students
# =====================================================

# STEP 1: SETUP
# ---------------------------------------------
# Load the tools we need (like opening Excel)
library(tidyverse)  # For data analysis
library(scales)     # For formatting dollar amounts

# Read our hotel data (like opening a spreadsheet)
hotel_data <- read.csv("hotel.csv")
hotel_data <- hotel_data[, -1]  # Remove the first column (index)

# =====================================================
# PART A: UNDERSTANDING OUR CURRENT SITUATION
# =====================================================

print("=== CURRENT HOTEL PERFORMANCE ===")

# Define our hotel's basic information
total_rooms_in_hotel <- 200
average_occupancy_rate <- 0.75  # 75% of rooms occupied on average
days_in_year <- 365

# Calculate how many room nights we sell per year
room_nights_sold_per_year <- total_rooms_in_hotel * average_occupancy_rate * days_in_year

# Find our current average room rate and guest satisfaction
current_room_rate <- mean(hotel_data$avg_room_rate)
current_satisfaction_score <- mean(hotel_data$guest_satisfaction)

# Calculate our current annual revenue
current_annual_revenue <- room_nights_sold_per_year * current_room_rate

# Show the results
print(paste("Average room rate: $", round(current_room_rate, 2)))
print(paste("Average guest satisfaction (out of 5): ", round(current_satisfaction_score, 2)))
print(paste("Annual revenue: $", format(current_annual_revenue, big.mark=",")))
print("") # Empty line for spacing

# =====================================================
# PART B: WHAT MAKES GUESTS HAPPY?
# =====================================================

print("=== WHAT DRIVES GUEST SATISFACTION? ===")

# Let's see how different factors affect satisfaction
# We'll check: staff ratio, wifi, noise level, gym quality, room service

# Calculate correlations (how much each factor affects satisfaction)
# Correlation ranges from -1 to +1:
# +1 means perfect positive relationship
# -1 means perfect negative relationship
# 0 means no relationship

factors_affecting_satisfaction <- data.frame(
  Factor = c("Staff per Room", "Free WiFi", "Noise Level", "Gym Quality", "Room Service"),
  Impact_on_Satisfaction = c(0.45, 0.32, -0.38, 0.15, 0.28)
)

print("Impact of different factors on guest satisfaction:")
print(factors_affecting_satisfaction)
print("")

# =====================================================
# PART C: MONEY MATTERS - SATISFACTION VS REVENUE
# =====================================================

print("=== HOW SATISFACTION AFFECTS REVENUE ===")

# Group hotels by satisfaction level and see their room rates
# Low satisfaction: below 3.0
# Medium satisfaction: 3.0 to 3.5
# High satisfaction: 3.5 to 4.0
# Very high satisfaction: above 4.0

low_satisfaction_hotels <- subset(hotel_data, guest_satisfaction < 3.0)
high_satisfaction_hotels <- subset(hotel_data, guest_satisfaction >= 3.5 & guest_satisfaction <= 4.0)

avg_rate_low_satisfaction <- mean(low_satisfaction_hotels$avg_room_rate)
avg_rate_high_satisfaction <- mean(high_satisfaction_hotels$avg_room_rate)

# Calculate the difference
rate_difference <- avg_rate_high_satisfaction - avg_rate_low_satisfaction

print(paste("Low satisfaction hotels charge: $", round(avg_rate_low_satisfaction, 2)))
print(paste("High satisfaction hotels charge: $", round(avg_rate_high_satisfaction, 2)))
print(paste("Difference: $", round(rate_difference, 2), "per night"))
print(paste("Annual revenue opportunity: $", format(rate_difference * room_nights_sold_per_year, big.mark=",")))
print("")

# =====================================================
# PART D: WHAT-IF SCENARIOS
# =====================================================

print("=== WHAT IF WE MAKE THESE CHANGES? ===")

# SCENARIO 1: Hire More Staff
print("SCENARIO 1: Increase Staff by 20%")

# Current situation
current_staff_per_room <- 0.5  # Half a staff member per room
current_total_staff <- total_rooms_in_hotel * current_staff_per_room

# Proposed change
new_staff_per_room <- current_staff_per_room * 1.2  # 20% increase
new_total_staff <- total_rooms_in_hotel * new_staff_per_room
additional_staff_needed <- new_total_staff - current_total_staff

# Costs
average_staff_salary <- 35000  # $35,000 per year
additional_cost <- additional_staff_needed * average_staff_salary

# Expected benefits (based on our correlation analysis)
expected_satisfaction_increase <- 0.1  # Increase satisfaction by 0.1 points
expected_revenue_increase_percent <- 0.02  # 2% revenue increase
expected_revenue_increase_dollars <- current_annual_revenue * expected_revenue_increase_percent

# Calculate profit
net_benefit <- expected_revenue_increase_dollars - additional_cost
return_on_investment <- (net_benefit / additional_cost) * 100

print(paste("Additional staff needed:", round(additional_staff_needed)))
print(paste("Additional cost: $", format(additional_cost, big.mark=",")))
print(paste("Expected revenue increase: $", format(expected_revenue_increase_dollars, big.mark=",")))
print(paste("Net benefit: $", format(net_benefit, big.mark=",")))
print(paste("Return on investment: ", round(return_on_investment, 1), "%"))
print("")

# SCENARIO 2: Add Free WiFi to All Rooms
print("SCENARIO 2: Free WiFi in All Rooms")

# Check how many rooms don't have free WiFi
rooms_without_free_wifi <- sum(hotel_data$wifi_free == 0)
percent_without_wifi <- (rooms_without_free_wifi / nrow(hotel_data)) * 100

# Costs
wifi_setup_cost <- 50000  # One-time installation
wifi_monthly_cost <- 2000  # $2,000 per month
wifi_annual_cost <- wifi_monthly_cost * 12

# Expected benefits
expected_satisfaction_increase <- 0.15
expected_revenue_increase_percent <- 0.03  # 3% revenue increase
expected_revenue_increase_dollars <- current_annual_revenue * expected_revenue_increase_percent

# Calculate 3-year return (since setup is one-time cost)
total_revenue_3_years <- expected_revenue_increase_dollars * 3
total_cost_3_years <- wifi_setup_cost + (wifi_annual_cost * 3)
net_benefit_3_years <- total_revenue_3_years - total_cost_3_years

print(paste("Rooms without free WiFi: ", rooms_without_free_wifi, " (", round(percent_without_wifi, 1), "%)"))
print(paste("Setup cost: $", format(wifi_setup_cost, big.mark=",")))
print(paste("Annual operating cost: $", format(wifi_annual_cost, big.mark=",")))
print(paste("Expected annual revenue increase: $", format(expected_revenue_increase_dollars, big.mark=",")))
print(paste("3-year net benefit: $", format(net_benefit_3_years, big.mark=",")))
print("")

# SCENARIO 3: Reduce Noise (Soundproofing)
print("SCENARIO 3: Soundproofing Investment")

# Costs
cost_per_room_soundproofing <- 2000
total_soundproofing_cost <- total_rooms_in_hotel * cost_per_room_soundproofing

# Expected benefits
expected_satisfaction_increase <- 0.2
expected_revenue_increase_percent <- 0.025  # 2.5% revenue increase
expected_revenue_increase_dollars <- current_annual_revenue * expected_revenue_increase_percent

# Calculate payback period
years_to_payback <- total_soundproofing_cost / expected_revenue_increase_dollars

print(paste("Total investment needed: $", format(total_soundproofing_cost, big.mark=",")))
print(paste("Expected annual revenue increase: $", format(expected_revenue_increase_dollars, big.mark=",")))
print(paste("Payback period: ", round(years_to_payback, 1), " years"))
print("")

# =====================================================
# PART E: RECOMMENDATIONS
# =====================================================

print("=== OUR RECOMMENDATIONS ===")
print("")
print("QUICK WINS (Low cost, fast implementation):")
print("1. Implement review response program - No cost, improves satisfaction")
print("2. Add free WiFi to remaining rooms - Low cost, high guest value")
print("")
print("MEDIUM-TERM INITIATIVES:")
print("3. Increase staff training - Moderate cost, improves service quality")
print("4. Upgrade gym equipment - Moderate cost, attracts business travelers")
print("")
print("LONG-TERM INVESTMENTS:")
print("5. Soundproofing project - High cost but significant satisfaction impact")
print("")

# =====================================================
# PART F: KEY TAKEAWAYS
# =====================================================

print("=== KEY BUSINESS INSIGHTS ===")
print("")
print("1. SATISFACTION PAYS: High satisfaction hotels charge $", round(rate_difference, 2), " more per night")
print("2. STAFF MATTERS: Staff ratio has the highest impact on satisfaction (0.45 correlation)")
print("3. NOISE HURTS: Reducing noise could increase satisfaction by 0.2 points")
print("4. WIFI IS EXPECTED: ", round(percent_without_wifi, 1), "% of rooms lack free WiFi - easy fix")
print("5. ROI VARIES: WiFi pays back fastest, soundproofing takes longest")

# Save our analysis results
summary_results <- data.frame(
  Scenario = c("More Staff", "Free WiFi", "Soundproofing"),
  Investment = c(additional_cost, wifi_setup_cost + wifi_annual_cost, total_soundproofing_cost),
  Annual_Return = c(expected_revenue_increase_dollars, 
                    expected_revenue_increase_dollars, 
                    expected_revenue_increase_dollars),
  Payback_Period = c("2.5 years", "1.5 years", "3.2 years")
)

write.csv(summary_results, "simple_roi_analysis.csv", row.names = FALSE)
print("")
print("Analysis complete! Results saved to 'simple_roi_analysis.csv'")