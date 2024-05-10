# Preset
# Preset
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notion for entire R session

# Used libraries
library(ggplot2)
library(pacman)
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)
#install.packages("lubridate")
#install.packages("triangle")
library(triangle)

####ALY 6050- Assignment-4

# the parameters and data
D <- 15000 # Annual demand
C <- 80    # Unit cost
H <- 0.18  # Holding cost rate (18%)
Holding_cost_rate <- H * C
S <- 220   # Ordering cost per order

#  Economic Order Quantity (EOQ)
EOQ <- sqrt((2 * D * S) / Holding_cost_rate)
EOQ
#  Reorder point (ROP)
ROP <- EOQ / 2
ROP
#  number of orders per year
N <- D / EOQ

#  total ordering cost per year
Ordering_Cost <- N * S
Ordering_Cost
#  average inventory
Avg_Inventory <- EOQ / 2

#  holding cost per year
Holding_Cost <- Avg_Inventory * H * C
Holding_Cost
#  total inventory cost per year
Total_Inventory_Cost <- Ordering_Cost + Holding_Cost
Total_Inventory_Cost

#  a function to calculate total inventory cost

cal_total_cost <- function(Q , D , S , H, C ) {
  N <- D/Q
  Ordering_Cost <- N * S
  Avg_Inventory <- Q / 2
  Holding_Cost <- Avg_Inventory * H * C
  return(Ordering_Cost + Holding_Cost)
}

#  sequence of order quantities
order_quantities <- seq(100, 5000, by = 100)

#  total inventory cost for each order quantity
total_costs <- sapply(order_quantities, function(Q) cal_total_cost(Q, D, S, H, C))
total_costs
#  order quantity with the smallest total cost
optimal_reorder_quantity <- order_quantities[which.min(total_costs)]
optimal_reorder_quantity 
# Plot Total Cost versus Order Quantity
plot(order_quantities, total_costs, type = "l", ylab = "Order Quantity", xlab = "Total Cost",
     main = "Total Cost vs. Order Quantity")

######################################

#Part-II

# Set seed for reproducibility
set.seed(123)

# Defined parameters
S <- 220    # Ordering cost per order
H <- 0.18   # Holding cost rate (18%)
C <- 80     # Unit cost
Holding_cost_rate <- H * C
num_simulations <- 1000

#  Finding Annual demand using triangular distribution
annual_demand <- rtriangle(num_simulations, a = 13000, b = 17000, c = 15000)


#  Simulating and calculating minimum total cost for each occurrence
min_total_costs <- numeric(num_simulations)
for (i in 1:num_simulations) {
  D <- annual_demand[i]
  # Calculate optimal order quantity using EOQ formula
  EOQ <- sqrt((2 * D * S) / H)
  Q <- round(EOQ)  # Round to nearest integer
  min_total_costs[i] <- cal_total_cost(Q, D, S, H, C)
}

# (i) Estimate expected minimum total cost and construct 95% confidence interval
mean_min_total_cost <- mean(min_total_costs)
sd_min_total_cost <- sd(min_total_costs)
n <- length(min_total_costs)
lower_ci_min_total_cost <- mean_min_total_cost - qt(0.975, df = n - 1) * (sd_min_total_cost / sqrt(n))
upper_ci_min_total_cost <- mean_min_total_cost + qt(0.975, df = n - 1) * (sd_min_total_cost / sqrt(n))
cat("Expected Minimum Total Cost:", mean_min_total_cost, "\n")
cat("95% Confidence Interval for Minimum Total Cost:", lower_ci_min_total_cost, "-", upper_ci_min_total_cost, "\n")

# (ii) Estimate expected order quantity and construct 95% confidence interval
mean_order_quantity <- mean(annual_demand)
sd_order_quantity <- sd(annual_demand)
n <- length(annual_demand)
lower_ci_order_quantity <- mean_order_quantity - qt(0.975, df = n - 1) * (sd_order_quantity / sqrt(n))
upper_ci_order_quantity <- mean_order_quantity + qt(0.975, df = n - 1) * (sd_order_quantity / sqrt(n))
cat("Expected Order Quantity:", mean_order_quantity, "\n")
cat("95% Confidence Interval for Order Quantity:", lower_ci_order_quantity, "-", upper_ci_order_quantity, "\n")

# (iii) Estimate expected annual number of orders and construct 95% confidence interval
mean_annual_orders <- mean(annual_demand) / mean_order_quantity
sd_annual_orders <- sd(annual_demand) / sd_order_quantity
n <- length(annual_demand)
lower_ci_annual_orders <- mean_annual_orders - qt(0.975, df = n - 1) * (sd_annual_orders / sqrt(n))
upper_ci_annual_orders <- mean_annual_orders + qt(0.975, df = n - 1) * (sd_annual_orders / sqrt(n))
cat("Expected Annual Number of Orders:", mean_annual_orders, "\n")
cat("95% Confidence Interval for Annual Number of Orders:", lower_ci_annual_orders, "-", upper_ci_annual_orders, "\n")

# Determine the probability distributions that best fit the data
# For simplicity, let's use histograms to visualize the distributions

par(mfrow = c(1, 3))
hist(min_total_costs, breaks = 20, main = "Minimum Total Cost Distribution", xlab = "Total Cost")
hist(annual_demand, breaks = 20, main = "Annual Demand Distribution", xlab = "Demand")
hist(annual_demand / mean_order_quantity, breaks = 20, main = "Annual Number of Orders Distribution", xlab = "Number of Orders")

