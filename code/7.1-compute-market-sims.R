# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
load(here("models", "mnl.RData"))

# -----------------------------------------------------------------------------
# Single market simulation using the linear model

summary(mnl_linear)

# Create a set of alternatives for which to simulate shares
data <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    price       = c(15, 25, 21),
    fuelEconomy = c(20, 100, 40),
    accelTime   = c(8, 6, 7),
    powertrain_Electric  = c(0, 1, 0))

# Columns are attributes, rows are alternatives
data

# Use the predict() function to compute the probabilities
sim_mnl_linear <- predict(
    mnl_linear,
    newdata = data, 
    obsID = 'obsID', 
    ci = 0.95, 
    returnData = TRUE # This returns your data along with predicted values
)

sim_mnl_linear

# -----------------------------------------------------------------------------
# Multiple simulations using the linear model

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
head(scenarios)

# Use the predict() function to compute the probabilities
sim_mnl_linear_multi <- predict(
    mnl_linear,
    newdata = scenarios, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE
)

head(sim_mnl_linear_multi)

# Save simulations
save(
    sim_mnl_linear,
    sim_mnl_linear_multi,
    file = here("sims", "mnl_linear.RData")
)
