# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
load(here("models", "mnl.RData"))

# Load data
data <- read_csv(here('data', 'mnl.csv'))
head(data)

# -----------------------------------------------------------------------------
# Single market simulation using the linear model

head(data)
summary(mnl_linear)

# Create a set of alternatives for which to simulate shares
alts <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    price       = c(15, 25, 21),
    fuelEconomy = c(20, 100, 40),
    accelTime   = c(8, 6, 7),
    powertrain_Electric  = c(0, 1, 0))

# Columns are attributes, rows are alternatives
alts 

# Use the predictProbs() function to compute the probabilities
sim_mnl_linear <- predictProbs(
    model = mnl_linear,
    alts = alts, 
    altID = 'altID',
    obsID = 'obsID', 
    ci = 0.95)

sim_mnl_linear

# -----------------------------------------------------------------------------
# Multiple simulations using the linear model

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
head(scenarios)

# Use the predictProbs() function to compute the probabilities
sim_mnl_linear_multi <- predictProbs(
    model = mnl_linear,
    alts = scenarios, 
    altID = 'altID',
    obsID = 'obsID', 
    ci = 0.95)

head(sim_mnl_linear_multi)

# Save simulations
save(
    sim_mnl_linear,
    sim_mnl_linear_multi,
    file = here("sims", "mnl_linear.RData")
)
