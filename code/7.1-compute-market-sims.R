# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
load(here("models", "model_mnl.RData"))

# -----------------------------------------------------------------------------
# Single market simulation using the mnl model

summary(model_mnl)

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
  altID = c(1, 2, 3),
  obsID = c(1, 1, 1),
  price = c(15, 25, 21),
  fuelEconomy = c(20, 100, 40),
  accelTime = c(8, 6, 7),
  powertrainElectric = c(0, 1, 0)
)

# Columns are attributes, rows are alternatives
baseline

# Use the predict() function to compute the probabilities
sim_mnl <- predict(
  model_mnl,
  newdata = baseline,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE # This returns your data along with predicted values
)

sim_mnl

# -----------------------------------------------------------------------------
# Multiple simulations using the mnl model

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
head(scenarios)

# Use the predict() function to compute the probabilities
sim_mnl_multi <- predict(
  model_mnl,
  newdata = scenarios,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
)

head(sim_mnl_multi)

# Save simulations
save(
  sim_mnl,
  sim_mnl_multi,
  file = here("sims", "mnl.RData")
)
