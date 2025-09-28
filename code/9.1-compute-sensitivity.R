# Measure the sensitivity of the market simulation outcome for one product to
# changes in attribute levels

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)
library(jph)

# Load estimated models
load(here("models", "model_mnl.RData"))

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in *price*

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
  altID = c(1, 2, 3),
  obsID = c(1, 1, 1),
  price = c(15, 25, 21),
  fuelEconomy = c(20, 100, 40),
  accelTime = c(8, 6, 7),
  powertrainelectric = c(0, 1, 0)
)

baseline

# Define the sensitivity cases
# For this case, let's see how the market share for the Electric Vehicle
# (option 2) changes with different EV prices. That is, I'm holding everything
# the same in every simulation except the price for the EV

prices <- seq(10, 30) # Define sensitivity price levels
n <- length(prices) # Number of simulations (21)
scenarios_price <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Set the price for each scenario
scenarios_price$price[which(scenarios_price$altID == 2)] <- prices
head(scenarios_price)

# For each case, simulate the market share predictions
sens_price <- predict(
  model_mnl,
  newdata = scenarios_price,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
) %>%
  # Keep only EV alternative
  filter(altID == 2) %>%
  # Keep only prices and predictions
  select(price, starts_with("predicted_"))

sens_price

# The probability shifts from essentially 100% of the market share at
# a price of $10,000 to 0% at $30,000

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in multiple attributes

# For these cases, we'll look at how the market share for the Electric Vehicle
# (option 2) changes with +/- 20% changes price, fuel economy, & acceleration time

# "high" means they result in higher market shares
# "low"  means they result in lower market shares
cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~value,
    2,      2,     'price',       'high',  25*0.8,
    3,      2,     'price',       'low',   25*1.2,
    4,      2,     'fuelEconomy', 'high',  100*1.2,
    5,      2,     'fuelEconomy', 'low',   100*0.8,
    6,      2,     'accelTime',   'high',  6*0.8,
    7,      2,     'accelTime',   'low',   6*1.2
)

cases

# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n)
scenarios_atts$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Replace scenarios with case values
scenarios_atts <- scenarios_atts %>%
  left_join(cases, by = c("altID", "obsID")) %>%
  mutate(
    attribute = ifelse(is.na(attribute), "other", attribute),
    case = ifelse(is.na(case), "base", case),
    price = ifelse(attribute == 'price', value, price),
    fuelEconomy = ifelse(attribute == 'fuelEconomy', value, fuelEconomy),
    accelTime = ifelse(attribute == 'accelTime', value, accelTime)
  )

scenarios_atts

# For each case, simulate the market share predictions
sens_atts <- predict(
  model_mnl,
  newdata = scenarios_atts,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
) %>%
  # Keep only EV alternative
  filter(altID == 2) %>%
  # Keep only attributes and predictions
  select(attribute, case, value, predicted_prob)

sens_atts

# -----------------------------------------------------------------------------
# Save simulations

save(
  sens_price,
  sens_atts,
  file = here("sims", "sens_price_mnl.RData")
)
