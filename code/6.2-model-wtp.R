# Directly estimate WTP using a "WTP Space" model

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)

options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data <- read_csv(file.path('data', 'mnl.csv'))
head(data)

# Dummy code categorical variables (powertrain)
# This keeps all columns and adds one dummy column per level. The reference
# level (Gasoline) is the one we leave out of "pars" in the model below.
data <- data %>%
  dummy_cols("powertrain")

# Estimate the model
model_mnl_wtp <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    'fuelEconomy',
    'accelTime',
    'powertrain_Electric',
    'powertrain_Hybrid'
  ),
  scalePar = 'price',
  numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(model_mnl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_wtp$hessian)$values

# Compare computed versus estimated WTP
load(file.path("models", "model_mnl.RData"))
wtpCompare(model_mnl, model_mnl_wtp, scalePar = 'price')

# Save model
save(
  model_mnl_wtp,
  file = file.path("models", "model_mnl_wtp.RData")
)
