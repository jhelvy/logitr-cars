# Estimate mixed logit (MXL) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'mxl.csv'))
head(data)

# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrainelectric" = Indicates if the car is electric or gas (1, 0)

# -----------------------------------------------------------------------------
# Estimate preference space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
model_mxl_pref <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c('price', 'fuelEconomy', 'accelTime', 'powertrainelectric'),
  randPars = c(fuelEconomy = 'n', accelTime = 'n', powertrainelectric = 'n')
)

# View summary of results
summary(model_mxl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mxl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mxl_pref$hessian)$values

# -----------------------------------------------------------------------------
# Estimate WTP space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
model_mxl_wtp <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c('fuelEconomy', 'accelTime', 'powertrainelectric'),
  scalePar = 'price',
  randPars = c(fuelEconomy = 'n', accelTime = 'n', powertrainelectric = 'n')
)

# View summary of results
summary(model_mxl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mxl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mxl_wtp$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects

save(
  model_mxl_pref,
  model_mxl_wtp,
  file = here("models", "model_mxl.RData")
)
