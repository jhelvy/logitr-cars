# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(cbcTools)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'mnl_attspec.csv'))
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
# "rangeElectric" = The driving range of the electric car
# "powertrain" = Indicates if the car is electric or gasoline

# -----------------------------------------------------------------------------
# Estimate MNL model with:
# - Continuous (linear) coefficients for price, fuelEconomy, accelTime, and rangeElectric
# - Dummy-coded (discrete) coefficients for powertrain

# First dummy code the powertrain variable
data <- data %>%
  cbc_encode(coding = "dummy", ref_levels = list(powertrain = "Gasoline"))

# Estimate the model
model_mnl_attspec <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    'price',
    'fuelEconomy',
    'accelTime',
    'rangeElectric',
    'powertrainElectric'
  )
)

# View summary of results
summary(model_mnl_attspec)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_attspec$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_attspec$hessian)$values

# What is the utility of each EV range (100, 150, 200, 250)?
ev <- coef(model_mnl_attspec)['powertrainElectric']
range <- coef(model_mnl_attspec)['rangeElectric']
ev100 <- ev + range * 100
ev150 <- ev + range * 150
ev200 <- ev + range * 200
ev250 <- ev + range * 250

ev100
ev150
ev200
ev250

# -----------------------------------------------------------------------------
# Save model object

save(
  model_mnl_attspec,
  file = here("models", "model_mnl_attspec.RData")
)
