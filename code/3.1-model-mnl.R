# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(cbcTools)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'mnl.csv'))
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
# "powertrain"  = Indicates if the car is electric or gasoline

# -----------------------------------------------------------------------------
# Estimate MNL model with:
# - Continuous (linear) coefficients for price, fuelEconomy, and accelTime
# - Dummy-coded (discrete) coefficients for powertrain

# First dummy code the powertrain variable
data <- data %>%
  cbc_encode(coding = "dummy", ref_levels = list(powertrain = "Gasoline"))

# Estimate the model
model_mnl <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c('price', 'fuelEconomy', 'accelTime', 'powertrainElectric')
)

# View summary of results
summary(model_mnl)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl$hessian)$values

# -----------------------------------------------------------------------------
# Save model object

save(
  model_mnl,
  file = here("models", "model_mnl.RData")
)
