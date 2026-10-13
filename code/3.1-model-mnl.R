# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)

options(dplyr.width = Inf) # So you can see all of the columns

# -------------------------------------------------------------
# Load the data set:
data <- read_csv(file.path('data', 'mnl.csv'))
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
# "powertrain"  = Indicates if the car is gasoline, hybrid, or electric

# -------------------------------------------------------------
# Estimate MNL model with:
# - Continuous (linear) coefficients for price, fuelEconomy, and accelTime
# - Dummy-coded (discrete) coefficients for powertrain

# Dummy code categorical variables (powertrain)
# This keeps all columns and adds one dummy column per level. The reference
# level (Gasoline) is the one we leave out of "pars" in the model below.
data <- data %>%
  dummy_cols("powertrain")

head(data) # preview

# Estimate the model
model_mnl <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    'price',
    'fuelEconomy',
    'accelTime',
    'powertrain_Electric',
    'powertrain_Hybrid'
  )
)

# View summary of results
summary(model_mnl)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl$hessian)$values

# -------------------------------------------------------------
# Save model object

save(
  model_mnl,
  file = file.path("models", "model_mnl.RData")
)
