# Estimate multinomial logit (mnl) models *with outside good*

# Load libraries
library(logitr)
library(tidyverse)
library(cbcTools)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'no_choice.csv'))
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
# "powertrain" = Indicates if the car is electric or gasoline
# "no_choice" = Indicates the "no choice" alternative

# -----------------------------------------------------------------------------
# Estimate MNL model with outside good

# First dummy code the powertrain variable
data <- data %>%
  cbc_encode(
    coding = "dummy",
    ref_levels = list(powertrain = "Gasoline")
  )

# Estimate the model
model_mnl_no_choice <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    'price',
    'fuelEconomy',
    'accelTime',
    'powertrainElectric',
    'no_choice'
  )
)

# View summary of results
summary(model_mnl_no_choice)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_no_choice$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_no_choice$hessian)$values

# Save model object
save(
  model_mnl_no_choice,
  file = here("models", "model_mnl_no_choice.RData")
)
