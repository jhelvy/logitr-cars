# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
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
# "powertrain"  = Indicates if the car is electric or gas

# -----------------------------------------------------------------------------
# Estimate MNL model where all covariates are dummy-coded

# Create dummy coded variables
data_dummy <- dummy_cols(
    data, c('price', 'fuelEconomy', 'accelTime', 'powertrain'))
head(data_dummy)

# Estimate the model
mnl_dummy <- logitr(
    data    = data_dummy,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c(
        # Remember one level must be "dummied out"
        "price_20", "price_25", 
        "fuelEconomy_25", "fuelEconomy_30", 
        "accelTime_7", "accelTime_8",
        "powertrain_Electric")
)

# View summary of results
summary(mnl_dummy)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_dummy$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_dummy$hessian)$values

# -----------------------------------------------------------------------------
# Estimate MNL model with continuous (linear) price, fuelEconomy, and accelTime

# Estimate the model
mnl_linear <- logitr(
    data    = data_dummy,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c('price', 'fuelEconomy', 'accelTime', 'powertrain_Electric')
)

# View summary of results
summary(mnl_linear)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_linear$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_linear$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects 

save(
    mnl_dummy,
    mnl_linear,
    file = here("models", "mnl.RData")
)
