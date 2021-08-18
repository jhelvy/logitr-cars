# Estimate multinomial logit (mnl) models using the logitr package

# Load libraries
library(tidyverse)
library(logitr)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'data_mnl.csv'))
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
# Estimate MNL model with price, fuelEconomy, accelTime, and powertrain

# Estimate the model
model <- logitr(
    data   = data,
    choice = "choice",
    obsID  = "obsID",
    pars   = c('price', 'fuelEconomy', 'accelTime', 'powertrain')
)

# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

# -----------------------------------------------------------------------------
# Estimate MNL model where all covariates are dummy-coded

# Create dummy coded variables
data_dummy <- dummy_cols(
    data, c('price', 'fuelEconomy', 'accelTime', 'powertrain'))
head(data_dummy)

# Estimate the model
model_dummy <- logitr(
    data   = data_dummy,
    choice = "choice",
    obsID  = "obsID",
    pars   = c(
        # Remember one level must be "dummied out"
        "price_20", "price_25", 
        "fuelEconomy_25", "fuelEconomy_30", 
        "accelTime_7", "accelTime_8",
        "powertrain_Gasoline")
)

# View summary of results
summary(model_dummy)

# Check the 1st order condition: Is the gradient at the solution zero?
model_dummy$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_dummy$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects 

save(
    model, 
    model_dummy, 
    file = here("output", "models.RData")
)
