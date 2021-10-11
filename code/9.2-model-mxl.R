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
# "powertrain"  = Indicates if the car is electric or gas

# -----------------------------------------------------------------------------
# Estimate MXL model with linear price, fuelEconomy, and accelTime

# Create dummy coefficients for powertrain
data_dummy <- fastDummies::dummy_cols(data, 'powertrain')
head(data_dummy)

# Estimate the model
mxl_linear <- logitr(
    data   = data_dummy,
    choice = "choice",
    obsID  = "obsID",
    pars   = c('price', 'fuelEconomy', 'accelTime', 'powertrain_Electric'),
    randPars = c(fuelEconomy = 'n', accelTime = 'n', powertrain_Electric = 'n')
)

# View summary of results
summary(mxl_linear)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_linear$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_linear$hessian)$values

# -----------------------------------------------------------------------------
# Save model objects 

save(
    mxl_linear,
    file = here("models", "mxl.RData")
)
