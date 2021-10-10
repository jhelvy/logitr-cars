# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
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
# "range"       = The *additional* driving range of the electric car 
#                 beyond 100 miles (0 means an EV with a 100-mile range)
# "powertrain_Electric" = Indicates if the car is electric or gas (1, 0)

# -----------------------------------------------------------------------------
# Estimate MNL model with continuous (linear) price, fuelEconomy, accelTime, 
# and range

# Estimate the model
mnl_attspec <- logitr(
    data   = data,
    choice = "choice",
    obsID  = "obsID",
    pars = c(
        'price', 'fuelEconomy', 'accelTime', 'powertrain_Electric', 'range')
)

# View summary of results
summary(mnl_attspec)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_attspec$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_attspec$hessian)$values

# What is the utililty of each EV range (100, 150, 200, 250)? 
ev100 <- coef(mnl_attspec)['powertrain_Electric']
range <- coef(mnl_attspec)['range']
ev150 <- ev100 + range*50
ev200 <- ev100 + range*100
ev250 <- ev100 + range*150

ev100
ev150
ev200
ev250

# -----------------------------------------------------------------------------
# Save model object

save(
    mnl_attspec,
    file = here("models", "mnl_attspec.RData")
)

