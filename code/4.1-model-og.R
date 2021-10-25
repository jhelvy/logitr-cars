# Estimate multinomial logit (mnl) models *with outside good*

# Load libraries
library(logitr)
library(tidyverse)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data_og <- read_csv(here('data', 'og.csv'))
head(data_og)

# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain_Electric" = Indicates if the car is electric or gas (1, 0)
# "outsideGood" = Indicates the outside good alternative

# -----------------------------------------------------------------------------
# Estimate MNL model with outside good

# Estimate the model
mnl_og <- logitr(
    data    = data_og,
    outcome = "choice",
    obsID   = "obsID",
    pars = c(
        'price', 'fuelEconomy', 'accelTime', 'powertrain_Electric', 
        'outsideGood')
)

# View summary of results
summary(mnl_og)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_og$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_og$hessian)$values

# Save model object
save(
    mnl_og, 
    file = here("models", "mnl_og.RData")
)
