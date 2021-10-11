# Directly estimate WTP using a "WTP Space" model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data <- read_csv(here('data', 'mnl.csv'))
head(data)

# Create dummy coded variable for powertrain
data_dummy <- dummy_cols(data, 'powertrain')
head(data_dummy)

# Estimate the model
mnl_wtp <- logitr(
    data   = data_dummy,
    choice = "choice",
    obsID  = "obsID",
    pars   = c('fuelEconomy', 'accelTime', 'powertrain_Electric'),
    price = 'price', 
    modelSpace = 'wtp', 
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(mnl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values

# Compare computed versus estimated WTP
load(here("models", "mnl.RData"))
wtpCompare(mnl_linear, mnl_wtp, price = 'price')

# Save model
save(
    mnl_wtp,
    file = here("models", "mnl_wtp.RData")
)
