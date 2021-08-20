# Directly estimate WTP using a "WTP Space" model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data <- read_csv(here('data', 'data_mnl.csv'))
head(data)

# Estimate the model
model_wtp <- logitr(
    data   = data,
    choice = "choice",
    obsID  = "obsID",
    pars   = c('fuelEconomy', 'accelTime', 'powertrain'), 
    price = 'price', 
    modelSpace = 'wtp', 
    numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(model_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
model_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_wtp$hessian)$values

# Compare computed versus estimated WTP
load(here("output", "model_mnl.RData"))
wtpCompare(model_linear, model_wtp, price = 'price')

# Save model
save(
    model_wtp,
    file = here("output", "model_wtp.RData")
)
