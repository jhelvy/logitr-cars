# Compute WTP from estimated "preference space" model

# Load libraries
library(logitr)
library(tidyverse)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# Load the estimated model
load(here("models", "model_mnl.RData"))

# Get the model coefficients
coefs <- coef(model_mnl)
coefs

# Compute WTP estimates
wtp <- coefs / (-1 * coefs['price'])

# Compute WTP with uncertainty:

# Get the model coefficients and covariance matrix
covariance <- vcov(model_mnl)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Compute WTP for each coefficient draw
wtp_draws = -1 * (coef_draws[, 2:4] / coef_draws[, 1])
head(wtp_draws)

# For each coefficient, get the mean and 95% confidence interval of WTP
wtp_ci <- ci(wtp_draws, level = 0.95)
wtp_ci
