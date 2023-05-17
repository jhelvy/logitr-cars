# Estimate multinomial logit (MNL) models for two groups in the data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)
library(jph)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'mnl_2groups.csv'))
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
# "group"       = Indicates the respondent group ("A" or "B")

# -----------------------------------------------------------------------------
# Estimate MNL model with linear price, fuelEconomy, and accelTime

# Create dummy coefficients for the group and powertrain variables
data_dummy <- fastDummies::dummy_cols(data, c('powertrain', 'group'))
head(data_dummy)

# Create interactions of each variable with group_B
data_dummy <- data_dummy %>%
    mutate(
        price_B       = price*group_B,
        fuelEconomy_B = fuelEconomy*group_B,
        accelTime_B   = accelTime*group_B,
        powertrain_Electric_B = powertrain_Electric*group_B
    )
head(data_dummy)

# Estimate the model
mnl_groups <- logitr(
    data    = data_dummy,
    outcome = "choice",
    obsID   = "obsID",
    pars = c(
        'price', 'fuelEconomy', 'accelTime', 'powertrain_Electric',
        # Introduce group interactions with all main effects
        'price_B', 'fuelEconomy_B', 'accelTime_B',
        'powertrain_Electric_B'
    )
)

# View summary of results
summary(mnl_groups)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_groups$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_groups$hessian)$values

# Save model objects
save(
    mnl_groups,
    file = here("models", "mnl_groups.RData")
)

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

# Get the model coefficients and covariance matrix
coefs <- coef(mnl_groups)
covariance <- vcov(mnl_groups)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
coef_draws_A <- coef_draws %>%
    select(price, fuelEconomy, accelTime, powertrain_Electric)
coef_draws_B <- coef_draws %>%
    mutate(
        price       = price + price_B,
        fuelEconomy = fuelEconomy + fuelEconomy_B,
        accelTime   = accelTime + accelTime_B,
        powertrain_Electric = powertrain_Electric + powertrain_Electric_B) %>%
    select(price, fuelEconomy, accelTime, powertrain_Electric)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1* coef_draws_A$price)
wtp_B <- coef_draws_B / (-1* coef_draws_B$price)
ci(wtp_A)
ci(wtp_B)

# -----------------------------------------------------------------------------
# Compute the market shares of a given market for each group

# Create a set of alternatives for which to simulate shares
data <- data.frame(
    altID       = c(1, 2, 3), 
    obsID       = c(1, 1, 1),
    price       = c(15, 25, 21),
    fuelEconomy = c(20, 100, 40),
    accelTime   = c(8, 6, 7),
    powertrain_Electric = c(0, 1, 0))

# Columns are attributes, rows are alternatives
data 

# Use the logit_probs() function (from {jph}) to compute the probabilities
sim_A <- jph::logit_probs(
    coefs = coef_draws_A,
    newdata = data, 
    obsID = 'obsID', 
    ci = 0.95
)

sim_B <- jph::logit_probs(
    coefs = coef_draws_B,
    newdata = data, 
    obsID = 'obsID', 
    ci = 0.95
)

sim_A
sim_B
