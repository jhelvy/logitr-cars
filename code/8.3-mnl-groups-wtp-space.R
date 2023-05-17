# Estimate multinomial logit (MNL) models for two groups in the data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)

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

# Create dummy coefficients for powertrain variable
data <- fastDummies::dummy_cols(data, 'powertrain')

# Split data into groups
data_A <- data %>% filter(group == "A")
data_B <- data %>% filter(group == "B")

# Estimate separate models for each group
mnl_group_A <- logitr(
    data    = data_A,
    outcome = "choice",
    obsID   = "obsID",
    pars = c('fuelEconomy', 'accelTime', 'powertrain_Electric'),
    scalePar = 'price'
)

mnl_group_B <- logitr(
    data    = data_B,
    outcome = "choice",
    obsID   = "obsID",
    pars = c('fuelEconomy', 'accelTime', 'powertrain_Electric'),
    scalePar = 'price'
)

# View summary of results
summary(mnl_group_A)
summary(mnl_group_B)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_group_A$gradient
mnl_group_B$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_group_A$hessian)$values
eigen(mnl_group_B$hessian)$values

# Save model objects
save(
    mnl_group_A,
    mnl_group_B,
    file = here("models", "mnl_group_wtp.RData")
)

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(mnl_group_A)
covariance_A <- vcov(mnl_group_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))

coefs_B <- coef(mnl_group_B)
covariance_B <- vcov(mnl_group_B)
coef_draws_B <- as.data.frame(MASS::mvrnorm(10^4, coefs_B, covariance_B))

# -----------------------------------------------------------------------------
# Compute WTP for each group

ci(coef_draws_A)
ci(coef_draws_B)

# Compare with estimated coefficients
coef(mnl_group_A)
coef(mnl_group_B)

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

# Create "price" coefficient as negative of "scalePar"
coef_draws_A <- coef_draws_A %>% 
    mutate(price = -1*scalePar) %>% 
    select(-scalePar)
coef_draws_B <- coef_draws_B %>% 
    mutate(price = -1*scalePar) %>% 
    select(-scalePar)

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




# Estimate a preference space model
mnl_pref <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
)

# Compute a confidence interval
confint(mnl_pref)
