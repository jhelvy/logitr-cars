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
# "powertrainelectric" = Indicates if the car is electric or gas (1, 0)
# "group"       = Indicates the respondent group ("A" or "B")

# -----------------------------------------------------------------------------
# Estimate MNL model with linear price, fuelEconomy, and accelTime

# Create dummy coefficients for the group
data <- fastDummies::dummy_cols(data, 'group')
head(data)

# Create interactions of each variable with group_B
data <- data %>%
  mutate(
    price_B = price * group_B,
    fuelEconomy_B = fuelEconomy * group_B,
    accelTime_B = accelTime * group_B,
    powertrainelectric_B = powertrainelectric * group_B
  )
head(data)

# Estimate the model
model_mnl_groups <- logitr(
  data = data,
  outcome = "choice",
  obsID = "obsID",
  pars = c(
    'price',
    'fuelEconomy',
    'accelTime',
    'powertrainelectric',
    # Introduce group interactions with all main effects
    'price_B',
    'fuelEconomy_B',
    'accelTime_B',
    'powertrainelectric_B'
  )
)

# View summary of results
summary(model_mnl_groups)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_groups$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_groups$hessian)$values

# Save model objects
save(
  model_mnl_groups,
  file = here("models", "model_mnl_groups.RData")
)

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

# Get the model coefficients and covariance matrix
coefs <- coef(model_mnl_groups)
covariance <- vcov(model_mnl_groups)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
coef_draws_A <- coef_draws %>%
  select(price, fuelEconomy, accelTime, powertrainelectric)
coef_draws_B <- coef_draws %>%
  mutate(
    price = price + price_B,
    fuelEconomy = fuelEconomy + fuelEconomy_B,
    accelTime = accelTime + accelTime_B,
    powertrainelectric = powertrainelectric + powertrainelectric_B
  ) %>%
  select(price, fuelEconomy, accelTime, powertrainelectric)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1 * coef_draws_A$price)
wtp_B <- coef_draws_B / (-1 * coef_draws_B$price)
ci(wtp_A, level = 0.95)
ci(wtp_B, level = 0.95)

# -----------------------------------------------------------------------------
# Alternatively, can just directly estimate WTP for each group

# First, split data into groups
data_A <- data %>% filter(group == "A")
data_B <- data %>% filter(group == "B")

# Estimate separate models for each group
model_mnl_wtp_group_A <- logitr(
  data = data_A,
  outcome = "choice",
  obsID = "obsID",
  pars = c('fuelEconomy', 'accelTime', 'powertrainelectric'),
  scalePar = 'price'
)

model_mnl_wtp_group_B <- logitr(
  data = data_B,
  outcome = "choice",
  obsID = "obsID",
  pars = c('fuelEconomy', 'accelTime', 'powertrainelectric'),
  scalePar = 'price'
)

# View summary of results
summary(model_mnl_wtp_group_A)
summary(model_mnl_wtp_group_B)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_wtp_group_A$gradient
model_mnl_wtp_group_B$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_wtp_group_A$hessian)$values
eigen(model_mnl_wtp_group_B$hessian)$values

# Save model objects
save(
  model_mnl_wtp_group_A,
  model_mnl_wtp_group_B,
  file = here("models", "model_mnl_group_wtp.RData")
)

# -----------------------------------------------------------------------------
# Compute WTP for each group with uncertainty

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(model_mnl_wtp_group_A)
covariance_A <- vcov(model_mnl_wtp_group_A)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))

coefs_B <- coef(model_mnl_wtp_group_B)
covariance_B <- vcov(model_mnl_wtp_group_B)
coef_draws_B <- as.data.frame(MASS::mvrnorm(10^4, coefs_B, covariance_B))

# Compute WTP for each group
ci(coef_draws_A, level = 0.95)
ci(coef_draws_B, level = 0.95)


# -----------------------------------------------------------------------------
# Simulate the market shares of a given market for each group

# Create a set of alternatives for which to simulate shares
data_sim <- data.frame(
  altID = c(1, 2, 3),
  obsID = c(1, 1, 1),
  price = c(15, 25, 21),
  fuelEconomy = c(20, 100, 40),
  accelTime = c(8, 6, 7),
  powertrainelectric = c(0, 1, 0)
)

# Use the logit_probs() function to compute the probabilities
sim_A <- predict(
  model_mnl_wtp_group_A,
  newdata = data_sim,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
)

sim_B <- predict(
  model_mnl_wtp_group_B,
  newdata = data_sim,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
)

sim_A
sim_B
