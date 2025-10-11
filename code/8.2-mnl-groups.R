# Estimate multinomial logit (MNL) models for two groups in the data

# Load libraries
library(logitr)
library(tidyverse)
library(cbcTools)
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
# "powertrain"  = Indicates if the car is electric or gasoline
# "group"       = Indicates the respondent group ("A" or "B")

# -----------------------------------------------------------------------------
# Estimate MNL model with linear price, fuelEconomy, and accelTime

# First dummy code the powertrain variable
data <- data %>%
  cbc_encode(coding = "dummy", ref_levels = list(powertrain = "Gasoline"))

# Create interactions of each variable with groupB
data <- data %>%
  mutate(
    price_B = price * groupB,
    fuelEconomy_B = fuelEconomy * groupB,
    accelTime_B = accelTime * groupB,
    powertrainElectric_B = powertrainElectric * groupB
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
    'powertrainElectric',
    # Introduce group interactions with all main effects
    'price_B',
    'fuelEconomy_B',
    'accelTime_B',
    'powertrainElectric_B'
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
  select(price, fuelEconomy, accelTime, powertrainElectric)
coef_draws_B <- coef_draws %>%
  mutate(
    price = price + price_B,
    fuelEconomy = fuelEconomy + fuelEconomy_B,
    accelTime = accelTime + accelTime_B,
    powertrainElectric = powertrainElectric + powertrainElectric_B
  ) %>%
  select(price, fuelEconomy, accelTime, powertrainElectric)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1 * coef_draws_A$price)
wtp_B <- coef_draws_B / (-1 * coef_draws_B$price)
ci(wtp_A, level = 0.95)
ci(wtp_B, level = 0.95)

# -----------------------------------------------------------------------------
# Alternatively, can just directly estimate WTP for each group

# First, split data into groups
data_A <- data %>% filter(groupB == 0)
data_B <- data %>% filter(groupB == 1)

# Estimate separate models for each group
model_mnl_wtp_groupA <- logitr(
  data = data_A,
  outcome = "choice",
  obsID = "obsID",
  pars = c('fuelEconomy', 'accelTime', 'powertrainElectric'),
  scalePar = 'price'
)

model_mnl_wtp_groupB <- logitr(
  data = data_B,
  outcome = "choice",
  obsID = "obsID",
  pars = c('fuelEconomy', 'accelTime', 'powertrainElectric'),
  scalePar = 'price'
)

# View summary of results
summary(model_mnl_wtp_groupA)
summary(model_mnl_wtp_groupB)

# Check the 1st order condition: Is the gradient at the solution zero?
model_mnl_wtp_groupA$gradient
model_mnl_wtp_groupB$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_mnl_wtp_groupA$hessian)$values
eigen(model_mnl_wtp_groupB$hessian)$values

# Save model objects
save(
  model_mnl_wtp_groupA,
  model_mnl_wtp_groupB,
  file = here("models", "model_mnl_group_wtp.RData")
)

# -----------------------------------------------------------------------------
# Compute WTP for each group with uncertainty

# Take 10,000 draws of the coefficients of each model
coefs_A <- coef(model_mnl_wtp_groupA)
covariance_A <- vcov(model_mnl_wtp_groupA)
coef_draws_A <- as.data.frame(MASS::mvrnorm(10^4, coefs_A, covariance_A))

coefs_B <- coef(model_mnl_wtp_groupB)
covariance_B <- vcov(model_mnl_wtp_groupB)
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
  powertrainElectric = c(0, 1, 0)
)

# Use the logit_probs() function to compute the probabilities
sim_A <- predict(
  model_mnl_wtp_groupA,
  newdata = data_sim,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
)

sim_B <- predict(
  model_mnl_wtp_groupB,
  newdata = data_sim,
  obsID = 'obsID',
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE
)

sim_A
sim_B
