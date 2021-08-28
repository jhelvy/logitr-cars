# Estimate multinomial logit (MNL) models for two groups in the data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)
library(maddTools)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'data_mnl_2groups.csv'))
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

# Estimate the model
mnl_groups <- logitr(
    data   = data,
    choice = "choice",
    obsID  = "obsID",
    pars   = c(
        'price', 'fuelEconomy', 'accelTime', 'powertrain',
        # Introduce group interactions with all main effects
        'price*group', 'fuelEconomy*group', 'accelTime*group',
        'powertrain*group'
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
coefs <- coef(mnl_linear)
covariance <- vcov(mnl_linear)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
coef_draws_A <- coef_draws %>%
    select(price, fuelEconomy, accelTime, powertrain_elec)
coef_draws_B <- coef_draws %>%
    mutate(
        price           = price + price_B,
        fuelEconomy     = fuelEconomy + fuelEconomy_B,
        accelTime       = accelTime + accelTime_B,
        powertrain_elec = powertrain_elec + powertrain_elec_B) %>%
    select(price, fuelEconomy, accelTime, powertrain_elec)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A = draws_A / (-1* draws_A$price)
wtp_B = draws_B / (-1* draws_B$price)
getCI(wtp_A)
getCI(wtp_B)

# -----------------------------------------------------------------------------
# Compute the market shares of a given market for each group

# Define a market:
market = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Use the logitProbsDraws() function to compute the market shares
# with uncertainty for each group
sharesUnc_A = logitProbsDraws(draws_A, market)
sharesUnc_B = logitProbsDraws(draws_B, market)
sharesUnc_A
sharesUnc_B

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in price with uncertainty
# for each group

# Define a baseline market:
marketBaseline = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Define the sensitivity cases:
cases_price_unc_A = data.frame(
    price      = seq(10, 20),
    share_mean = NA,
    share_low  = NA,
    share_high = NA)
cases_price_unc_B = cases_price_unc_A

# Run the simulation for each case
for (i in 1:nrow(cases_price_unc_A)) {
    market = marketBaseline
    market$price[1] = cases_price_unc_A$price[i]
    sharesUnc_A = logitProbsDraws(draws_A, market)
    sharesUnc_B = logitProbsDraws(draws_B, market)
    cases_price_unc_A[i,2:4] = sharesUnc_A[1,]
    cases_price_unc_B[i,2:4] = sharesUnc_B[1,]
}
cases_price_unc_A
cases_price_unc_B

# Plot price sensitivity results for each group
cases_price_unc_A$group = 'A'
cases_price_unc_B$group = 'B'
cases_price_unc = bind_rows(cases_price_unc_A, cases_price_unc_B)
share_price_plot = ggplot(cases_price_unc,
    aes(x=price, y=share_mean, ymin=share_low, ymax=share_high)) +
    geom_line(aes(color = group)) +
    geom_ribbon(aes(fill=group), alpha=0.2) +
    scale_x_continuous(breaks=seq(10, 20, 2)) +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Price ($1,000)', y='Market Share') +
    theme_bw()
share_price_plot


















