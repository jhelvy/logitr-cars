# Compute and visualize uncertainty around estimated coefficients

# Load libraries & functions
library(here)
library(MASS)
source(here("code", "0-functions.R"))

# -----------------------------------------------------------------------------
# Compute 95% confidence interval using simulation

# Load estimated models
load(here("output", "model_mnl.RData"))

# Get the model coefficients and covariance matrix
coefs <- coef(model_linear)
covariance <- vcov(model_linear)

# Take 10,000 draws of the coefficients
draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))

# For each coefficient, get the mean and 95% confidence interval:
coef_ci <- getCI(draws, ci = 0.95)
coef_ci

# -----------------------------------------------------------------------------
# Visualize uncertainty around parameter estimates

# Separate coefficient CIs by attribute 
coef_ci$par <- row.names(coef_ci)
coef_price <- coef_ci %>% filter(par == 'price')
coef_fuelEconomy <- coef_ci %>% filter(par == 'fuelEconomy')
coef_accelTime <- coef_ci %>% filter(par == 'accelTime')
coef_powertrain <- coef_ci %>% filter(par == 'powertrainGasoline')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(level = c(15, 20, 25)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_price$mean,
        lower = diff*coef_price$lower,
        upper = diff*coef_price$upper)
    
df_fuelEconomy <- data.frame(level = c(20, 25, 30)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_fuelEconomy$mean,
        lower = diff*coef_fuelEconomy$lower,
        upper = diff*coef_fuelEconomy$upper)
    
df_accelTime <- data.frame(level = c(6, 7, 8)) %>% 
    mutate(
        diff  = level - min(level),
        mean  = diff*coef_accelTime$mean,
        lower = diff*coef_accelTime$lower,
        upper = diff*coef_accelTime$upper)
    
df_powertrain <- data.frame(level = c("Electric", "Gasoline")) %>%
    mutate(
        mean  = c(0, coef_powertrain$mean),
        lower = c(0, coef_powertrain$lower),
        upper = c(0, coef_powertrain$upper))


# Get y-axis upper and lower bounds (plots should have the same y-axis):
ymin = min(c(df_price$lower, df_fuelEconomy$lower, df_accelTime$lower, 
             df_powertrain$lower))
ymax = max(c(df_price$upper, df_fuelEconomy$upper, df_accelTime$upper, 
             df_powertrain$upper))

# Plot utility for each attribute with error bars for a 95% confidence interval
plot_price = ggplot(df_price,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() + # Adds the trend line
    geom_ribbon(alpha=0.2) + # Adds the uncertainty band
    # Set alpha between 0 and 1 for transparency level
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain, 
    aes(x=level, y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)
