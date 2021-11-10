# Compute and visualize uncertainty around estimated coefficients

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)
library(maddTools)

# -----------------------------------------------------------------------------
# Compute 95% confidence interval using simulation

# Load estimated models
load(here("models", "mnl.RData"))

# Get the model coefficients and covariance matrix
coefs <- coef(mnl_linear)
covariance <- vcov(mnl_linear)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# For each coefficient, get the mean and 95% confidence interval:
coef_ci <- ci(coef_draws, ci = 0.95)
coef_ci

# -----------------------------------------------------------------------------
# Visualize uncertainty around parameter estimates
# Linear model

# Separate coefficient CIs by attribute 
coef_ci$par <- row.names(coef_ci)
coef_price <- coef_ci %>% filter(par == 'price')
coef_fuelEconomy <- coef_ci %>% filter(par == 'fuelEconomy')
coef_accelTime <- coef_ci %>% filter(par == 'accelTime')
coef_powertrain <- coef_ci %>% filter(par == 'powertrain_Electric')

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
    
df_powertrain <- data.frame(level = c("Gasoline", "Electric")) %>%
    mutate(
        mean  = c(0, coef_powertrain$mean),
        lower = c(0, coef_powertrain$lower),
        upper = c(0, coef_powertrain$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_price$lower, df_fuelEconomy$lower, 
    df_accelTime$lower, df_powertrain$lower)))
ymax <- ceiling(max(c(
    df_price$upper, df_fuelEconomy$upper, 
    df_accelTime$upper, df_powertrain$upper)))

# Plot the utility for each attribute *with 95% CI*
plot_price <- df_price %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) + # Add uncertainty band, alpha sets transparency
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($1000)', y = 'Utility') +
    theme_bw()

plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) + 
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'Utility') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) + 
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'Utility') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
plot_mnl_linear_unc <- plot_grid(
    plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    nrow = 1
)

# Save plots 
ggsave(
    filename = here('figs', 'mnl_linear_unc.png'), 
    plot = plot_mnl_linear_unc, 
    width = 10, height = 2.3
)



# -----------------------------------------------------------------------------
# Visualize uncertainty around parameter estimates
# Dummy model

# Get the model coefficients and covariance matrix
coefs <- coef(mnl_dummy)
covariance <- vcov(mnl_dummy)
draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
coef_ci <- ci(draws, ci = 0.95)
coef_ci

# Separate coefficient CIs by attribute 
coef_ci$par <- row.names(coef_ci)
coef_price_20 <- coef_ci %>% filter(par == 'price_20')
coef_price_25 <- coef_ci %>% filter(par == 'price_25')
coef_fuelEconomy_25 <- coef_ci %>% filter(par == 'fuelEconomy_25')
coef_fuelEconomy_30 <- coef_ci %>% filter(par == 'fuelEconomy_30')
coef_accelTime_7 <- coef_ci %>% filter(par == 'accelTime_7')
coef_accelTime_8 <- coef_ci %>% filter(par == 'accelTime_8')
coef_powertrain <- coef_ci %>% filter(par == 'powertrain_Electric')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(
    level = c(15, 20, 25), 
    mean = c(0, coef_price_20$mean, coef_price_25$mean),
    lower = c(0, coef_price_20$lower, coef_price_25$lower),
    upper = c(0, coef_price_20$upper, coef_price_25$upper))

df_fuelEconomy <- data.frame(
    level = c(20, 25, 30), 
    mean = c(0, coef_fuelEconomy_25$mean, coef_fuelEconomy_30$mean),
    lower = c(0, coef_fuelEconomy_25$lower, coef_fuelEconomy_30$lower),
    upper = c(0, coef_fuelEconomy_25$upper, coef_fuelEconomy_30$upper))

df_accelTime <- data.frame(
    level = c(6, 7, 8), 
    mean = c(0, coef_accelTime_7$mean, coef_accelTime_8$mean),
    lower = c(0, coef_accelTime_7$lower, coef_accelTime_8$lower),
    upper = c(0, coef_accelTime_7$upper, coef_accelTime_8$upper))

df_powertrain <- data.frame(level = c("Gasoline", "Electric")) %>%
    mutate(
        mean  = c(0, coef_powertrain$mean),
        lower = c(0, coef_powertrain$lower),
        upper = c(0, coef_powertrain$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
    df_price$lower, df_fuelEconomy$lower, 
    df_accelTime$lower, df_powertrain$lower)))
ymax <- ceiling(max(c(
    df_price$upper, df_fuelEconomy$upper, 
    df_accelTime$upper, df_powertrain$upper)))

# Plot the utility for each attribute *with 95% CI*
plot_price <- df_price %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($1000)', y = 'Utility') +
    theme_bw()

plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'Utility') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'Utility') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
plot_mnl_dummy_unc <- plot_grid(
    plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    nrow = 1
)

# Save plots 
ggsave(
    filename = here('figs', 'plot_mnl_dummy_unc.png'), 
    plot = plot_mnl_dummy_unc, 
    width = 10, height = 2.3
)
