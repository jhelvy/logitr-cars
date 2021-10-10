# Visualize results of estimated multinomial logit (mnl) models 

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("models", "mnl.RData"))

# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(mnl_dummy)

# If you want the standard errors, use se()
se(mnl_dummy)

# If you want to get the full summary table of the model coefficients 
# as a data frame, use coef(summary(model)) 
coef(summary(mnl_dummy))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(mnl_dummy)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(
  level = c(15, 20, 25), 
  utility = c(0, coefs['price_20'], coefs['price_25']))
    
df_fuelEconomy <- data.frame(
  level = c(20, 25, 30),
  utility = c(0, coefs['fuelEconomy_25'], coefs['fuelEconomy_30']))
    
df_accelTime <- data.frame(
  level = c(6, 7, 8),
  utility = c(0, coefs['accelTime_7'], coefs['accelTime_8']))

df_powertrain = data.frame(
  level = c("Electric", "Gasoline"),
  utility = c(coefs['powertrain_Electric'], 0))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_price$utility, df_fuelEconomy$utility, 
    df_accelTime$utility, df_powertrain$utility) 
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute
plot_price <- df_price %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($1000)', y = 'Utility') +
    theme_bw()

plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'Utility') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'Utility') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
plot_mnl_dummy <- plot_grid(
  plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
  nrow = 1
)

# Save plots 
ggsave(
  filename = here('figs', 'mnl_dummy.png'), 
  plot = plot_mnl_dummy, 
  width = 10, height = 2.3
)
