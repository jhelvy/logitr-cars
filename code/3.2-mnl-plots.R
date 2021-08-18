# Visualize results of estimated multinomial logit (mnl) models 

# Load libraries
# library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("output", "mnl_models.RData"))

# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(model)

# If you want the standard errors, use se()
se(model)

# If you want to get the full summary table of the model coefficients 
# as a data frame, use coef(summary(model)) 
coef(summary(model))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(model)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(price = c(15, 20, 25)) %>% 
    mutate(utility = price*coefs['price'])
    
df_fuelEconomy <- data.frame(fuelEconomy = c(20, 25, 30)) %>% 
    mutate(utility = fuelEconomy*coefs['fuelEconomy'])
    
df_accelTime <- data.frame(accelTime = c(6, 7, 8)) %>% 
    mutate(utility = accelTime*coefs['accelTime'])
    
df_powertrain = data.frame(powertrain = c("Gasoline", "Electric")) %>% 
    mutate(utility = c(coefs['powertrainGasoline'], 0))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_price$utility, df_fuelEconomy$utility, 
    df_accelTime$utility, df_powertrain$utility) 
ymin <- min(utility)
ymax <- max(utility)

# Plot the utility for each attribute
plot_price <- df_price %>% 
    ggplot() +
    geom_line(aes(x = price, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($1000)', y = 'Utility') +
    theme_bw()

plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot() +
    geom_line(aes(x = fuelEconomy, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'Utility') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot() +
    geom_line(aes(x = accelTime, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'Utility') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot() +
    geom_point(aes(x = powertrain, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
grid_linear <- plot_grid(
  plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
  nrow = 1
)

# Save plots 
ggsave(here('figs', 'grid_linear.pdf'), grid_linear, width = 10, height = 2.3)
