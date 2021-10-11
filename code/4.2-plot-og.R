# Visualize results of estimated multinomial logit (mnl) models 

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("models", "mnl_og.RData"))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(mnl_og)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(level = c(15, 20, 25)) %>% 
    mutate(utility = coefs['price']*(level - min(level)))
    
df_fuelEconomy <- data.frame(level = c(20, 25, 30)) %>% 
    mutate(utility = coefs['fuelEconomy']*(level - min(level)))
    
df_accelTime <- data.frame(level = c(6, 7, 8)) %>% 
    mutate(utility = coefs['accelTime']*(level - min(level)))
    
df_powertrain = data.frame(level = c("Gasoline", "Electric")) %>%
    mutate(utility = c(0, coefs['powertrain_Electric']))

df_og = data.frame(level = c("No", "Yes")) %>% 
    mutate(utility = c(0, coefs['outsideGood']))

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_price$utility, df_fuelEconomy$utility, 
    df_accelTime$utility, df_powertrain$utility, df_og$utility) 
ymin <- floor(min(utility))
ymax <- ceiling(max(utility))

# Plot the utility for each attribute
plot_price <- df_price %>% 
    ggplot() +
    geom_line(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Price ($1000)', y = 'Utility') +
    theme_bw()

plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot() +
    geom_line(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'Utility') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot() +
    geom_line(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'Utility') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'Utility') +
    theme_bw()

plot_og <- df_og %>% 
    ggplot() +
    geom_point(aes(x = level, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Outside Good', y = 'Utility') +
    theme_bw()

# Plot all plots in one figure
plots_og <- plot_grid(
  plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, plot_og,
  nrow = 1
)

# Save plots 
ggsave(
  filename = here('figs', 'mnl_og.png'), 
  plot = plots_og, 
  width = 12, height = 2.3
)
