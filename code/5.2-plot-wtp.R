# Visualize results of estimated WTP space model

# Load libraries
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("output", "model_wtp.RData"))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(model_wtp)

# Create data frames for plotting each attribute:
#   level = The attribute level (x-axis)
#   wtp   = The wtp (in $1,000) associated with each level (y-axis)
df_fuelEconomy <- data.frame(level = c(20, 25, 30)) %>% 
    mutate(wtp = coefs['fuelEconomy']*(level - min(level)))
    
df_accelTime <- data.frame(level = c(6, 7, 8)) %>% 
    mutate(wtp = coefs['accelTime']*(level - min(level)))
    
df_powertrain = data.frame(level = c("Electric", "Gasoline")) %>% 
    mutate(wtp = c(0, coefs['powertrainGasoline']))

# Get upper and lower bounds (plots should have the same y-axis)
wtp <- c(df_fuelEconomy$wtp, df_accelTime$wtp, df_powertrain$wtp) 
ymin <- floor(min(wtp))
ymax <- ceiling(max(wtp))

# Plot the WTP for each attribute
plot_fuelEconomy <- df_fuelEconomy %>% 
    ggplot() +
    geom_line(aes(x = level, y = wtp)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'WTP ($1,000)') +
    theme_bw()

plot_accelTime <- df_accelTime %>% 
    ggplot() +
    geom_line(aes(x = level, y = wtp)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'WTP ($1,000)') +
    theme_bw()

plot_powertrain <- df_powertrain %>% 
    ggplot() +
    geom_point(aes(x = level, y = wtp)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Powertrain', y = 'WTP ($1,000)') +
    theme_bw()

# Plot all plots in one figure
plot_mnl_wtp <- plot_grid(
  plot_fuelEconomy, plot_accelTime, plot_powertrain,
  nrow = 1
)

# Save plots 
ggsave(
  filename = here('figs', 'plot_mnl_wtp.png'), 
  plot = plot_mnl_wtp, 
  width = 8, height = 2.3)
