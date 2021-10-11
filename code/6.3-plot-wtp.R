# Visualize results of estimated WTP space model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI

# Method 1: Computed WTP from preference space model:
load(here("models", "mnl.RData")) # Load pref space model
coefs <- coef(mnl_linear)
covariance <- vcov(mnl_linear)
coef_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
wtp_draws = -1*(coef_draws[,2:4] / coef_draws[,1])
wtp_ci1 <- ci(wtp_draws)
wtp_ci1

# Method 2: Estimate WTP in WTP space model:
load(here("models", "mnl_wtp.RData")) # Load estimated models
coefs <- coef(mnl_wtp)
covariance <- vcov(mnl_wtp)
wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
wtp_ci2 <- ci(wtp_draws)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2

# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci2

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_fuelEconomy <- wtp_ci %>% filter(par == 'fuelEconomy')
wtp_accelTime <- wtp_ci %>% filter(par == 'accelTime')
wtp_powertrain <- wtp_ci %>% filter(par == 'powertrain_Electric')

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_fuelEconomy <- data.frame(level = c(20, 25, 30)) %>%
  mutate(
    diff  = level - min(level),
    mean  = diff*wtp_fuelEconomy$mean,
    lower = diff*wtp_fuelEconomy$lower,
    upper = diff*wtp_fuelEconomy$upper)

df_accelTime <- data.frame(level = c(6, 7, 8)) %>%
  mutate(
    diff  = level - min(level),
    mean  = diff*wtp_accelTime$mean,
    lower = diff*wtp_accelTime$lower,
    upper = diff*wtp_accelTime$upper)

df_powertrain <- data.frame(level = c("Gasoline", "Electric")) %>%
  mutate(
    mean  = c(0, wtp_powertrain$mean),
    lower = c(0, wtp_powertrain$lower),
    upper = c(0, wtp_powertrain$upper))

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(c(
  df_price$lower, df_fuelEconomy$lower,
  df_accelTime$lower, df_powertrain$lower)))
ymax <- ceiling(max(c(
  df_price$upper, df_fuelEconomy$upper,
  df_accelTime$upper, df_powertrain$upper)))

# Plot the WTP for each attribute *with 95% CI*
plot_fuelEconomy <- df_fuelEconomy %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Fuel Economy (mpg)', y = 'WTP ($1,000)') +
    theme_bw()

plot_accelTime <- df_accelTime %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.2) +
    geom_line() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = '0-60mph Accel. Time (sec)', y = 'WTP ($1,000)') +
    theme_bw()

plot_powertrain <- df_powertrain %>%
    ggplot(aes(x = level, y = mean, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
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
  filename = here('figs', 'mnl_wtp.png'),
  plot = plot_mnl_wtp,
  width = 8, height = 2.3
)

# -----------------------------------------------------------------------------
# Compare WTP for changes in all attributes

# WTP for:
# 10 mpg improvement in fuel economy
# 3 sec improvement in acceleration time
# Gasoline vs Electric powertrain

df_compare <- wtp_ci
cols <- c('mean', 'lower', 'upper')
df_compare[1, cols] <- df_compare[1, cols]*10 # Fuel Economy
df_compare[2, cols] <- df_compare[2, cols]*-3  # Acceleration Time
df_compare$label <- c(
  "Fuel Economy:\n+10 mpg", "Accel. Time:\n-3 sec", "Powertrain:\nElectric over Gasoline"
)

barplot_mnl_wtp <- df_compare %>% 
    ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
    geom_col(width = 0.5, fill = 'gray') +
    geom_errorbar(width = 0.3) +
    labs(x = 'WTP ($1,000)', y = 'Attribute') +
    theme_bw()

# Save plots 
ggsave(
  filename = here('figs', 'mnl_wtp_barplot.png'), 
  plot = barplot_mnl_wtp, 
  width = 5, height = 3
)
