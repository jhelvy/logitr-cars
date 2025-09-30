# Visualize results of estimated multinomial logit (mnl) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# Load estimated models
load(here("models", "model_mnl.RData"))

# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(model_mnl)

# If you want the standard errors, use se()
se(model_mnl)

# If you want to get the full summary table of the model coefficients
# as a data frame, use coef(summary(model))
coef(summary(model_mnl))

# -----------------------------------------------------------------------------
# Plot results

# Get the estimated coefficients
coefs <- coef(model_mnl)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price <- data.frame(level = c(15, 20, 25)) %>%
  mutate(
    diff = level - min(level),
    utility = diff * coefs['price']
  )

df_price

df_fuelEconomy <- data.frame(level = c(20, 25, 30)) %>%
  mutate(
    diff = level - min(level),
    utility = diff * coefs['fuelEconomy']
  )

df_fuelEconomy

df_accelTime <- data.frame(level = c(6, 7, 8)) %>%
  mutate(
    diff = level - min(level),
    utility = diff * coefs['accelTime']
  )

df_accelTime

df_powertrain <- data.frame(level = c("Gasoline", "Electric")) %>%
  mutate(utility = c(0, coefs['powertrainElectric']))

df_powertrain

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
  df_price$utility,
  df_fuelEconomy$utility,
  df_accelTime$utility,
  df_powertrain$utility
)
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

# Plot all plots in one figure
plot_model_mnl <- plot_grid(
  plot_price,
  plot_fuelEconomy,
  plot_accelTime,
  plot_powertrain,
  nrow = 1
)

plot_model_mnl

# Save plots
ggsave(
  filename = here('figs', 'model_mnl.png'),
  plot = plot_model_mnl,
  width = 10,
  height = 2.3
)
