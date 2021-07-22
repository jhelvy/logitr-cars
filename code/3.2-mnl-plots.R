# Visualize results of estimated multinomial logit (mnl) models 

# Load libraries
library(tidyverse)
library(here)

# Read in estimated models
model <- readRDS(here("output", "mnl_model.Rds"))

# -----------------------------------------------------------------------------
# Some tips for working with model objects

# If you want to get the resulting model parameters, use the coef() function
coef(model)

# If you want to get the full summary table of the model coefficients 
# as a data frame, use coef(summary(model)) 
coef(summary(model))

# -----------------------------------------------------------------------------
# Plot results

# Define the attributes and levels
levels <- list(
  price       = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
  accelTime   = c(6, 7, 8),      # 0-60 mph acceleration time (s)
  powertrain  = c("Gasoline", "Electric")
)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price = levels$price
    distinct(level = price) %>%
    arrange(level) %>%
    mutate(utility = (level - min(level)) * coefs['price'])
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    mutate(utility = (level - min(level)) * coefs['fuelEconomy'])
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    mutate(utility = (level - min(level)) * coefs['accelTime'])
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    mutate(utility = c(coefs['powertrain_elec'], 0))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
utility = c(df_price$utility, df_fuelEconomy$utility, df_accelTime$utility, 
            df_powertrain$utility) 
ymin = min(utility)
ymax = max(utility)

# Plot the utility for each attribute
plot_price = ggplot(df_price,
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain Electric', y='Utility') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)

# Save plots 
mainwd = getwd()
setwd('./results/plots/coefficients')
ggsave('./linear_price.pdf', plot_price, width=4, height=3)
ggsave('./linear_fuelEconomy.pdf', plot_fuelEconomy, width=4, height=3)
ggsave('./linear_accelTime.pdf', plot_accelTime, width=4, height=3)
ggsave('./linear_powertrain.pdf', plot_powertrain, width=4, height=3)
ggsave('./linear_multiplot.pdf', multiplot, width=10, height=2.5)
setwd(mainwd)







# Get the model coefficients:
coefs = coef(model_partworth)
coefs

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price = data %>%
    distinct(level = price) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('price_20', 'price_25')]))
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('fuelEconomy_25', 'fuelEconomy_30')]))
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('accelTime_7', 'accelTime_8')]))
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    arrange(level) %>%
    mutate(utility = c(coefs[c('powertrain_elec')], 0))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
utility = c(df_price$utility, df_fuelEconomy$utility, df_accelTime$utility,
            df_powertrain$utility)
ymin = min(utility)
ymax = max(utility)

# Plot the utility for each attribute
plot_price = ggplot(df_price,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)

# Save plots
mainwd = getwd()
setwd('./results/plots/coefficients')
ggsave('./partworth_price.pdf', plot_price, width=4, height=3)
ggsave('./partworth_fuelEconomy.pdf', plot_fuelEconomy, width=4, height=3)
ggsave('./partworth_accelTime.pdf', plot_accelTime, width=4, height=3)
ggsave('./partworth_powertrain.pdf', plot_powertrain, width=4, height=3)
ggsave('./partworth_multiplot.pdf', multiplot, width=10, height=2.5)
setwd(mainwd)
