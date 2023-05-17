# Plot sensitivity simulation results

# Load libraries & functions
library(tidyverse)
library(here)
library(jph)

# Load simulation results
load(here("sims", "sens_price_mnl_linear.RData"))

# -----------------------------------------------------------------------------
# Make a line plot of the market sensitivity to price (with uncertainty)

share_price_plot <- 
    sens_price %>% 
    ggplot(aes(
        x = price, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_price %>% filter(price <= 25, price >= 15), 
        linetype = "solid") +
    expand_limits(x = c(10, 30), y = c(0, 1)) +
    labs(x = 'Price ($1,000)', y = 'Market Share') +
    theme_bw()

share_price_plot

# Save plot
ggsave(
    filename = here('figs', 'share_price_plot.png'), 
    plot = share_price_plot,
    width = 5, height = 4
)

# -----------------------------------------------------------------------------
# Make a line plot of the revenue sensitivity to price (with uncertainty)

marketSize <- 1000
rev_data <- sens_price %>%
    mutate(
        rev_mean = price*marketSize*predicted_prob / 10^3, # Convert to millions
        rev_low  = price*marketSize*predicted_prob_lower / 10^3,
        rev_high = price*marketSize*predicted_prob_upper / 10^3)

rev_price_plot <- rev_data %>% 
    ggplot(aes(x = price, y = rev_mean, ymin = rev_low, ymax = rev_high)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = rev_data %>% filter(price <= 25, price >= 15), 
        linetype = "solid") +
    # expand_limits(x = c(10, 30), y = c(0, 1)) +
    labs(x = 'Price ($ Thousand)', y = 'Revenue ($ Million)') +
    theme_bw()

rev_price_plot

# Save plot
ggsave(
    filename = here('figs', 'rev_price_plot.png'), 
    plot = rev_price_plot,
    width = 5, height = 4
)

# -----------------------------------------------------------------------------
# Make a tornado diagram to show market sensitivity to multiple

labels <- data.frame( 
    attribute = c('price', 'fuelEconomy', 'accelTime'), 
    label = c('Price ($1,000)', 'Fuel Economy (mpg)', '0-60 mph Acceleration Time')
)

tornado_data <- sens_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')

tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'value', 
    result = 'predicted_prob'
) 

# Change the fill colors, adjust labels
tornado_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) + 
    labs(x = 'Market Share', y = 'Attribute')

# Save plot
ggsave(
    filename = here('figs', 'tornado_plot.png'), 
    plot = tornado_plot,
    width = 5, height = 3
)
