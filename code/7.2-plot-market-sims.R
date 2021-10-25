# Plot market simulation results

# Load libraries & functions
library(tidyverse)
library(here)

# Load simulation results
load(here("sims", "mnl_linear.RData"))

# Bar plot of probabilities for single market simulation (with 95% CI) 
sim_mnl_linear %>% 
    mutate(label = c("Conventional", "Electric", "Hybrid")) %>% 
    ggplot(aes(
        x = label, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Share') +
    theme_bw()

# Save plot
ggsave(
    filename = here('figs', 'sim.png'), 
    width = 4, height = 3.5
)

# Bar plot of probabilities for multiple market simulations (with 95% CI) 
sim_mnl_linear_multi %>%
    ggplot(aes(
        x = as.factor(altID), y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    facet_wrap(~obsID) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Share') +
    theme_bw()

# Save plot
ggsave(
    filename = here('figs', 'sim_multi.png'), 
    width = 8, height = 7
)
