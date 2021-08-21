# Plot market simulation results

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)

# Load estimated models
load(here("sims", "mnl_linear.RData"))

# Bar plot of shares (with 95% CI) 
sim_mnl_linear %>% 
    mutate(label = c("Conventional", "Electric", "Hybrid")) %>% 
    ggplot(aes(x = label, y = prob_mean, ymin = prob_low, ymax = prob_high)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x='Alternative', y='Market Share') +
    theme_bw()

# Save plot
ggsave(
    filename = here('figs', 'sim.png'), 
    width = 4, height = 3.5
)
