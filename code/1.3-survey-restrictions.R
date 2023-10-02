# Make conjoint surveys using the cbcTools package

# Load libraries
library(cbcTools)
library(fastDummies)
library(tidyverse)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
    price       = c(15, 20, 25),   # Price ($1,000)
    fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
    accelTime   = c(6, 7, 8),      # 0-60 mph acceleration time (s)
    powertrain  = c("Gasoline", "Electric"), 
    range       = c(100, 150, 200, 250) # EV driving range
)

head(profiles)
dim(profiles)

# Let's say we think $15,000 for an EV is unrealistic since most EVs 
# cost more. To add that restriction, use cbc_restrict:

profiles <- profiles %>% 
    cbc_restrict(
        powertrain == "Electric" & price == 15
    )

head(profiles)
dim(profiles)

# Make a full-factorial design of experiment 
design <- cbc_design(
    profiles = profiles,
    n_resp   = 1000, # Number of respondents
    n_alts   = 3,    # Number of alternatives per question
    n_q      = 8     # Number of questions per respondent
)

head(design) # preview

# Check that the Electric powertrain never has 15 for price:
design %>% 
  count(price, powertrain)

