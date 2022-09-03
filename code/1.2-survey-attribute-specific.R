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

head(profiles) # preview

# The "range" attribute only applies to the case when powertrain == "Electric"
# To account for this, we have to do two things: 
# 1) we need the range to be 0 when powertrain != "Electric"
# 2) we need to subtract away the minimum range level such that the value of 
#    "0" reflects a reference level of 100 miles. Then the value for "range" 
#    would mean the *additional* range beyond 100 miles. 
profiles <- profiles %>% 
    mutate(
        range = range - min(range),
        range = ifelse(powertrain != "Electric", 0, range)) %>% 
    # Now remove any duplicate rows and re-label the profileIDs
    distinct() %>% 
    mutate(profileID = seq(n()))

head(profiles) # preview

# Make a full-factorial design of experiment 
design <- cbc_design(
    profiles = profiles,
    n_resp   = 1000, # Number of respondents
    n_alts   = 3,    # Number of alternatives per question
    n_q      = 8     # Number of questions per respondent
)

head(design) # preview

# Check that range is always 0 when powertrain == "Gasoline"
design %>% 
  count(range, powertrain)
