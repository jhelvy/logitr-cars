# Make conjoint surveys and simulate choice data using {cbcTools}

# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)

# Make basic surveys ----

# Define profiles with attributes and levels
profiles <- cbc_profiles(
    price       = c(15, 20, 25), # Price ($1,000)
    fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
    accelTime   = c(6, 7, 8),    # 0-60 mph acceleration time (s)
    electric    = c(0, 1)        # Electric vehicle (1) or gas (0)
)

# Make a full-factorial design of experiment 
design <- cbc_design(
    profiles = profiles,
    n_resp   = 500, # Number of respondents
    n_alts   = 3,   # Number of alternatives per question
    n_q      = 8    # Number of questions per respondent
)

# Make a survey with no choice option
design_nochoice <- cbc_design(
    profiles = profiles,
    n_resp   = 500, # Number of respondents
    n_alts   = 3,   # Number of alternatives per question
    n_q      = 8,   # Number of questions per respondent
    no_choice = TRUE
)

# Make attribute-specific survey ----

profiles_attspec <- cbc_profiles(
    price       = c(15, 20, 25), # Price ($1,000)
    fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
    accelTime   = c(6, 7, 8),    # 0-60 mph acceleration time (s)
    electric    = c(0, 1),       # Electric vehicle (1) or gas (0)
    range       = c(100, 150, 200, 250) # Driving range (miles)
) %>% 
    mutate(
        range = range - min(range),
        range = ifelse(electric != 1, 0, range)) %>% 
    rename(powertrain_Electric = electric) %>% 
    # Now remove any duplicate rows and re-label the profileIDs
    distinct() %>% 
    mutate(profileID = seq(n()))

# Make a full-factorial design of experiment
design_attspec <- cbc_design(
    profiles = profiles_attspec,
    n_resp   = 500, # Number of respondents
    n_alts   = 3,   # Number of alternatives per question
    n_q      = 8    # Number of questions per respondent
)

# Simulate choices ----

# Simulate choices based on a utility model

data_mnl1 <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        electric    = -4.0
    )
)

# Choices using a different utility model
data_mnl2 <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price       = -0.6,
        fuelEconomy = 0.15,
        accelTime   = -0.3,
        electric    = -1.0
    )
)

# Simulate choices based on a MXL utility model
data_mxl <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price       = -0.7,
        fuelEconomy = randN(0.1, 1),
        accelTime   = randN(-0.2, 2),
        electric    = randN(-4, 5)
    )
)

# Simulate choices based on a utility model
data_mnl_attspec <- cbc_choices(
    design = design_attspec,
    obsID = "obsID",
    priors = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        powertrain_Electric = -4.0, 
        range = 0.05
    )
)

# Recode powertrain variable using a character
data_mnl1$powertrain <- ifelse(data_mnl1$elec == 1, 'Electric', 'Gasoline')
data_mnl2$powertrain <- ifelse(data_mnl2$elec == 1, 'Electric', 'Gasoline')
data_mxl$powertrain <- ifelse(data_mxl$elec == 1, 'Electric', 'Gasoline')

# Choices for outside good model
data_nochoice <- cbc_choices(
    design = design_nochoice,
    obsID = "obsID",
    priors = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        electric    = -4.0, 
        no_choice   = -15.0
    )
) %>% 
  rename(powertrain_Electric = electric)

# Rearrange column names
varNames <- c(
    'respID', 'obsID', 'qID', 'altID', 'choice', 'price', 'fuelEconomy',
    'accelTime')
data_mnl1 <- data_mnl1[c(varNames, 'powertrain')]
data_mnl2 <- data_mnl2[c(varNames, 'powertrain')]
data_mxl <- data_mxl[c(varNames, 'powertrain')]
data_nochoice <- data_nochoice[c(varNames, 'powertrain_Electric', 'no_choice')]
data_mnl_attspec <- data_mnl_attspec[c(varNames, 'powertrain_Electric', 'range')]

# Create "2groups" data by combining half of data_mnl1 and data_mnl2
data_mnl2 <- data_mnl2 %>% 
  filter(respID <= 500 / 2) %>% 
  mutate(group = 'A')
temp <- data_mnl1 %>% 
  filter(respID > 500 / 2) %>% 
  mutate(group = 'B')
data_mnl_2groups <- rbind(data_mnl2, temp)
data_mnl_2groups$obsID <- rep(seq(500*8), each = 3)

# Save data
write_csv(data_mnl1, here('data', 'mnl.csv'))
write_csv(data_mxl, here('data', 'mxl.csv'))
write_csv(data_mnl_2groups, here('data', 'mnl_2groups.csv'))
write_csv(data_nochoice, here('data', 'nochoice.csv'))
write_csv(data_mnl_attspec, here('data', 'mnl_attspec.csv'))

