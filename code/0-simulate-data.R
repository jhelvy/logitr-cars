# Make conjoint surveys and simulate choice data using {cbcTools}

# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)

# Make basic surveys ----

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  price = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime = c(6, 7, 8), # 0-60 mph acceleration time (s)
  powertrain = c('gas', 'electric')
)

# Make a full-factorial design of experiment
design <- cbc_design(
  profiles = profiles,
  n_resp = 500, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8 # Number of questions per respondent
)

# View version of design with powertrain not dummy-coded
cbc_decode(design)

# Make survey with no_choice option ----

design_no_choice <- cbc_design(
  profiles = profiles,
  n_resp = 500, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  no_choice = TRUE
)


# Make attribute-specific survey ----

profiles_attspec <- cbc_profiles(
  price = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime = c(6, 7, 8), # 0-60 mph acceleration time (s)
  powertrain = c('gas', 'electric'),
  # Note that we include 0 in the range below for the non-EV powertrains
  range_electric = c(0, 100, 150, 200, 250) # EV driving range (miles)
) %>%
  # Now restrict the range to appropriate levels based on the powertrain
  cbc_restrict(
    (powertrain == 'electric') & (range_electric == 0),
    (powertrain != 'electric') & (range_electric != 0),
  )

# View the profiles to confirm that range is 0 for non-electric powertrains
View(profiles_attspec)

# Note now that the balance of the powertrain levels is not even:
table(profiles_attspec$powertrain)

# Make a full-factorial design of experiment
design_attspec <- cbc_design(
  profiles = profiles_attspec,
  n_resp = 500, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  balance_by = 'powertrain' # Include since balance in profiles is not even
)

# Check to confirm balance and overlap in all levels is good
cbc_inspect(cbc_decode(design_attspec))

# Simulate choices ----

# Simulate choices based on a utility model

priors <- cbc_priors(
  profiles = profiles,
  price = -0.7,
  fuelEconomy = 0.1,
  accelTime = -0.2,
  powertrain = -4.0
)

data_mnl1 <- cbc_choices(
  design = design,
  priors = priors
)

# Choices using a different set of priors

priors2 <- cbc_priors(
  profiles = profiles,
  price = -0.6,
  fuelEconomy = 0.15,
  accelTime = -0.3,
  powertrain = -1.0
)

data_mnl2 <- cbc_choices(
  design = design,
  priors = priors2
)

priors_random <- cbc_priors(
  profiles = profiles,
  price = -0.7,
  fuelEconomy = rand_spec(
    dist = "n",
    mean = 0.1,
    sd = 1
  ),
  accelTime = rand_spec(
    dist = "n",
    mean = -0.2,
    sd = 2
  ),
  powertrain = rand_spec(
    dist = "n",
    mean = -4,
    sd = 5
  )
)

# Simulate choices based on a MXL utility model
data_mxl <- cbc_choices(
  design = design,
  priors = priors_random
)

# Simulate choices for no_choice design
priors_no_choice <- cbc_priors(
  profiles = profiles,
  price = -0.7,
  fuelEconomy = 0.1,
  accelTime = -0.2,
  powertrain = -4.0,
  no_choice = -15.0
)

data_no_choice <- cbc_choices(
  design = design_no_choice,
  priors = priors_no_choice
)

# Simulate choices for alternative-specific attribute design
priors_attspec <- cbc_priors(
  profiles = profiles_attspec,
  price = -0.7,
  fuelEconomy = 0.1,
  accelTime = -0.2,
  powertrain = -4.0,
  range_electric = 0.02
)

data_attspec <- cbc_choices(
  design = design_attspec,
  priors = priors_attspec
)


# Rearrange column names
varNames <- c(
  'profileID',
  'respID',
  'obsID',
  'qID',
  'altID',
  'choice',
  'price',
  'fuelEconomy',
  'accelTime',
  'powertrainelectric'
)
data_mnl1 <- data_mnl1[varNames]
data_mnl2 <- data_mnl2[varNames]
data_mxl <- data_mxl[varNames]
data_no_choice <- data_no_choice[c(varNames, 'no_choice')]
data_attspec <- data_attspec[c(varNames, 'range_electric')]

# Create "2groups" data by combining half of data_mnl1 and data_mnl2
data_mnl2 <- data_mnl2 %>%
  filter(respID <= 500 / 2) %>%
  mutate(group = 'A')
temp <- data_mnl1 %>%
  filter(respID > 500 / 2) %>%
  mutate(group = 'B')
data_mnl_2groups <- rbind(data_mnl2, temp)
data_mnl_2groups$obsID <- rep(seq(500 * 8), each = 3)

# Save data
write_csv(data_mnl1, here('data', 'mnl.csv'))
write_csv(data_mxl, here('data', 'mxl.csv'))
write_csv(data_mnl_2groups, here('data', 'mnl_2groups.csv'))
write_csv(data_no_choice, here('data', 'no_choice.csv'))
write_csv(data_attspec, here('data', 'mnl_attspec.csv'))
