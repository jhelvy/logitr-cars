# Make conjoint surveys using the cbcTools package

# Load libraries
library(cbcTools)
library(fastDummies)
library(tidyverse)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  price = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime = c(6, 7, 8), # 0-60 mph acceleration time (s)
  powertrain = c('gas', 'electric'),
  range_electric = c(0, 100, 150, 200, 250) # EV driving range (miles)
) %>%
  # Now restrict the range to appropriate levels based on the powertrain
  cbc_restrict(
    (powertrain == 'electric') & (range_electric == 0),
    (powertrain != 'electric') & (range_electric != 0),
  )

head(profiles) # preview

# View the profiles to confirm that range is 0 for non-electric powertrains
View(profiles)

# Note now that the balance of the powertrain levels is not even:
table(profiles_attspec$powertrain)

# Make a full-factorial design of experiment
design <- cbc_design(
  profiles = profiles_attspec,
  n_resp = 500, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  balance_by = 'powertrain' # Include since balance in profiles is not even
) %>%
  cbc_decode()

# Check to confirm balance and overlap in all levels is good
cbc_inspect(design)

# Check that range is always 0 when powertrain == "Gasoline"
design %>%
  count(range_electric, powertrain)
