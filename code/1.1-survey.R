# Make conjoint surveys using the cbcTools package

# Load libraries
library(cbcTools)
library(fastDummies)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  price = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime = c(6, 7, 8), # 0-60 mph acceleration time (s)
  powertrain = c('Gasoline', 'Hybrid', 'Electric')
)

# Make a full-factorial design of experiment
design <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8 # Number of questions per respondent
)

# Preview design
design

# Preview with powertrain dummy-coded
design |>
  cbc_encode('dummy')

# Make a labeled design with "powertrain" as the label
design_labeled <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  label = 'powertrain'
)

# Preview design
design_labeled

# Make a survey with a "no choice" option
design_no_choice <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  no_choice = TRUE
)

# Preview design
design_no_choice

# View dummy-coded version
design_no_choice |>
  cbc_encode('dummy')
