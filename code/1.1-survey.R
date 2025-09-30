# Make conjoint surveys using the cbcTools package

# Load libraries
library(cbcTools)
library(fastDummies)

# Define profiles with attributes and levels
profiles <- cbc_profiles(
  price = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime = c(6, 7, 8), # 0-60 mph acceleration time (s)
  powertrain = c('Gasoline', 'Electric')
)

# Make a full-factorial design of experiment
design <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8 # Number of questions per respondent
)

head(design) # preview

head(cbc_decode(design)) # preview with powertrain not dummy-coded

# Make a labeled design with "powertrain" as the label
design_labeled <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 2, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  label = 'powertrain'
) %>%
  # Adding decoding here so that the powertrain level remains not dummy-coded
  cbc_decode()

head(design_labeled) # preview

# Make a survey with a "no choice" option
design_no_choice <- cbc_design(
  profiles = profiles,
  n_resp = 1000, # Number of respondents
  n_alts = 3, # Number of alternatives per question
  n_q = 8, # Number of questions per respondent
  no_choice = TRUE
)

head(design_no_choice) # preview

# Note that for designs with a no_choice option, all categorical variables
# MUST be dummy-coded, so cbc_decode won't work:
cbc_decode(design_no_choice)
