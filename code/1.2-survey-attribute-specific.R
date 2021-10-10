# Make conjoint surveys using the conjointTools package

# Load libraries
library(conjointTools)
library(fastDummies)
library(tidyverse)

# Define the attributes and levels
levels <- list(
  price       = c(15, 20, 25),   # Price ($1,000)
  fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
  accelTime   = c(6, 7, 8),      # 0-60 mph acceleration time (s)
  powertrain  = c("Gasoline", "Electric"), 
  range       = c(100, 150, 200, 250) # EV driving range
)

# Make a full-factorial design of experiment
doe <- makeDoe(levels)
head(doe) # preview

# Recode the design of experiment
doe <- recodeDesign(doe, levels)
head(doe) # preview

# The "range" attribute only applies to the case where
# powertrain == "Electric"
# To account for this, we'll first need to create "dummy-coded"
# variables for each type of powertrain
doe <- dummy_cols(doe, "powertrain")
head(doe) # preview

# Now we only want range to have a non-zero value when
# powertrain_Electric == 1
# So we can just multiple range by the powertrain_Electric variable
doe <- doe %>%
  mutate(range = range*powertrain_Electric)
head(doe) # preview

# Finally, remove non-unique rows
doe <- distinct(doe)
head(doe) # preview

# Make a basic survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 8     # Number of questions per respondent
)
head(survey) # preview

# Check that range is always 0 when powertrain_Gasoline == 1 
survey %>% 
  count(range, powertrain_Electric, powertrain_Gasoline)
