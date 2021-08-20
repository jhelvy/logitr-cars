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

# Make a basic survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 8     # Number of questions per respondent
)
head(survey) # preview

# The "range" attribute only applies to the case where "powertrain" == Electric
# To account for this, set "range" equal to 0 for cases where
# "powertrain" = Gasoline
survey <- survey %>%
  mutate(range = ifelse(powertrain == "Gasoline", 0, range))
head(survey) # preview

# Now create dummy-coded variables for each level of range
survey <- dummy_cols(survey, "range") %>% 
  select(-range_0)
head(survey) # preview

# Keep in mind now that when you run your model, you'll need to choose 
# one level for "range" as the reference level. 
