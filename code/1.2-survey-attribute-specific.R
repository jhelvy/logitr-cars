# Make conjoint surveys using the cbcTools package

# Load libraries
library(cbcTools)
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

# The "range" attribute only applies to the case when powertrain == "Electric"
# To account for this, we have to do two things: 
# 1) we need the range to be 0 when powertrain != "Electric"
# 2) we need to subtract away the minimum range level such that the value of 
#    "0" reflects a reference level of 100 miles. Then the value for "range" 
#    would mean the *additional* range beyond 100 miles. 
doe <- doe %>% 
  mutate(
    range = range - min(range),
    range = ifelse(powertrain != "Electric", 0, range))
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
  count(range, powertrain)
