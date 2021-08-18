# Make conjoint surveys and simulate choice data using {conjointTools}

# Load libraries
library(conjointTools)
library(readr)
library(here)

# Define the attributes and levels
levels <- list(
  price       = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
  accelTime   = c(6, 7, 8),      # 0-60 mph acceleration time (s)
  elec        = c(0, 1)          # Electric vehicle (1) or gas (0)
)

# Make a full-factorial design of experiment, then recode the levels
doe <- makeDoe(levels)
doe <- recodeDesign(doe, levels)

# Make a survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 8     # Number of questions per respondent
)

# Make a survey with outside good
survey_og <- makeSurvey(
    doe       = doe,  
    nResp     = 1000, 
    nAltsPerQ = 3,  
    nQPerResp = 8,
    outsideGood = TRUE
)

# Simulate choices based on a utility model
data_mnl1 <- simulateChoices(
    survey = survey,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        elec        = -4.0)
)

# Choices using a different utility model
data_mnl2 <- simulateChoices(
    survey    = survey,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.6,
        fuelEconomy = 0.15,
        accelTime   = -0.3,
        elec        = -1.0)
)

# Choices for outside good model
data_og <- simulateChoices(
    survey    = survey_og,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        elec        = -4.0, 
        outsideGood = -15.0)
)

# Recode powertrain variable using a character
data_mnl1$powertrain <- ifelse(data_mnl1$elec == 1, 'Electric', 'Gasoline')
data_mnl2$powertrain <- ifelse(data_mnl2$elec == 1, 'Electric', 'Gasoline')
data_og$powertrain <- ifelse(data_og$elec == 1, 'Electric', 'Gasoline')

# Rearrange column names
varNames <- c(
    'respID', 'obsID', 'qID', 'altID', 'choice', 'price', 'fuelEconomy',
    'accelTime', 'powertrain')
data_mnl1 <- data_mnl1[varNames]
data_mnl2 <- data_mnl2[varNames]
data_og <- data_og[c(varNames, 'outsideGood')]

# Create "2groups" data by combining data_mnl1 and data_mnl2
data_mnl2$group <- 'B' 
temp <- data_mnl1
temp$group <- 'A'
data_mnl_2groups <- rbind(temp, data_mnl2)

# Save data
write_csv(data_mnl1, here('data', 'data_mnl.csv'))
write_csv(data_mnl_2groups, here('data', 'data_mnl_2groups.csv'))
write_csv(data_og, here('data', 'data_og.csv'))
