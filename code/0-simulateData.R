# Make conjoint surveys and simulate choice data using {conjointTools}

# Load libraries
library(tidyverse)
library(conjointTools)
library(readr)
library(here)

# Make basic surveys ----

# Define the attributes and levels
levels <- list(
  price       = c(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30), # Fuel economy (mpg)
  accelTime   = c(6, 7, 8),    # 0-60 mph acceleration time (s)
  electric    = c(0, 1)        # Electric vehicle (1) or gas (0)
)

# Make a full-factorial design of experiment, then recode the levels
doe <- makeDoe(levels)
doe <- recodeDesign(doe, levels)

# Set data metrics 
nResp     <- 500 # Total number of respondents (upper bound)
nAltsPerQ <- 3   # Number of alternatives per question
nQPerResp <- 8   # Number of questions per respondent

# Make a survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = nResp,
    nAltsPerQ = nAltsPerQ,    
    nQPerResp = nQPerResp  
)

# Make a survey with outside good
survey_og <- makeSurvey(
    doe       = doe,  
    nResp     = nResp,
    nAltsPerQ = nAltsPerQ,    
    nQPerResp = nQPerResp,
    outsideGood = TRUE
)

# Make attribute-specific survey ----

# Define the attributes and levels
levels_attspec <- levels
levels_attspec$range <- c(100, 150, 200, 250)

# Make a full-factorial design of experiment
doe_attspec <- makeDoe(levels_attspec)
doe_attspec <- recodeDesign(doe_attspec, levels_attspec) %>% 
  mutate(
    range = range - min(range),
    range = ifelse(electric != 1, 0, range)) %>% 
    distinct() %>% 
  rename(powertrain_Electric = electric)

# Make a basic survey
survey_attspec <- makeSurvey(
    doe       = doe_attspec,  
    nResp     = 1000, 
    nAltsPerQ = 3,  
    nQPerResp = 8   
)

# Simulate choices ----

# Simulate choices based on a utility model
data_mnl1 <- simulateChoices(
    survey = survey,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        electric    = -4.0)
)

# Choices using a different utility model
data_mnl2 <- simulateChoices(
    survey = survey,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.6,
        fuelEconomy = 0.15,
        accelTime   = -0.3,
        electric    = -1.0)
)

# Simulate choices based on a MXL utility model
data_mxl <- simulateChoices(
  survey = survey,
  altID  = "altID",
  obsID  = "obsID",
  pars = list(
    price       = -0.7,
    fuelEconomy = randN(0.1, 1),
    accelTime   = randN(-0.2, 2),
    electric    = randN(-4, 5))
)

# Simulate choices based on a utility model
data_mnl_attspec <- simulateChoices(
    survey = survey_attspec,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        powertrain_Electric = -4.0, 
        range = 0.05)
)

# Recode powertrain variable using a character
data_mnl1$powertrain <- ifelse(data_mnl1$elec == 1, 'Electric', 'Gasoline')
data_mnl2$powertrain <- ifelse(data_mnl2$elec == 1, 'Electric', 'Gasoline')
data_mxl$powertrain <- ifelse(data_mxl$elec == 1, 'Electric', 'Gasoline')

# Choices for outside good model
data_og <- simulateChoices(
    survey    = survey_og,
    altID  = "altID",
    obsID  = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        electric    = -4.0, 
        outsideGood = -15.0)
) %>% 
  rename(powertrain_Electric = electric)

# Rearrange column names
varNames <- c(
    'respID', 'obsID', 'qID', 'altID', 'choice', 'price', 'fuelEconomy',
    'accelTime')
data_mnl1 <- data_mnl1[c(varNames, 'powertrain')]
data_mnl2 <- data_mnl2[c(varNames, 'powertrain')]
data_mxl <- data_mxl[c(varNames, 'powertrain')]
data_og <- data_og[c(varNames, 'powertrain_Electric', 'outsideGood')]
data_mnl_attspec <- data_mnl_attspec[c(varNames, 'powertrain_Electric', 'range')]

# Create "2groups" data by combining half of data_mnl1 and data_mnl2
data_mnl2 <- data_mnl2 %>% 
  filter(respID <= nResp / 2) %>% 
  mutate(group = 'A')
temp <- data_mnl1 %>% 
  filter(respID > nResp / 2) %>% 
  mutate(group = 'B')
data_mnl_2groups <- rbind(data_mnl2, temp)
data_mnl_2groups$obsID <- rep(seq(nResp*nQPerResp), each = nAltsPerQ)

# Save data
write_csv(data_mnl1, here('data', 'mnl.csv'))
write_csv(data_mxl, here('data', 'mxl.csv'))
write_csv(data_mnl_2groups, here('data', 'mnl_2groups.csv'))
write_csv(data_og, here('data', 'og.csv'))
write_csv(data_mnl_attspec, here('data', 'mnl_attspec.csv'))
