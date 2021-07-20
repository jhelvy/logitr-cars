# Load libraries
source(here::here("code", "0-setup.R"))

# -----------------------------------------------------------------------------
# Functions for adding the outside good

addOutsideGood <- function(data) {
    numObs  = max(data$obsID)
    numResp = max(data$respID)
    numAlts = max(data$alt)
    data_outsideGood = 0*data[1:numObs,]
    data_outsideGood$outsideGood = 1
    data$outsideGood = 0
    data_outsideGood$order = seq(numAlts, (nrow(data) + (numAlts-1)), numAlts)
    data$order = seq(1, nrow(data), 1)
    data       = rbind(data, data_outsideGood)
    data       = data[order(data$order),]
    numAlts    = numAlts + 1
    data       = addMetaData(data, numObs, numResp, numAlts)
    data$order <- NULL
    return(data)
}

addMetaData <- function(data, numObs, numResp, numAlts) {
    varNames    = colnames(data)
    varNames    = varNames[which(! varNames %in% c('respID', 'obsID', 'alt'))]
    numQ        = numObs / numResp
    data$respID = rep(seq(1, numResp), each=numQ*numAlts)
    data$obsID  = rep(seq(1, numObs), each=numAlts)
    data$alt    = rep(seq(1, numAlts), numObs)
    row.names(data) = seq(nrow(data))
    data = data[c('respID', 'obsID', 'alt', varNames)]
    return(data)
}

# -----------------------------------------------------------------------------
# Simulate data using conjointTools package

# Define the attributes and levels
levels <- list(
  price       = seq(15, 20, 25), # Price ($1,000)
  fuelEconomy = c(20, 25, 30),   # Fuel economy (mpg)
  accelTime   = c(6, 7, 8),      # 0-60 mph acceleration time (s)
  elec        = c(0, 1)          # Electric vehicle (1) or gas (0)
)

# Make a full-factorial design of experiment, then recode the levels
doe <- makeDoe(levels)
doe <- recodeDesign(doe, levels)

# Make the survey
survey <- makeSurvey(
    doe       = doe,  # Design of experiment
    nResp     = 1000, # Total number of respondents (upper bound)
    nAltsPerQ = 3,    # Number of alternatives per question
    nQPerResp = 8     # Number of questions per respondent
)

# Make survey with outside good
survey_og = addOutsideGood(survey)

# Simulate choices based on a utility model
data_mnl1 <- simulateChoices(
    survey    = survey,
    altIDName = "altID",
    obsIDName = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        elec        = -4.0)
)
# Choices using a different utility model
data_mnl2 <- simulateChoices(
    survey    = survey,
    altIDName = "altID",
    obsIDName = "obsID",
    pars = list(
        price       = -0.6,
        fuelEconomy = 0.15,
        accelTime   = -0.3,
        elec        = -1.0)
)
# Choices for outside good model
data_og <- simulateChoices(
    survey    = survey_og,
    altIDName = "altID",
    obsIDName = "obsID",
    pars = list(
        price       = -0.7,
        fuelEconomy = 0.1,
        accelTime   = -0.2,
        elec        = -4.0, 
        outsideGood = -15.0)
)

# Recode powertrain variable using a character
data_mnl1$powertrain <- ifelse(data_mnl1$elec == 1, 'elec', 'gas')
data_mnl2$powertrain <- ifelse(data_mnl2$elec == 1, 'elec', 'gas')
data_og$powertrain <- ifelse(data_og$elec == 1, 'elec', 'gas')

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
write_csv(data_mnl1, here::here('data', 'data_mnl.csv'))
write_csv(data_mnl_2groups, here::here('data', 'data_mnl_2groups.csv'))
write_csv(data_og, here::here('data', 'data_og.csv'))
