# Explore choice data

# Load libraries
library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here('data', 'mnl.csv'))
head(data)

# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas

# Look at counts of values in data:
data %>% count(price)
data %>% count(fuelEconomy)
data %>% count(accelTime)
data %>% count(powertrain)

# Count number of times each alternative was chosen:
data %>% count(altID, choice)

# Check if any respondents made the same choice for all questions:
data %>% 
    filter(choice == 1) %>%
    count(respID, altID) %>% 
    filter(n == 8)

# Visualize how many times each price level was chosen:
data %>% count(price, choice)
data %>%
    ggplot() + 
    geom_col(aes(x = as.factor(price), y = choice))

# Visualize how many times electric cars were chosen compared to gasoline cars:
data %>% count(powertrain, choice)
data %>%
    ggplot() + 
    geom_col(aes(x = as.factor(powertrain), y = choice))
