# Data Preprocessing 
# Team Alpha: Jess and Tracey

########## Initial Setup ###########
# define paths
# PROJ_PATH <- "/cloud/project" # path for Posit
PROJ_PATH <- "/Users/jessweeks/Documents/Capstone" # Jess's local working directory
# PROJ_PATH <- "C:\\Users\\trace\\OneDrive\\Documents\\Capstone\\Capstone-Repo-Shared" # Tracey's local working directory
setwd(PROJ_PATH)

# load libraries 
library(tidyverse)

# on-time data constants
ONTIME_DATA_FILE <- "Cleaned_On_Time_Marketing_Carrier_On_Time_Performance_January_2018_December_2021.csv"
ONTIME_FEATURES <- c("YEAR", "MONTH", "CARRIER", "CARRIERDELAY", "WEATHERDELAY", "LATEAIRCRAFTDELAY",
                     "SECURITYDELAY", "NASDELAY", "CANCELLED", "DIVERTED", "ARRDELAY", "DEPDELAY")

# baggage data constants
BAGGAGE_DATA_FILE <- "Commercial_Aviation_-_Mishandled_Baggage_and_Mishandled_Wheelchairs_and_Scooter_20240717.csv"
BAGGAGE_FEATURES <- c("YEAR", "MONTH", "CARRIER", "PASSENGERS", "MISHANDLED_BAGGAGE")

# load flight delay dataset as CSV 
ontime_data <- read_csv(ONTIME_DATA_FILE)  
View(ontime_data)

# load mishandled baggage data as CSV
mishandled_data <- read_csv(BAGGAGE_DATA_FILE)
View(mishandled_data)

########## On-Time Data Preprocessing ##########
# Passenger data is missing from other years in the mishandled_data file, to the point where dropping the values would be more effective than imputing them.
# filter for 2018
ontime_2018 <- filter(ontime_data, YEAR == 2018) %>%
  select(-c(YEAR, ARRDELAY))
summary(ontime_2018) # updated summary statistics

# drop YEAR, ARRDELAY columns
# ontime_2018 <- select(ontime_data, -c(YEAR, ARRDELAY))
head(ontime_2018) # view first few rows
View(ontime_2018) # view file

# group by MONTH and CARRIER ***
ontime_2018_grouped <- group_by(ontime_2018, MONTH, CARRIER)
View(ontime_2018_grouped)

# aggregate using mean for CANCELLED, DIVERTED & aggregate using median for DELAY types
ontime_2018_aggregated <- summarize(ontime_2018_grouped,
                                    mean_CANCELLED = mean(CANCELLED, na.rm = T),
                                    mean_DIVERTED = mean(DIVERTED, na.rm = T),
                                    median_DEPDELAY = median(DEPDELAY, na.rm = T),
                                    median_CARRIERDELAY = median(CARRIERDELAY, na.rm = T),
                                    median_WEATHERDELAY = median(WEATHERDELAY, na.rm = T),
                                    median_NASDELAY = median(NASDELAY, na.rm = T), 
                                    median_SECURITYDELAY = median(SECURITYDELAY, na.rm = T),
                                    median_LATEAIRCRAFTDELAY = median(LATEAIRCRAFTDELAY, na.rm = T))
View(ontime_2018_aggregated) 
# considering using a less-aggregated file for analysis, this resulting dataframe has only 124 rows

########## Mishandled Data Preprocessing ###########
# filter for the year 2018, drop YEAR column
mishandled_2018 <- filter(mishandled_data, YEAR == 2018) %>%
  select(-YEAR)
View(mishandled_2018)

########## Mishandled / Ontime Data Merge ##########
# merge MISHANDLED/ONTIME by MONTH/CARRIER
combined_data <- inner_join(mishandled_2018, ontime_2018_aggregated, by = c("MONTH", "CARRIER"))
View(combined_data)
# merged dataset only has about 100 rows at this point - will revisit so we have a more robust dataset! 
