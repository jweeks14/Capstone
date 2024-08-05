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
# drop ARRDELAY column
ontime_data <- select(ontime_data, -ARRDELAY)
summary(ontime_data) # see updated summary statistics

head(ontime_data) # view first few rows
View(ontime_data) # view file

# group by YEAR, MONTH, and CARRIER
ontime_grouped <- group_by(ontime_data, YEAR, MONTH, CARRIER)
View(ontime_grouped)

summarize(ontime_grouped, count = n())

# aggregate using mean for CANCELLED, DIVERTED & aggregate using median for DELAY types
ontime_aggregated <- summarize(ontime_grouped,
                                    mean_CANCELLED = mean(CANCELLED, na.rm = T),
                                    mean_DIVERTED = mean(DIVERTED, na.rm = T),
                                    median_DEPDELAY = median(DEPDELAY, na.rm = T),
                                    median_CARRIERDELAY = median(CARRIERDELAY, na.rm = T),
                                    median_WEATHERDELAY = median(WEATHERDELAY, na.rm = T),
                                    median_NASDELAY = median(NASDELAY, na.rm = T), 
                                    median_SECURITYDELAY = median(SECURITYDELAY, na.rm = T),
                                    median_LATEAIRCRAFTDELAY = median(LATEAIRCRAFTDELAY, na.rm = T))
View(ontime_aggregated) 
# considering using a less-aggregated file for analysis, this resulting dataframe has only 124 rows
# having now kept the data from 2018-2021 (instead of sticking to only 2018), the resulting dataframe has 484 rows

########## Mishandled Data Preprocessing ###########
# revisit mishandled_data_by_year and think of possibilities
head(mishandled_missing_by_year)

# Do we need the passenger column? One possibility is to drop that column, and keep the data from 2018-2021.
  # This option would mean restoring the column YEAR, and filtering out the 2016/2017 observations.
# Another possibility is to change the level of aggregation. 

# Other options for dealing with the passenger column: drop NA values, drop passenger column entirely, bootstrap additional values
# Filter out the data from 2016 and 2017 (no ontime data to match it), and remove the passenger column
mishandled_no_pass <- mishandled_data %>%
  filter(!(YEAR %in% c(2016, 2017))) %>%
  select(-PASSENGERS)
head(mishandled_no_pass)

########## Mishandled / Ontime Data Merge ##########
# merge MISHANDLED/ONTIME by YEAR/MONTH/CARRIER
combined_data <- inner_join(mishandled_no_pass, ontime_aggregated, by = c("YEAR", "MONTH", "CARRIER"))
View(combined_data)
# merged dataset only has about 100 rows at this point - will revisit so we have a more sizable dataset (8/3)
# merged dataset with edits now has 625 rows - better, but still not ideal (8/4)

