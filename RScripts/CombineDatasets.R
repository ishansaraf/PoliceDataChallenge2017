#-----------------------------------------------------------------------------
# @author: Ishan Saraf, Nick Harrelson
# @date: October 29th, 2017
# This file combines the use of force and calls for service datasets.
#-----------------------------------------------------------------------------

# Loading required libraries
library(dplyr)
library(caret)
library(e1071)
library(lubridate)

# Read 911 call data
datapath <- "./Data/PDI_Police_Calls_For_Service__CAD_.csv"
cinci <- read.csv(datapath, header = TRUE)
# Read force data
forcepath <- "./Data/PDI_Use_of_Force.csv"
UOF <- read.csv(forcepath, header = TRUE)

# Get rid of empty columns, split date time into two columns
cinci <- cinci %>% mutate(INCIDENT_TYPE_DESC = NULL) %>% 
  mutate(PRIORITY_COLOR = NULL) %>% 
  mutate(AGENCY = NULL) %>%
  mutate(DATE = substr(CREATE_TIME_INCIDENT,1,10)) %>% 
  mutate(TIME = substr(CREATE_TIME_INCIDENT,12,22)) %>% 
  mutate(CREATE_TIME_INCIDENT = NULL)

# Match up use of force incidents with call incidents
matches <- cinci$EVENT_NUMBER %in% intersect(cinci$EVENT_NUMBER, UOF$CFS_NO)

# Append this list to entire dataframe
cinci <- cbind(cinci, as.data.frame(matches))
