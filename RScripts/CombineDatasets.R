#-----------------------------------------------------------------------------
# @author: Ishan Saraf, Nick Harrelson
# @date: October 29th, 2017
# This file combines the use of force and calls for service datasets.
#-----------------------------------------------------------------------------

# Loading required libraries
library(dplyr)
library(caret)
library(e1071)

# Read 911 call data
datapath <- "./Data/PDI_Police_Calls_For_Service__CAD_.csv"
cinci <- read.csv(datapath, header = TRUE)
# Read force data
forcepath <- "./Data/PDI_Use_of_Force.csv"
UOF <- read.csv(forcepath, header = TRUE)

# Get rid of empty columns
cinci <- cinci %>% mutate(INCIDENT_TYPE_DESC = NULL) %>% mutate(PRIORITY_COLOR = NULL)

# Match up use of force incidents with call incidents
matches <- cinci$EVENT_NUMBER %in% intersect(cinci$EVENT_NUMBER, UOF$CFS_NO)

# Append this list to entire dataframe
cinci <- cbind(cinci, as.data.frame(matches))

set.seed(20160912)

# Construct Partition
# 70% reserved for training, 30% reserved for testing.
# Balance across 5 quantile groups.
inTrain <- createDataPartition(y = cinci$matches,
                               p = 0.70,
                               list = FALSE,
                               groups = 5)

Cinci.Train.df <- cinci %>% slice(inTrain)
Cinci.Test.df <- cinci %>% slice(-inTrain)

fit.CIrf <- train(matches ~ .,
                  data=Cinci.Train.df,
                  method="cforest",
                  trControl=CIrf.cv,
                  tuneGrid=CIrf.tuning.grid)
