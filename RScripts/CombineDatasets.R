library(dplyr)
library(caret)
library(e1071)

# read 911 call data
datapath <- "C:/Users/harreln/Desktop/Police Data Challenge/cinci_calls.csv"
cinci <- read.csv(datapath, header = TRUE)
# read force data
forcepath <- "C:/Users/harreln/Desktop/Police Data Challenge/use_of_force.csv"
UOF <- read.csv(forcepath, header = TRUE)

# get rid of empty columns
cinci <- cinci %>% mutate(INCIDENT_TYPE_DESC = NULL) %>% mutate(PRIORITY_COLOR = NULL)

# match up use of force incidents with call incidents
matches <- cinci$EVENT_NUMBER %in% intersect(cinci$EVENT_NUMBER, UOF$CFS_NO) 

#append this list to entire dataframe
cinci <- cbind(cinci, as.data.frame(matches))

set.seed(20160912)

# Construct Partition
# 70% reserved for training, 30% reserved for testing.
# Balance across 5 quantile groups.
inTrain <- createDataPartition(y = cinci$matches,
                               p = 0.70,
                               list = FALSE,
                               groups = 5)

Cinci.Train.df <- cinci %>%
  slice(inTrain)
Cinci.Test.df <- cinci %>%
  slice(-inTrain)

fit.CIrf <- train(matches ~ .,
                  data=Cinci.Train.df,
                  method="cforest",
                  trControl=CIrf.cv,
                  tuneGrid=CIrf.tuning.grid)

