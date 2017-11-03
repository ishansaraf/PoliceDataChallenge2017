library(caret)
library(party)
theft_grid <- read.csv("10x10Grid.csv", header = TRUE)

# Splitting into training and test data
# 80% of data in training set
set.seed(42)  # Reproducability
inTrain <- createDataPartition(y = theft_grid$hour, p = 0.80,
                               list = FALSE, groups = 5)
theft_train <- theft_grid %>% slice(inTrain)
theft_test <- theft_grid %>% slice(-inTrain)

# Specify Cross-Validation
cross_validation <- trainControl(method = "cv", number = 5)

# Tuning Grid
tuning_grid <- expand.grid(mtry = seq(from = 3, to = 15, by = 1))

# Fitting random forest
trees <- train(num_thefts ~ .,
               data = theft_train,
               method = "cforest",
               trControl = cross_validation.
               tuneGrid = tuning_grid)

# Predictions
predictions <- predict(trees, newdata = theft_test)

# Confusion Matrix
confusionMatrix(predictions, theft_test$num_thefts)
