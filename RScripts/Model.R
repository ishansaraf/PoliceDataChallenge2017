# Loading libraries and reading data
libraries <- c("dplyr", "caret", "party", "beepr")
lapply(libraries, require, character.only = TRUE)
start.time <- Sys.time()
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
tuning_grid <- expand.grid(mtry = seq(from = 1, to = 5, by = 1))

# Fitting random forest
trees <- train(num_thefts ~ .,
               data = theft_train,
               method = "cforest",
               trControl = cross_validation,
               tuneGrid = tuning_grid)

# Predictions
predictions <- predict(trees, newdata = theft_test)

# Confusion Matrix
# confusionMatrix(predictions, theft_test$num_thefts)

# Mean Squared Error
trees

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
beep(8)