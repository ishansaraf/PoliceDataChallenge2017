# Loading libraries and reading data
libraries <- c("dplyr", "caret", "party", "beepr", "ggplot2", "ggmap")
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

# Plugging original model into the fresh 50x50 grid
twenty_grid_orig <- read.csv("./20x20Grid.csv", header = TRUE)
new_predictions <- predict(trees, newdata = twenty_grid_orig)
twenty_grid_orig <- twenty_grid_orig %>% mutate(num_thefts = new_predictions)
# write.csv(fifty_grid, "~/github/PoliceDataChallenge2017/Data/Predicted Grid.csv")

transform_points <- function(df){
df <- df %>% mutate(num_thefts = round(num_thefts))
  for(i in 1:nrow(df)){
    print(i)
    temp_x <- rep(df$x_repr[i],df$num_thefts[i])
    temp_y <- rep(df$y_repr[i],df$num_thefts[i])
    temp_day <- rep(df$day[i],df$num_thefts[i])
    temp_hour <- rep(df$hour[i],df$num_thefts[i])
    temp_df <- data.frame(x=temp_x, y=temp_y, day = temp_day, hour = temp_hour)
    if(i == 1){
      new_data <- temp_df
    } else{
      new_data <- rbind(new_data,temp_df)
    }
  }
  return(new_data)
}

twenty_grid <- transform_points(twenty_grid_orig)
heatmapper(1,0)


# Constructing static images
seattle_google_map <- get_map("seattle", zoom = 11, maptype = "roadmap", color = "bw")
heatmapper <- function(day1, hour1) {
  filepath <- paste("./Visualizations/seattleTheftPredictionDay", day1, "Hour", hour1, ".png",sep = "")
  tempdata <- twenty_grid %>% filter(day == day1 & hour == hour1)
  theft_plot <- ggmap(seattle_google_map) +
    #geom_point(data = tempdata, mapping = aes(x = x_repr, y = y_repr), color = "dark green", alpha = 0.03) +
    geom_density2d(data = tempdata, mapping = aes(x = x, y = y), size = 0.3) +
    stat_density2d(data = tempdata, mapping = aes(x = x, y = y, fill = ..level.., alpha = ..level..),
                   size = 0.01, bins = 16, geom = "polygon") +
    scale_alpha(range = c(0, 0.3), guide = FALSE) +
    scale_fill_gradient(low = "green", high = "red") +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank())
  theft_plot
  ggsave(filepath)
}

for(i in 1:7) {
  print(i)
  for(j in 0:23) {
    print(j)
    heatmapper(day1 = i, hour1 = j)
  }
}


