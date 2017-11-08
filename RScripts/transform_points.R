transform_points <- function(df){
  df <- df %>% mutate(num_thefts = round(num_thefts))
  for(i in 1:nrow(df)){
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
}