library(dplyr)

#assumes a square grid
grid_size <- 10

#assigns indexes to grid spaces
#origin is at bottom left
x_index <- rep(seq(from = 1, to = grid_size, by = 1),grid_size)
y_index <- sort(rep(seq(from = 1, to = grid_size, by = 1),grid_size), decreasing = FALSE)

#bounds on city
left_bound <- -122.5515
right_bound <- -122.112
top_bound <- 47.75391
bottom_bound <- 47.45762

#calculates left and right bounds for each grid space
x_bounds <- seq(left_bound, right_bound, length = grid_size+1)
left_x <- rep(x_bounds[1:grid_size],grid_size)
right_x <- rep(x_bounds[2:(grid_size+1)],grid_size)

#calculates top and bottom bounds for each grid space
y_bounds <- seq(bottom_bound, top_bound, length = grid_size+1)
bottom_y <- sort(rep(y_bounds[1:grid_size],grid_size), decreasing = FALSE)
top_y <- sort(rep(y_bounds[2:(grid_size+1)],grid_size), decreasing = FALSE)

#initializes 0 incidents in each element
num_thefts <- rep(0,grid_size^2)
orig_index <- c(1:grid_size^2)
#constructs data frame of grid
df <- data.frame(x_index,y_index,left_x, right_x, bottom_y, top_y, num_thefts, orig_index)


#function to append grid for a specific time bucket (latitude and longitude of incidents) on a specific day
append_grid <- function(grid, theft_bucket, hour1, day1){
  for(i in 1:nrow(theft_bucket)){
    x <- grid$x_index[min(which(theft_bucket$Longitude[i] > grid$left_x & theft_bucket$Longitude[i] < grid$right_x))]
    y <- grid %>% filter(x_index == x) %>% filter(theft_bucket$Latitude[i] < top_y & theft_bucket$Latitude[i] > bottom_y)
    y = y$y_index
    row <- grid %>% filter(x_index %in% x) %>% filter(y_index %in% y)
    grid$num_thefts[row$orig_index] <- grid$num_thefts[row$orig_index] + 1
  }
  grid %>% mutate(x_repr = (left_x+right_x)/2) %>% mutate(y_repr = (top_y+bottom_y)/2) %>% mutate(orig_index = NULL) %>%
    mutate(x_index = NULL) %>% mutate(y_index = NULL) %>% mutate(left_x = NULL) %>% mutate(right_x = NULL) %>%
    mutate(bottom_y = NULL) %>% mutate(top_y = NULL) %>% mutate(hour = hour1) %>% mutate(day = day1)
  grid
}

empty_grid <- function(grid, hour1, day1){
  grid %>% mutate(x_repr = (left_x+right_x)/2) %>% mutate(y_repr = (top_y+bottom_y)/2) %>% mutate(orig_index = NULL) %>%
    mutate(x_index = NULL) %>% mutate(y_index = NULL) %>% mutate(left_x = NULL) %>% mutate(right_x = NULL) %>%
    mutate(bottom_y = NULL) %>% mutate(top_y = NULL) %>% mutate(hour = hour1) %>% mutate(day = day1)
  grid
}

thefts <- read.csv('TheftData.csv')

for(day in 1:7){
  print('day:')
  print(day)
  for(hour1 in 0:23){
    print('hour:')
    print(hour1)
    bucket <- thefts %>% filter(Weekday == day) %>% filter(Hour == hour1)
    if(nrow(bucket) != 0){
      add_theft <- append_grid(df,bucket, hour1, day)
    } else{
      add_theft <- empty_grid(df,hour1,day)
    }
    
    if(day == 1 & hour1 == 0){
      theft_grid <- add_theft
    } else{
      theft_grid <- rbind(theft_grid,add_theft)
    }
  }
}