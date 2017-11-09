library(dplyr)

#assumes a square grid
grid_size <- 20

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

#constructs data frame of grid
df <- data.frame(x_index,y_index,left_x, right_x, bottom_y, top_y)

empty_grid <- function(grid, hour1, day1){
  new_grid <- grid %>% mutate(x_repr = (left_x+right_x)/2) %>% mutate(y_repr = (top_y+bottom_y)/2) %>% mutate(orig_index = NULL) %>%
    mutate(x_index = NULL) %>% mutate(y_index = NULL) %>% mutate(left_x = NULL) %>% mutate(right_x = NULL) %>%
    mutate(bottom_y = NULL) %>% mutate(top_y = NULL) %>% mutate(hour = hour1) %>% mutate(day = day1) %>% mutate(num_thefts = 0)
  new_grid
}

for(day in 1:7){
  for(hour1 in 0:23){
    add_theft <- empty_grid(df,hour1,day)
    
    if(day == 1 & hour1 == 0){
      fresh_grid <- add_theft
    } else{
      fresh_grid <- rbind(fresh_grid,add_theft)
    }
  }
}

write.csv(fresh_grid, "./20x20Grid.csv")
