#assumes a square grid
grid_size <- 3

#assigns indexes to grid spaces
#origin is at bottom left
x_index <- rep(seq(from = 1, to = grid_size, by = 1),grid_size)
y_index <- sort(rep(seq(from = 1, to = grid_size, by = 1),grid_size), decreasing = FALSE)

#bounds on city
left_bound <- 0
right_bound <- 10
top_bound <- 10
bottom_bound <- 0

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


#function to append grid for a specific time bucket (lat and long of incidents) on a specific day
append_grid <- function(grid, theft_bucket, hour, day){
  for(i in 1:length(theft_bucket)){
    x <- grid$x_index[min(which(theft_bucket$long[i] > grid$left_x))]
    y <- grid$x_index[min(which(theft_bucket$lat[i] > grid$left_x)) & grid$x_index == x]
    row <- grid %>% filter(x_index == x) %>% filter(y_index == y)
    grid$num_thefts[row$orig_index] <- grid$num_thefts[row$orig_index] + 1
  }
  grid
}