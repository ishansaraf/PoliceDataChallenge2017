#-----------------------------------------------------------------------------
# @author: Ishan Saraf, Nick Harrelson
# @date: October 29th, 2017
# This file creates a heatmap of use of force incident data in Cincinnati, OH
#-----------------------------------------------------------------------------


# Load required libraries
library(ggmap)
library(ggplot2)

# Read use of force and 911 call datasets
# Set Working directory to repository home first
force.datapath <- "./Data/PDI_Use_of_Force.csv"
calls.datapath <- "./Data/PDI_Police_Calls_For_Service__CAD_.csv"
force <- read.csv(force.datapath, header = TRUE)
cinci <- read.csv(calls.datapath, header = TRUE)

# Heatmap of Cincinnati force incidents
cinci.map <- get_map("Cincinnati Zoo & Botanical Garden", zoom = 12, maptype = 'roadmap', color = "bw")
cinci.plot <- ggmap(cinci.map) +
    geom_point(data = force, mapping = aes(x = LONGITUDE_X, y = LATITUDE_X), color = "dark green", alpha = 0.1, size = 1.1) +
    geom_density2d(data = force, mapping = aes(x = LONGITUDE_X, y = LATITUDE_X), size = 0.3) +
    stat_density2d(data = force, mapping = aes(x = LONGITUDE_X, y = LATITUDE_X, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE)
