library(dplyr)
library(ggmap)
seattle <- read.csv("Seattle_Police_Department_911_Incident_Response.csv", header = TRUE)

# Cleaning dataset of irrelevant columns
drop_columns <- c("CAD.CDW.ID", "General.Offense.Number", "CAD.Event.Number", "Hundred.Block.Location", "Zone.Beat",
                  "District.Sector", "Census.Tract", "Incident.Location", "Event.Clearance.Code")
seattle <- seattle[, !(names(seattle) %in% drop_columns)]

# Filtering dataset to only have the calls for service for calls determined to be dealing with thefts, etc.
theft_groups <- c("ROBBERY", "BURGLARY", "AUTO THEFTS", "SHOPLIFTING")
theft_sub_groups <- c("RESIDENTIAL BURGLARIES", "THEFT", "ROBBERY", "AUTO THEFTS")
theft_descriptions <- c("THEFT - MISCELLANEOUS", "SHOPLIFT", "BURGLARY - RESIDENTIAL, OCCUPIED", "THEFT - CAR PROWL",
                        "BURGLARY - RESIDENTIAL, UNOCCUPIED", "STRONG ARM ROBBERY", "BURGLARY - COMMERCIAL", "AUTO THEFT",
                        "THEFT - AUTO ACCESSORIES", "ARMED ROBBERY", "BURGLARY - UNOCCUPIED STRUCTURE ON RESIDENTIAL PROPERTY")
thefts <- seattle %>% filter(Event.Clearance.Group %in% theft_groups |
                               Event.Clearance.SubGroup %in% theft_sub_groups |
                               Event.Clearance.Description %in% theft_descriptions)

# Visualizing theft locations
seattle_google_map <- get_map("seattle", zoom = 11, maptype = "roadmap", color = "bw")
theft_plot <- ggmap(seattle_google_map) +
  geom_point(data = thefts, mapping = aes(x = Longitude, y = Latitude), color = "dark green", alpha = 0.03) +
  geom_density2d(data = thefts, mapping = aes(x = Longitude, y = Latitude), size = 0.3) +
  stat_density2d(data = thefts, mapping = aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
      size = 0.01, bins = 16, geom = "polygon") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  scale_fill_gradient(low = "green", high = "red") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.line = element_blank())
theft_plot
