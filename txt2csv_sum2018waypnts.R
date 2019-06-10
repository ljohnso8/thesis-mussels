setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)

# Take text file of waypoints that has way too many unnecessary columns and par down to relevant data,
# then export as a csv file
way_points <- read.delim("summer2018_mussel_waypoints.txt")
waypoints <- as.tibble(waypoints)
waypoints %>%
  select(ident:comment, time) %>%
  select(-y_proj, -x_proj) %>%
  separate(time, into = c("date", "time"), sep = " ") %>%
  write.csv(file = "/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data/summer2018_waypnts.csv")

