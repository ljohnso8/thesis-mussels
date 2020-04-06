#Set working directory and bring in libraries
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")
library(tidyverse)
library(rgdal)

# Bring in usgs file with utm coordinates
usgsData <- as.tibble(read_csv("USGS_barLocations.csv", col_names = TRUE))

# Add column specifying datum is NAD83
usgsData <- usgsData %>%
  add_column(datum = "NAD83")

# convert from UTM to lat/long (keep datum the same)
utmcoord <- SpatialPoints(cbind(usgsData$utm_east, usgsData$utm_north),
                          proj4string = CRS("+proj=utm +zone=10 +datum=NAD83"))
longlatcoord <- spTransform(utmcoord, CRS("+proj=longlat"))

#Add the lat/long results to the usgs data
usgsData$X <- coordinates(longlatcoord)[,1]
usgsData$Y <- coordinates(longlatcoord)[,2]

#export file... keeps doing this weird thing by adding a column of numbered row id's but I can't figure out 
# how to get it to stop doing it and it is real easy to fix in excel so I just dealt with it that way
write.table(usgsData, "/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore/usgsData.txt",sep="\t", col.names = NA)





