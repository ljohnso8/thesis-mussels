setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")
library(tidyverse)

#Bring in needed files... site/abundance info, stream power info, land cover info
abundance <- as.tibble(read.csv("laurancy.csv", header = TRUE))
streampwr <- as.tibble(read.csv("ord_streamPWR.csv", header = TRUE))
landDB <- as.tibble(read.csv("Landcover_drainagebasin.csv", header = TRUE))
landhuc12 <- as.tibble(read.csv("Landcover_huc12.csv", header = TRUE))

# create simple dataframe with only observation id and abundance information (use site id and abundance 0 for sites where 
###     there were no mussel observations)

obsabun <- abundance %>%
  select(obs_id, total_count) %>%
  filter(obs_id != "ZERO_CowCr")
  

write.table(obsabun, "ord_obsabun.csv", sep = ",", col.names = TRUE)

# Create environmental variable data frame in wide format
# Do stream power first... need to make sure rows match the order as it will appear on the simple dataframe site id/
  # abundance key



# Option 1: Stream power variables and land use variables separate 


