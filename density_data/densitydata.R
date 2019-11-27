#set working directory and bring in libraries
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")
library(tidyverse)

#read in csv of my and Nancy's data
duncandata <- as.tibble(read_tsv("LatLongduncandata.txt", col_names = TRUE)) %>%
  rename(obs_num = X1) %>%
  rename(datum = Datum)
lauradata <- as.tibble(read_csv("SUMPdensitydata.csv", col_names = TRUE)) %>%
  rename(Y = dec_lat) %>%
  rename(X = dec_long)

# Create new tibbles for each data set that only represent needed columns
lauradata_sel <- lauradata %>%
  select(obs_id, obs_type,water_body,usgs_gage,species_comm,total_count,area,Y,X,datum,obs_date)
nancydata_sel <- duncandata %>%
  select(obs_id,obs_type,water_body,species_comm,total_count,area,datum,Y,X,obs_date)

# Join datasets together
density <- left_join(lauradata_sel, nancydata_sel, by = "obs_id")
