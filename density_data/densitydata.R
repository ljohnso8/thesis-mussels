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

# Bind datasets together and create 2 different datasets for comparing densities of only visual observations VS visual + sampled beds combined
density <- bind_rows(lauradata_sel, nancydata_sel)
density_vis <- density %>%
  filter(obs_type == "visual")
density_all <- density%>%
  filter(obs_type != "shell" )

#Add columns with mutate to compare different density calculations ()
density_vis <- density_vis %>%
  mutate(density1 = total_count/75) %>%
  mutate(density2 = total_count/43) %>%
  mutate(density3 = ifelse(area >= 2, total_count/area, total_count/75))

#Export density_all file
#write.table(density_all, "/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data/densityALL.txt", sep="\t", col.names = NA)

ggplot(subset(density_vis), aes(x = log(density3))) + geom_histogram(bins = 15)


hist(density_vis$density3, bin = 10)


