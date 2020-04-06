setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")
library(tidyverse)
library(cowplot)

slopeData <- as.tibble(read_csv("sump_slope.csv", col_names = TRUE))

#read in csv of my and Nancy's data PLUS site distance/basin area file
duncandata <- as.tibble(read_tsv("LatLongduncandata.txt", col_names = TRUE)) %>%
  rename(obs_num = X1) %>%
  rename(datum = Datum)
lauradata <- as.tibble(read_csv("SUMP_densitydata_2018.csv", col_names = TRUE)) %>%
  rename(Y = dec_lat) %>%
  rename(X = dec_long)

site_dist_area <- as.tibble(read_csv("SUMPpnts_distance_area.csv", col_names = TRUE))




# Create new tibbles for each data set that only represent needed columns
lauradata_sel <- lauradata %>%
  select(obs_id, site_id, obs_type,water_body,usgs_gage,species_comm,total_count,area,Y,X,datum,obs_date)
nancydata_sel <- duncandata %>%
  select(obs_id, site_id, obs_type,water_body,usgs_gage,species_comm,total_count,area,datum,Y,X,obs_date)

slope_sel <- slopeData %>%
  select(obs_id, slope) %>%
#Need to rename obs_id to site_id
  rename("site_id" = obs_id)

slope_exp <- slopeData %>%
  rename("site_id" = obs_id) %>%
  select(-(US_lwr:max_rise)) %>%
  select(-(slope:notes)) 
write.csv(slope_exp, file = "/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore/slope_exp.csv")

  

# Bind Nancy and Laura's datasets together 
laurancy <- bind_rows(lauradata_sel, nancydata_sel)

#export laurancy as a .csv file
write.csv(laurancy, file = "/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore/laurancy.csv")

# Bind together laurancy and slope_sel
#GRRRR.... except this is not working!!!
laurancy_slope <- bind_rows(laurancy, slope_sel)

# I actually think I need to do a join. I think primary keys in laurancy are obs_id + site_id. I think foreign key in
# slope_sel is site_id. Verify that each of these keys uniquely id's every observation in the data set.

laurancy %>%
  count(obs_id,site_id) %>%
  filter(n > 1)

slope_sel %>%
  count(site_id) %>%
  filter(n > 1)
# OK... so since both return a tibble with no rows it means every record is unique within them

#Join laurancy and slope_sel datasets
laurancy_slope <- laurancy %>%
  inner_join(slope_sel, by = "site_id")

# Yay! It worked!!!
#Now select by area (using gage as operator) and visualize log(abundance) as a function of slope

BKY <- laurancy_slope %>%
  filter(usgs_gage == "Brockway")

BKY_slopePlot <- BKY %>%
  ggplot(aes(slope, total_count)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)

TIL <- laurancy_slope %>%
  filter(usgs_gage == "Tiller")

TIL_slopePlot <- TIL %>%
  ggplot(aes(slope, total_count)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x)

COW <- laurancy_slope %>%
  filter(usgs_gage == "Riddle")

COW_slopePlot <- COW %>%
  ggplot(aes(slope, total_count)) + geom_jitter() 

#Plot all together now
plot_grid(BKY_slopePlot, TIL_slopePlot, COW_slopePlot)
 






