setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")
library(tidyverse)

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
  select(obs_id, obs_type,water_body,usgs_gage,species_comm,total_count,area,Y,X,datum,obs_date)
nancydata_sel <- duncandata %>%
  select(obs_id,obs_type,water_body,usgs_gage,species_comm,total_count,area,datum,Y,X,obs_date)

# Bind datasets together and create 2 different datasets for comparing densities of only visual observations VS visual + sampled beds combined
laurancy <- bind_rows(lauradata_sel, nancydata_sel)
laurancy_vis <- laurancy %>%
  filter(obs_type == "visual")
laurancy_all <- laurancy %>%
  filter(obs_type != "shell" )

#Join laurancy and distance_area datasets
laurancy_distArea2 <- inner_join(laurancy_all, site_dist_area)

#want to visualize abundance as a function of river km with individual points colored by the nearest gage
#PROBLEM: NEED TO ASSIGN EACH OF NANCY'S POINTS TO THE NEAREST GAGE... NEED TO UPDATE SPREADSHEET ACCORDINGLY

#Change usgs_gage in dataset from character to factor
as_factor(laurancy_distArea$usgs_gage)
as_factor(laurancy_distArea2$usgs_gage)

# add column to specify whether asian clams present at site
laurancy_distArea2 <- laurancy_distArea2 %>%
  # need to figure out how to specify if "BKY" is in the column that it qualifies as "yes"!!!
  mutate(clams = ifelse(obs_id %in% c("BKY"), "yes", "no"))

SUMP <- laurancy_distArea2 %>%
  filter(water_body != "Cow Cr") 

  
ggplot(laurancy_distArea_SUMP) + 
  geom_point(aes(laurancy_distArea_SUMP$riv_dist_km, log(laurancy_distArea_SUMP$total_count))) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance on the South Umpqua River, OR") 


# This one is good! 
ggplot(laurancy_distArea_SUMP, aes(laurancy_distArea_SUMP$riv_dist_km, log(laurancy_distArea_SUMP$total_count), color = usgs_gage)) + 
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance on the South Umpqua River, OR")




ggplot(laurancy_distArea_SUMP, aes(laurancy_distArea_SUMP$riv_dist_km, log(laurancy_distArea_SUMP$total_count))) +
  geom_point(aes(color = factor(laurancy_distArea_SUMP$usgs_gage))) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance as a Function of River Distance") 




ggplot(laurancy_distArea_SUMP, aes(riv_dist_km, (total_count))) +
  geom_point(aes(color = factor(usgs_gage))) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("Mussel Abundance") + ggtitle("Mussel Abundance as a Function of River Distance") + scale_y_log10()




  #scale_color_discrete(labels("Brockway on S. Umpqua", "Riddle on Cow Cr", "Tiller on S. Umpqua"))
                       #labels("Brockway on S. Umpqua", "Riddle on Cow Cr", "Tiller on S. Umpqua"))
  
#read in slope csv
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")



