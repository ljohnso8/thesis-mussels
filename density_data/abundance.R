setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")
library(tidyverse)

#read in csv of my and Nancy's data PLUS site distance/basin area file
duncandata <- as.tibble(read_csv("LatLongduncandataV2.csv", col_names = TRUE)) %>%
  rename(obs_num = X1) %>%
  rename(datum = Datum)
lauradata <- as.tibble(read_csv("SUMP_densitydata_2018.csv", col_names = TRUE)) %>%
  rename(Y = dec_lat) %>%
  rename(X = dec_long)
site_dist_area <- as.tibble(read_csv("SUMPpnts_distance_area.csv", col_names = TRUE))


# Create new tibbles for each data set that only represent needed columns
lauradata_sel <- lauradata %>%
  dplyr::select(obs_id, obs_type,water_body,usgs_gage,species_comm,total_count,area,Y,X,datum,obs_date)
nancydata_sel <- duncandata %>%
  dplyr::select(obs_id,obs_type,water_body,usgs_gage,species_comm,total_count,area,datum,Y,X,obs_date)

# Bind datasets together and create 2 different datasets for comparing densities of only visual observations VS visual + sampled beds combined
laurancy <- bind_rows(lauradata_sel, nancydata_sel)
laurancy_vis <- laurancy %>%
  filter(obs_type == "visual")
laurancy_all <- laurancy %>%
  filter(obs_type != "shell" )

# Write laurancy file to folders (4/8/2020: I wrote this file and then subsequently modified it to an updated format that 
##### is different than the one created below: I added site_id column)
#write.table(laurancy, file = "laurancy.csv", sep = ",", col.names = TRUE)
          

#Join laurancy and distance_area datasets
laurancy_distArea2 <- inner_join(laurancy_all, site_dist_area)

#want to visualize abundance as a function of river km with individual points colored by the nearest gage
#PROBLEM: NEED TO ASSIGN EACH OF NANCY'S POINTS TO THE NEAREST GAGE... NEED TO UPDATE SPREADSHEET ACCORDINGLY

#Change usgs_gage in dataset from character to factor
as_factor(laurancy_distArea2$usgs_gage)
as_factor(laurancy_distArea2$usgs_gage)

# add column to specify whether asian clams present at site 
laurancy_distArea2 <- laurancy_distArea2 %>%
  #specify that if "BKY" is in the column that it qualifies as "yes"!!!
  mutate(clams = ifelse(grepl("BKY", laurancy_distArea2$obs_id), "yes", "no"))

# change values in 'clams' column for unknown sites to 'unknown'
laurancy_distArea2[23, "clams"] <- "unknown"
laurancy_distArea2[25, "clams"] <- "unknown"
laurancy_distArea2[26, "clams"] <- "unknown"
laurancy_distArea2[27, "clams"] <- "unknown"
laurancy_distArea2[28, "clams"] <- "yes"
laurancy_distArea2[29, "clams"] <- "yes"



  
SUMP <- laurancy_distArea2 %>%
  filter(water_body != "Cow Cr") %>%
  filter(obs_id != "MAFA_KBarRanch" & obs_id != "MAFA_WiegleRd" & obs_id != "MAFA_CoffeCr1" & obs_id != "MAFA_BoomerHill")

#Export SUMP df to csv for use in ordination script
write.csv(SUMP,"/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore/SUMP.csv", row.names = FALSE)

#Created different subset of data to focus only on which sites were at which distance to compare to graph
SUMP2 <- SUMP %>%
  select(obs_id, total_count, riv_dist_km) 

SUMP2 <- SUMP2[order(SUMP2$riv_dist_km),]
  


  
ggplot(SUMP) + 
  geom_point(aes(SUMP$riv_dist_km, log(SUMP$total_count))) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance on the South Umpqua River, OR") 


# This one is good! Abundance VS River Distance w/ Points colored by gage
ggplot(SUMP, aes(SUMP$riv_dist_km, log(SUMP$total_count), color = usgs_gage)) + 
  geom_point() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance on the South Umpqua River, OR")

# Abundance VS River Distance w/ Points colored by clams status
riverDistplot <- ggplot(SUMP, aes(SUMP$riv_dist_km, SUMP$total_count, color = clams)) + 
  geom_point() +
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("Mussel Abundance") + ggtitle("Mussel Abundance & Invasive Asian Clam Presence \nat Sites on the South Umpqua River, OR") + 
  scale_y_log10() 

riverDistplot + labs(color = "Asian clams \npresent")


ggplot(SUMP, aes(SUMP$riv_dist_km, log(SUMP$total_count))) +
  geom_point(aes(color = factor(SUMP$usgs_gage))) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("log(Mussel Abundance)") + ggtitle("Mussel Abundance as a Function of River Distance") 




ggplot(SUMP, aes(riv_dist_km, (total_count))) +
  geom_point(aes(color = factor(usgs_gage))) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") + 
  ylab("Mussel Abundance") + ggtitle("Mussel Abundance as a Function of River Distance") + scale_y_log10()




  #scale_color_discrete(labels("Brockway on S. Umpqua", "Riddle on Cow Cr", "Tiller on S. Umpqua"))
                       #labels("Brockway on S. Umpqua", "Riddle on Cow Cr", "Tiller on S. Umpqua"))
  
#read in slope csv
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")



