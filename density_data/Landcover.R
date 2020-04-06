setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")
library(tidyverse)
library(cowplot)

#read in datasets (need laura + nancy abundance, huc12 key, huc12 landcover, drainage basin landcover)

laurancy <- as.tibble(read.csv("laurancy.csv", header = TRUE))
hucKey <- as.tibble(read_csv("siteID_huc12_key.csv", col_names = TRUE))

DBlandcover <- as.tibble(read_csv("Landcover_drainagebasin.csv", col_names = TRUE))

HUClandcover <- as.tibble(read_csv("Landcover_huc12.csv", col_names = TRUE))

# Join hucKey and laurancy data sets
laurancy_huc <- inner_join(laurancy, hucKey, by = "site_id")

# Join laurancy_huc and HUClandcover datasets
laurancy_huc_landcover <- left_join(laurancy_huc, HUClandcover, by = "huc12_id")

laurancy_huc_landcover2 <- laurancy_huc_landcover %>%
  select(obs_id, site_id.x, obs_type, water_body, usgs_gage, total_count, huc12_id, prc_frst, prc_OPN, 
         prc_dvlp, prc_Tdvlp, prc_ag, prc_TH) 

#Visualize % agriculture vs abundance at the HUC12 level

hucag <- ggplot(laurancy_huc_landcover2, aes(prc_ag, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % open developed vs abundance at the HUC12 level

huc_Odvlp <- ggplot(laurancy_huc_landcover2, aes(prc_OPN, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % developed (low-high intensity) vs abundance at the HUC12 level
huc_dvlp <- ggplot(laurancy_huc_landcover2, aes(prc_dvlp, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % developed (all categories) vs abundance at the HUC12 level
huc_Tdvlp <- ggplot(laurancy_huc_landcover2, aes(prc_Tdvlp, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % TH vs abundance at the HUC12 level

hucTH <- ggplot(laurancy_huc_landcover2, aes(prc_TH, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  xlab("HUC12 % Timber Harvest")

#Visualize % forest vs abundance at HUC12 level

hucfrst <- ggplot(laurancy_huc_landcover2, aes(prc_frst, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  xlab("HUC12 % Forest")

plot_grid(huc_Odvlp, huc_Tdvlp, hucTH, hucag)

# OK... now look at these trends on the drainage basin level

# Join laurancy_huc and DBlandcover datasets

laurancy_DBlandcover <- left_join(laurancy, DBlandcover, by = "site_id")
laurancy_DBlandcover2 <- laurancy_DBlandcover %>%
  select(obs_id, site_id, obs_type, water_body, usgs_gage, total_count, prc_frst, prc_OPN, 
         prc_dvlp, prc_Tdvlp, prc_ag, prc_TH)

#Visualize % agriculture vs abundance at the drainage basin (DB) level

dbag <- ggplot(laurancy_DBlandcover2, aes(prc_ag, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))


#Visualize % open developed vs abundance at the DB level

db_Odvlp <- ggplot(laurancy_DBlandcover2, aes(prc_OPN, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % developed (low-high intensity) vs abundance at the HUC12 level
db_dvlp <- ggplot(laurancy_DBlandcover2, aes(prc_dvlp, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % developed (all categories) vs abundance at the HUC12 level
db_Tdvlp <- ggplot(laurancy_DBlandcover2, aes(prc_Tdvlp, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))

#Visualize % TH vs abundance at DB level

dbTH <- ggplot(laurancy_DBlandcover2, aes(prc_TH, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  xlab("Drainage Basin % Timber Harvest")

#Visualize % forest vs abundance at db level

dbfrst <- ggplot(laurancy_DBlandcover2, aes(prc_frst, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  xlab("Drainage Basin % Forest")
  
# Are percent scrub-shrub and percent forest positively correlated? (Answer: NO! Negatively correlated!!!)

dbTHfrst <- ggplot(laurancy_DBlandcover2, aes(prc_frst, prc_TH)) + geom_jitter()+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +
  xlab("Drainage Basin (DB) % Forest") + ylab("DB % TH")

hucTHfrst <- ggplot(laurancy_huc_landcover2, aes(prc_frst, prc_TH)) + geom_jitter()+
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +
  xlab("HUC12 % Forest") + ylab("HUC12 % TH")

plot_grid(db_Odvlp, db_dvlp, dbag, dbTH)

plot_grid(hucTH, dbTH, ncol = 1, align = "v")

plot_grid(hucfrst, dbfrst, hucTHfrst, dbTHfrst)
