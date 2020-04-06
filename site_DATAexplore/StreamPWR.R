# Set workspace directory and bring in datasets + libraries
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")

library(tidyverse)

abundance <- as.tibble(read.csv("laurancy.csv", header = TRUE))
streampwr <- as.tibble(read.csv("streamPWR.csv", header = TRUE))
dist <- as.tibble(read.csv("SUMPpnts_distance_area.csv", header = TRUE))

#select only needed columns from streampwr
streampwr2 <- streampwr %>%
  select(site_id, av_SLPE_gradient, av_acw, Sstrpwr_2yr, Sstrpwr_5perc )

#### VISUALIZATION OF RIVER DISTANCE (KM) VS STREAM PWR (2 YR) for the South Umpqua 

# Join abundance and stream pwr datasets on site_id (need to bring in site id variable)
AbunPwr <- inner_join(abundance, streampwr, by = "site_id")

# Join AbunPwr and distance datasets by obs_id
AbunPwrDist <- inner_join(AbunPwr, dist, by = "obs_id") 

AbunPwrDist <- AbunPwrDist %>%
  filter(!usgs_gage %in% c("Riddle"))


# Visualize river distance (km) by stream pwr (2 yr)

ggplot(AbunPwrDist, aes(riv_dist_km, Sstrpwr_2yr, color = usgs_gage)) + geom_jitter() 


###### FOLLOWING CODE ONLY APPLIES TO VISUALIZATIONS INVOLVING ABUNDANCES

#filter out nancy's obs that are repeats @ k bar ranch, wiegle rd, and coffee cr
abundance2 <- abundance %>%
  filter(!obs_id %in% c("MAFA_CoffeCr1", "MAFA_KBarRanch", "MAFA_WiegleRd"))%>%
# filter out shell records
  filter(!obs_type == "shell")

#Join abundance and streampwr datasets
abunpwr <- abundance2 %>%
  inner_join(streampwr2, by = "site_id")

# Do initial visualization with streampwr2 and laurancy 
ggplot(abunpwr, aes(Sstrpwr_2yr, log(total_count), color = usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))


# No way to get around the fact that 90,000 mussels at TIL03 really obscures the graph... see what it looks like
# with this value removed
abunpwr %>%
  filter(!obs_id == "TIL0301") %>%
  ggplot(aes(Sstrpwr_2yr, log(total_count), color = usgs_gage)) + geom_jitter() 

#Now do the same thing with 5 perc flow Sstrpwr

ggplot(abunpwr, aes(Sstrpwr_5perc, log(total_count), color= usgs_gage)) + geom_jitter() +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, aes(group=1))


# No way to get around the fact that 90,000 mussels at TIL03 really obscures the graph... see what it looks like
# with this value removed
abunpwr %>%
  filter(!obs_id == "TIL0301") %>%
  ggplot(aes(Sstrpwr_5perc, log(total_count), color = usgs_gage)) + geom_jitter() 

# my points seem off... too many observations at log(x) value 6.... see what log(total_count) looks like


