# Set workspace directory and bring in datasets + libraries
setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")

path <- "/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore"

library(tidyverse)

#Bring in datasets

streampwr <- as.tibble(read.csv("streamPWR.csv", header = TRUE))

streampwr2 <- streampwr %>%
  select(site_id, av_SLPE_gradient, av_acw, Sstrpwr_2yr, Sstrpwr_5perc) %>%
  mutate(av_SLPE = mean(av_SLPE_gradient), av_ACW = mean(av_acw), 
         av_2yr = mean(Sstrpwr_2yr), av_5perc = mean(Sstrpwr_5perc)) %>%
  mutate(slope_compr = (((av_SLPE_gradient - .00171124)/ .00171124))*100) %>%
  mutate(ACW_compr = (((av_acw - 108.5586)/ 108.5586))*100) %>%
  mutate(strpwr2yr_compr = (((Sstrpwr_2yr - 132.6874)/ 132.6874))*100) %>%
  mutate(strpwr5perc_compr = (((Sstrpwr_5perc - 27.73297)/ 27.73297))*100) 

# Write streampwr2 to csv
write_excel_csv(streampwr2, "/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore/strpwr_sens.csv", 
                col_names = TRUE)

#Visualize 

ggplot(streampwr2, aes(slope_compr)) + geom_histogram(binwidth = 2)

ggplot(streampwr2, aes(ACW_compr)) + geom_histogram(binwidth = 2)

ggplot(streampwr2, aes(strpwr2yr_compr)) + geom_histogram(binwidth = 2)

ggplot(streampwr2, aes(strpwr5perc_compr)) + geom_histogram(binwidth = 2)
