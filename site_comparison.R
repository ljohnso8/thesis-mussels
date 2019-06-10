setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)
# read in csv files as tbl's
density <- as.tibble(read.csv("sump_sitecomparison.csv", header = TRUE))


density_graph <- density %>%
  filter(site != c("til02"))

ggplot(density_graph, aes(x = perc_forest, y = density, color = site)) + geom_point(size = 4) + 
  ggtitle("Western Pearlshell Densities Increase with Forest Cover") + xlab ("Percent Forest Cover (NLCD 2011)") + 
  ylab ("Mussel Density / m^2") + theme_minimal()
