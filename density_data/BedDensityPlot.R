setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")

library(tidyverse)
library(cowplot)
library(base)

#Bring in file with all mussel lengths for all surveyed beds
count <- as.tibble(read.csv("BedDensityPlot.csv"), colnames = TRUE)

# Filter count df to only include BKY01 (and not other BKY beds)
count2 <- count %>%
  filter(!bed_id %in% c("BKY02", "BKY04")) %>%
# Filter to only include M. falcata records
  filter(!species %in% c("Anodonta sp."))

# Density plot of mussel lengths by bed
density_plot <- ggplot(count2, aes(length, fill = bed_id, color = bed_id)) + geom_density(alpha = 0.1) + theme_classic()
density_plot2 <- density_plot + geom_vline(xintercept = 3) + geom_vline(xintercept = 10)
# Count plot of mussel lengths by bed
count_plot <- ggplot(count2, aes(length, fill = bed_id, color = bed_id)) + geom_density(aes(y = stat(count)), alpha = 0.1) + theme_classic()
count_plot2 <- count_plot + geom_vline(xintercept = 3)
# Aggregate all BKY sites into one aggregated bed to see how this changes visualization of density plots
count$bed_id <- as.character(count$bed_id) #Change bed_id variable to a character 
count3 <- count %>% 
  mutate(bedid = ifelse(grepl("BKY", count$bed_id), "Brockway", count$bed_id)) %>% #change all BKY sites to general Brockway identifier
  filter(!species %in% c("Anodonta sp."))
  
# Density plot of mussel lengths by bed using aggregated brockway
AGGdensity_plot <- ggplot(count3, aes(length, fill = bedid, color = bedid)) + geom_density(alpha = 0.1) + theme_classic()

# Count plot of mussel lengths by bed using aggregated brockway
AGGcount_plot <- ggplot(count3, aes(length, fill = bedid, color = bedid)) + geom_density(aes(y = stat(count)), alpha = 0.1) + theme_classic()

  
  
  
  
BedDensityPlot <- plot_grid(density_plot, count_plot, AGGdensity_plot, AGGcount_plot)

save_plot("BedDensityPlot.jpeg", BedDensityPlot, ncol = 2, nrow = 2, base_height = 4,
         base_width = 8)

BedDensityPlot2 <- plot_grid(density_plot2, count_plot2)
save_plot("BedDensityPlot2.jpeg", BedDensityPlot2, ncol = 1, nrow = 2, base_height = 2,
          base_width = 8)
                                                                          