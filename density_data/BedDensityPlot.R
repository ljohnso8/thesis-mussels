setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/density_data")

library(tidyverse)
library(cowplot)
library(base)

#Bring in file with all mussel lengths for all surveyed beds
count <- as.tibble(read.csv("BedDensityPlot.csv"), colnames = TRUE)

# 9/21/20: Want to know percent of juveniles as function of all mussels measured in lower brockway sites (to increase
#           sample size and make it more representative of the area)
count_bky <- count %>%
  filter(grepl("BKY", bed_id))

count_bky %>%
  #mutate(length, n = n()) %>%
  filter(length <= 3.0)

countrandom <- count %>%
  filter(bed_id == "TIL03") %>%
  mutate(mean_length = mean(length)) %>%
  mutate(med_length = median(length))

myvars <- names(mydata) %in% c("v1", "v2", "v3")


# Filter count df to only include BKY01 (and not other BKY beds)
count2 <- count %>%
  filter(!bed_id %in% c("BKY02", "BKY04")) %>%
# Filter to only include M. falcata records
  filter(!species %in% c("Anodonta sp."))

# Creater subset of only Anodonta bed
count3 <- count %>%
  filter(species == "Anodonta sp.") %>%
  filter(length >= 7.0)

# Density plot of mussel lengths by bed
density_plot <- ggplot(count2, aes(length, fill = bed_id, color = bed_id)) + geom_density(alpha = 0.1) + theme_classic()
density_plot2 <- density_plot + geom_vline(xintercept = 3) + scale_x_continuous(name = "Length (cm)",breaks = seq(0,13,1)) + 
  #guides(color = FALSE, fill = FALSE)
  scale_fill_discrete(breaks=c("BKY01","TIL03","COW01"), labels = c("Lower S. Umpqua", "Upper S. Umpqua", "Cow Cr")) +
  scale_y_continuous(name = "Density") + 
  guides(color = FALSE) + theme(legend.title=element_blank()) #+scale_fill_discrete(labels = c("Lower S. Umpqua", "Cow Creek", "Upper South Umpqua")) 
  

# Count plot of mussel lengths by bed
count_plot <- ggplot(count2, aes(length, fill = bed_id, color = bed_id)) + geom_density(aes(y = stat(count)), alpha = 0.1) + theme_classic()
count_plot2 <- count_plot + geom_vline(xintercept = 3) + scale_x_continuous(name = "Length (cm)",breaks = seq(0,13,1)) + 
  scale_fill_discrete(breaks=c("BKY01","TIL03","COW01"), labels = c("Lower S. Umpqua", "Upper S. Umpqua", "Cow Cr")) +
  scale_y_continuous(name = "Count") + 
  guides(color = FALSE) + theme(legend.title=element_blank())

# Aggregate all BKY sites into one aggregated bed to see how this changes visualization of density plots
count$bed_id <- as.character(count$bed_id) #Change bed_id variable to a character 
count3 <- count %>% 
  mutate(bedid = ifelse(grepl("BKY", count$bed_id), "Brockway", count$bed_id)) %>% #change all BKY sites to general Brockway identifier
  filter(!species %in% c("Anodonta sp."))
  
# Density plot of mussel lengths by bed using aggregated brockway
AGGdensity_plot <- ggplot(count3, aes(length, fill = bedid, color = bedid)) + geom_density(alpha = 0.1) + theme_classic()

# Count plot of mussel lengths by bed using aggregated brockway
AGGcount_plot <- ggplot(count3, aes(length, fill = bedid, color = bedid)) + geom_density(aes(y = stat(count)), alpha = 0.1) + theme_classic()

###### Anodonta figure
Ano_DENplot <- ggplot(count3, aes(length)) + geom_density(alpha = 0.1) + theme_classic() 
Ano_DenPlot2 <- Ano_DENplot + geom_vline(xintercept = 1) + scale_x_continuous(name = "Length (cm)",breaks = seq(0,8,1))




ANO_CNTPLOT <- ggplot(count3, aes(length, fill = bed_id, color = bed_id)) + geom_density(aes(y = stat(count)), alpha = 0.1) + theme_classic()
  
plot_grid(Ano_DENplot, ANO_CNTPLOT)
  
BedDensityPlot <- plot_grid(density_plot2, count_plot2)

save_plot("BedDensityPlot_Take3.jpeg", BedDensityPlot, ncol = 2, nrow = 1, base_height = 4,
        base_width = 8)

BedDensityPlot2 <- plot_grid(density_plot2, count_plot2)
save_plot("BedDensityPlot2.jpeg", BedDensityPlot2, ncol = 1, nrow = 2, base_height = 2,
          base_width = 8)
                                


################## Summary stats
count_til03 <- count %>%
  filter(bed_id == "TIL03")# %>%
  #filter(length <= 6.0)

v <- count_til03$length

mean(count_til03$length)
median(count_til03$length)  

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(v)
print(result)

count_cow01 <- count %>%
  filter(bed_id == "COW01") %>%
  filter(length <= 6.0)

count_bky01 <- count %>%
  filter(bed_id == "BKY01") %>%
  filter(length <= 6.0)


mean(count_cow01$length)
median(count_cow01$length)
