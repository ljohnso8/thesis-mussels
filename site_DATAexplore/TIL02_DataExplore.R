setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)
# Read in the .csv files and save as tbls
til02cen1 <- as.tibble(read.csv("til02_cen1.csv", header = TRUE))
til02cen2 <- as.tibble(read.csv("til02_cen2.csv", header = TRUE))

#Change block_num and unit_num variables to factors
til02cen1$Block_Num <- factor(til02cen1$Block_Num)
til02cen1$Unit_Num <- factor(til02cen1$Unit_Num)


# Filter to only consider quadrats with excavation. I need to get a sum of mussels in all 
#   excavated quadrats
til02ex <- til02cen1 %>%
  filter(Excavated_Live != c("NA")) 
# Return count of number of mussels found in all excavated quadrats 
sum_til02excavatedlive <- sum(til02ex$Excavated_Live)
# Return count of total # of quadrats excavated
quadcount_til02excavated <- nrow(til02ex)
# Return count of total # of quadrats sampled
quadcount_til02total <- nrow(til02cen1)
# Return count of number of mussels found at surface in all units that were double sampled
sum_til02surfex <- sum(til02ex$Surface_Live)

# Create new object 'burial_factor' 
burialfactor_til02 <- (sum_til02surfex + sum_til02excavatedlive) / sum_til02surfex
print(burialfactor_til02)
# Create new object 'realnum_til02' that is the true number of mussels across all quadrats 
#   sampled at the site (taking into account the burial factor)
realnum_til02 <- sum(til02cen1$Surface_Live) * burialfactor_til02
print(realnum_til02)
# Create new object 'surveyarea_TIL02' that represents the total area of the mussel bed surveyed
surveyarea_til02 <- quadcount_til02total * 0.25
# Create new object 'density_til02' that represents mussel density in the surveyed area
density_til02 <- realnum_til02/surveyarea_til02
print(density_til02)

max(til02cen1$Excavated_Live, na.rm = TRUE)

####################################################################################################################

# Change transect_num and quad_num to factors for til03_sys2 tbl
til02cen2$Block_Num <- factor(til02cen2$Block_Num)
til02cen2$Unit_Num <- factor(til02cen2$Unit_Num)
# Check
str(til02cen2)

# Histogram for TIL02 (ALL MUSSELS SAMPLED IN SURFACE + EXCAVATION PLOTS)
# What are the min and max values measured?
print(min(til02cen2$Length_cm))
print(max(til02cen2$Length_cm))

# Make a histogram!!!
til02cen2 %>%
  select(Length_cm) %>%
  ggplot(aes(x = Length_cm)) + geom_histogram()

# Try to mimic histogram in Mazzacano paper
til02mazzacano_hist <- til02cen2 %>%
  mutate(size_class = ifelse(Length_cm < 1.01, "0 - 1.01", "other")) %>%
  mutate(size_class = ifelse(Length_cm >= 1.01 & Length_cm < 2.01, "1.01 - 2.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 2.01 & Length_cm < 3.01, "2.01 - 3.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 3.01 & Length_cm < 4.01, "3.01 - 4.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 4.01 & Length_cm < 5.01, "4.01 - 5.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 5.01 & Length_cm < 6.01, "5.01 - 6.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 6.01 & Length_cm < 7.01, "6.01 - 7.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 7.01 & Length_cm < 8.01, "7.01 - 8.00", size_class))

til02mazzacano_hist2 <- til02mazzacano_hist %>%
  group_by(size_class) %>%
  summarize(count = n()) %>%
  mutate(percent_pop = ((count/112)*100)) 

row_order <- c("0 - 1.01", "1.01 - 2.00","2.01 - 3.00","3.01 - 4.00","4.01 - 5.00",
               "5.01 - 6.00", "6.01 - 7.00", "7.01 - 8.00")

til02mazzacano_hist2$size_class <- factor(til02mazzacano_hist2$size_class, levels = row_order)

ggplot(til02mazzacano_hist2, aes(x = size_class, y = percent_pop)) + geom_col(fill = "black") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Size Class by Length Categories (cm)") + 
  ylab("Percent of Population (n=112)") + ggtitle("TIL02 Floater Mussel Population Size Distribution") +
  theme(plot.title = element_text(face = "bold")) 

# Investigate the different average length of mussels found buried vs. at surface
til02exc_hist <- til02cen2 %>%
  group_by(S_or_E)%>%
  summarise(av_shell_length = mean(Length_cm))
# Turn this into a box plot

ggplot(til02cen2, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + theme_bw() + 
  ggtitle("TIL02: Comparison of Excavated (E) VS Surface (S) 
                             Anodonta sp. Lengths (cm)") + labs(x = "Mussel Location", y = "Length (cm)") +
  theme(plot.title = element_text(face = "bold")) + 
  scale_y_continuous(breaks=seq(1,7,1))




# Histogram for BKY site
bky01_sys2 <- read.csv("BKY01_sys2.csv", header = TRUE)
bky01sys2_tbl <- tbl_df(bky01_sys2)
bky01sys2_tbl %>%
  select(Length_cm) %>%
  ggplot(aes(x = Length_cm)) + geom_histogram(stat = "count")



