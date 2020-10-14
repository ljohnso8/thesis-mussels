setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)
# Read in the .csv files and save as tbls
cow01cen1 <- as.tibble(read.csv("cow01_cen1.csv", header = TRUE))
cow01cen2 <- as.tibble(read.csv("cow01_cen2.csv", header = TRUE))

#Change block_num and unit_num variables to factors
cow01cen1$Block_Num <- factor(cow01cen1$Block_Num)
cow01cen1$Unit_Num <- factor(cow01cen1$Unit_Num)

#Remove NA values from dataset
cow01cen1 <- cow01cen1[-c(173:175),]

# Filter to only consider quadrats with excavation. I need to get a sum of mussels in all 
#   excavated quadrats
cowex <- cow01cen1 %>%
  filter(Excavated_Live != c("NA")) 
# Return count of number of mussels found in all excavated quadrats 
sum_cowexcavatedlive <- sum(cowex$Excavated_Live)
# Return count of total # of quadrats excavated
quadcount_cowexcavated <- nrow(cowex)
# Return count of total # of quadrats sampled
quadcount_cowtotal <- nrow(cow01cen1)
# Return count of number of mussels found at surface in all units that were double sampled
sum_cowsurfex <- sum(cowex$Surface_Live)
# return count of total mussels counted at surface in all units
totalsumcowsurfex <- sum(cow01cen1$Surface_Live)

# Create new object 'burial_factor' 
burialfactor_cow01 <- (sum_cowsurfex + sum_cowexcavatedlive) / sum_cowsurfex
print(burialfactor_cow01)
# Create new object 'realnum_cow01' that is the true number of mussels across all quadrats 
#   sampled at the site
realnum_cow01 <- sum(cow01cen1$Surface_Live) * burialfactor_cow01
print(realnum_cow01)
# Create new object 'surveyarea_cow01' that represents the total area of the mussel bed surveyed
surveyarea_cow01 <- quadcount_cowtotal * 0.25
# Create new object 'density_cow01' that represents mussel density in the surveyed area
density_cow01 <- realnum_cow01/surveyarea_cow01
print(density_cow01)

max(cow01cen1$Surface_Live, na.rm = TRUE)

# Mean length
cowcen2 <- cow01cen2 %>%
  filter(L_or_D == "L") %>%
# Juveniles
  filter(Length_cm <= 3.0)

# 7 juveniles out of 97 measured animals = .072

mean(cowcen2$Length_cm)

# Juveniles




################################################################################
### time for visualization!

# Change transect_num and quad_num to factors for til03_sys2 tbl
cow01cen2$Block_Num <- factor(cow01cen2$Block_Num)
cow01cen2$Unit_Num <- factor(cow01cen2$Unit_Num)
# Check
str(cow01cen2)

# Histogram for COW01 (ALL MUSSELS SAMPLED IN SURFACE + EXCAVATION PLOTS)

# What are the min and max values measured?
print(min(cow01cen2$Length_cm, na.rm = TRUE))
print(max(cow01cen2$Length_cm, na.rm = TRUE))

# Histogram for COW01 (ALL MUSSELS SAMPLED IN SURFACE + EXCAVATION PLOTS)
# Make a histogram!!!
cowhist<- cow01cen2 %>%
  filter(L_or_D == "L") %>%
  mutate(size_class = ifelse(Length_cm < 1.01, "0 - 1.01", "other")) %>%
  mutate(size_class = ifelse(Length_cm >= 1.01 & Length_cm < 2.01, "1.01 - 2.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 2.01 & Length_cm < 3.01, "2.01 - 3.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 3.01 & Length_cm < 4.01, "3.01 - 4.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 4.01 & Length_cm < 5.01, "4.01 - 5.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 5.01 & Length_cm < 6.01, "5.01 - 6.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 6.01 & Length_cm < 7.01, "6.01 - 7.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 7.01 & Length_cm < 8.01, "7.01 - 8.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 8.01 & Length_cm < 9.01, "8.01 - 9.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 9.01 & Length_cm < 10.01, "9.01 - 10.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 10.01 & Length_cm < 11.01, "10.01 - 11.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 11.01 & Length_cm < 12.01, "11.01 - 12.00", size_class)) %>%
  mutate(size_class = ifelse(Length_cm >= 12.01 & Length_cm < 13.01, "12.01 - 13.00", size_class))

cowmazzacano_hist <- cowhist %>%
  group_by(size_class) %>%
  summarize(count = n()) %>%
  mutate(percent_pop = ((count/97)*100)) 

row_order <- c("0 - 1.01", "1.01 - 2.00","2.01 - 3.00","3.01 - 4.00","4.01 - 5.00",
               "5.01 - 6.00", "6.01 - 7.00", "7.01 - 8.00", "8.01 - 9.00", 
               "9.01 - 10.00", "10.01 - 11.00", "11.01 - 12.00","12.01 - 13.00")

cowmazzacano_hist$size_class <- factor(cowmazzacano_hist$size_class, levels = row_order)

ggplot(cowmazzacano_hist, aes(x = size_class, y = percent_pop)) + geom_col(fill = "black") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Size Class by Length Categories (cm)") + 
  ylab("Percent of Population (n = 97)") + ggtitle("COW01 Western Pearlshell Population Size Distribution") +
  theme(plot.title = element_text(face = "bold")) 

############# Investigate the different average length of mussels found buried vs. at surface  ##############
# Need to change E and S to excavated and surface in rows
cow01cen2 <- cow01cen2 %>%
  mutate(S_or_E = ifelse(S_or_E == "S", "Surface", "Excavated"))
cowexc_hist <- cow01cen2 %>%
  filter(L_or_D == "L") %>%
  group_by(S_or_E)%>%
  summarise(av_shell_length = mean(Length_cm))
# Turn this into a box plot
cow <- ggplot(cow01cen2, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + theme_classic() 

cow <- cow + theme(axis.title.x = element_blank()) + scale_y_continuous(name = "Length (cm)", breaks = seq(0,12.5,2.5))





# Need to change E and S to excavated and surface in rows
til03 <- til03 %>%
  mutate(S_or_E = ifelse(S_or_E == "S", "Surface", "Excavated"))


# Investigate the different average length of mussels found buried vs. at surface
exc_hist <- til03 %>%
  group_by(S_or_E)%>%
  summarise(meanshelllength = mean(Length_cm))
# Turn this into a box plot

til03 <- ggplot(til03, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + theme_classic() 

til03 <- til03 + theme(axis.title.x = element_blank()) + scale_y_continuous(name = "Length (cm)", breaks = seq(0,12.5,2.5))



# Histogram for BKY site
bky01_sys2 <- read.csv("BKY01_sys2.csv", header = TRUE)
bky01sys2_tbl <- tbl_df(bky01_sys2)
bky01sys2_tbl %>%
  select(Length_cm) %>%
  ggplot(aes(x = Length_cm)) + geom_histogram(stat = "count")


