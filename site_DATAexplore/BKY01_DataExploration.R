setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)
# Read in the .csv files and save as tbls
bky01sys1 <- as.tibble(read.csv("BKY01_sys1.csv", header = TRUE))
bky01sys2 <- as.tibble(read.csv("BKY01_sys2.csv", header = TRUE))

#Change transect_num and quadrat_num variables to factors
bky01sys1$Trans_Num <- factor(bky01sys1$Trans_Num)
bky01sys1$Quad_Num <- factor(bky01sys1$Quad_Num)

#Remove NA values from dataset
bky01sys1 <- bky01sys1[-c(109:175),]

# Filter to only consider quadrats with excavation. I need to get a sum of mussels in all 
#   excavated quadrats
bkyex <- bky01sys1 %>%
  filter(Excavated_Live != c("NA")) 
# Return count of number of mussels found in all excavated quadrats 
sum_bkyexcavatedlive <- sum(bkyex$Excavated_Live)
# Return count of total # of quadrats excavated
quadcount_bkyexcavated <- nrow(bkyex)
# Return count of total # of quadrats sampled
quadcount_bkytotal <- nrow(bky01sys1)
# Return count of number of mussels found at surface in all units that were double sampled
sum_bkysurfex <- sum(bkyex$Surface_Live)

# Create new object 'burial_factor' 
burialfactor_bky01 <- (sum_bkysurfex + sum_bkyexcavatedlive) / sum_bkysurfex
print(burialfactor_bky01)
# Create new object 'realnum_bky01' that is the true number of mussels across all quadrats 
#   sampled at the site
realnum_bky01 <- sum(bky01sys1$Surface_Live) * burialfactor_bky01
print(realnum_bky01)
# Create new object 'surveyarea_TIL03' that represents the total area of the mussel bed surveyed
surveyarea_bky01 <- quadcount_bkytotal * 0.25
# Create new object 'density_bky01' that represents mussel density in the surveyed area
density_bky01 <- realnum_bky01/surveyarea_bky01
print(density_bky01)
#determine true count of mussels at the site (including area NOT sampled)
totalcnt_bky <- 135 * density_bky01
print(totalcnt_bky)

max(bky01sys1$Surface_Live)

###############################################################################################################

# Change transect_num and quad_num to factors for bky01_sys2 
bky01sys2$Trans_num <- factor(bky01sys2$Trans_num)
bky01sys2$Quad_num <- factor(bky01sys2$Quad_num)
# Check
str(bky01sys2)

# What are the min and max values measured?
bky01sys2_live <- bky01sys2 %>%
  filter(L_or_D == "L") %>%

print(min(bky01sys2_live$Length_cm)) ### WHY IS THIS NOT WORKING? JUST GIVING ME A TBL IN RETURN????
print(max(bky01sys2_live$Length_cm))
# OK.. min value of 2.25 and max of 11.9 but lets keep all the size classes regardless so it is visually obvious that not
# many young mussels are there

# Histogram for BKY01 (ALL MUSSELS SAMPLED IN SURFACE + EXCAVATION PLOTS)
# Make a histogram!!!
bky01sys2_hist<- bky01sys2 %>%
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
  mutate(size_class = ifelse(Length_cm >= 12.01 & Length_cm < 13.01, "12.01 - 13.00", size_class))%>%
  # Remove weird columns that don't mean anything... where did they even come from?
  select(-c(X,X.1,X.2))

bkymazzacano_hist <- bky01sys2_hist %>%
  group_by(size_class) %>%
  summarize(count = n()) %>%
  mutate(percent_pop = ((count/8)*100)) 

row_order <- c("0 - 1.01", "1.01 - 2.00","2.01 - 3.00","3.01 - 4.00","4.01 - 5.00",
               "5.01 - 6.00", "6.01 - 7.00", "7.01 - 8.00", "8.01 - 9.00", 
               "9.01 - 10.00", "10.01 - 11.00", "11.01 - 12.00","12.01 - 13.00")

bkymazzacano_hist$size_class <- factor(bkymazzacano_hist$size_class, levels = row_order)

ggplot(bkymazzacano_hist, aes(x = size_class, y = percent_pop)) + geom_col(fill = "black") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Size Class by Length Categories (cm)") + 
  ylab("Percent of Population") + ggtitle("BKY01 Western Pearlshell Population Size Distribution") +
  theme(plot.title = element_text(face = "bold")) 

#Histogram that uses mussel status (live vs dead) as a factor (THIS HAS NOT WORKED WELL)
bky01sys2_hist2 <- bky01sys2 %>%
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
  mutate(size_class = ifelse(Length_cm >= 12.01 & Length_cm < 13.01, "12.01 - 13.00", size_class))%>%
  # Remove weird columns that don't mean anything... where did they even come from?
  select(-c(X,X.1,X.2)) 
  
# Order the levels of the factor size_class
bky01sys2_hist2$size_class <- factor(bky01sys2_hist2$size_class, levels = row_order)
# Make the histogram!!!
ggplot(bky01sys2_hist2, aes(x = size_class)) + geom_histogram(fill = "black", stat = "count") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Size Class by Length Categories (cm)") + 
  ylab("Count") + ggtitle("Comparison of Live VS Dead Mussel Lengths at BKY01") +
  theme(plot.title = element_text(face = "bold")) + facet_wrap( ~L_or_D)

# Investigate the different average length of mussels found dead vs. alive
status_hist <- bky01sys2 %>%
  group_by(L_or_D)%>%
  summarise(av_shell_length = mean(Length_cm))

# Turn this into a box plot---THIS ONE IS GOOD
ggplot(bky01sys2, aes(as.factor(L_or_D), Length_cm)) + geom_boxplot() + theme_classic() + 
  ggtitle("BKY01: Comparison of Live (L) VS Dead (D) 
                             Mussel Lengths (cm)") + labs(x = "Mussel Status", y = "Length (cm)") +
  theme(plot.title = element_text(face = "bold"))

