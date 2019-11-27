setwd("/Users/williamjohnson/Desktop/Laura/Mussels/Research/field_data")
library(tidyverse)
# read in csv files as tbl's
til03_sys1 <- as.tibble(read.csv("TIL03_systematic01.csv", header = TRUE))
til03_sys2 <- as.tibble(read.csv("TIL03_systematic02.csv", header = TRUE))

#Change transect_num and quadrat_num variables to factors
til03_sys1$Transect_Num <- factor(til03_sys1$Transect_Num)
# Make sure it worked OK.. it did! There are now 12 levels representing the 12 
#    different transects.
#levels(til03sys1_tbl$Transect_Num)
til03_sys1$Quadrat_Num <- factor(til03_sys1$Quadrat_Num)
#Look at structure of tbl again
str(til03_sys1)

# Need to get a sum of all mussels visible at the surface in all quadrats sampled
sum(til03_sys1$Surface_Live) + sum(til03_sys1$Excavated_Live, na.rm = TRUE)
  
# Filter to only consider quadrats with excavation. I need to get a sum of mussels in all 
#   excavated quadrats

### 05/15/19 Take 2 to sum all mussels buried in excavated quadrats
tilex1 <- til03_sys1 %>%
  filter(Excavated_Live != c("NA")) %>%
  mutate(burialfac = (Surface_Live + Excavated_Live) / Surface_Live) %>%
  # removed quadrat where no mussels at surface but 3 were buried for this calculation
  filter(is.finite(burialfac), !is.na(burialfac))
#SO... burialfactor_til03_1 is the burial factor with the quadrat removed (see above comment)
burialfactor_til03_1 <- mean((tilex$burialfac), na.rm=T)


# Create new object 'burial_factor' that INCLUDES the quadrat removed in burialfactor_til03_1 calculation
tilex2 <- til03_sys1 %>%
  filter(Excavated_Live != c("NA"))
# Return count of number of mussels found in all excavated quadrats 
sum_til03excavated <- sum(tilex2$Excavated_Live)
# Return count of total # of quadrats excavated
quadcount_til03excavated <- nrow(tilex2)
# Return count of number of mussels found at surface in all units that were double sampled
sum_til03surfex <- sum(tilex2$Surface_Live)

burialfactor_til03_2 <- (sum_til03surfex + sum_til03excavated) / sum_til03surfex
print(burialfactor_til03_2)
# Create new object 'realnum_TIL03' that is the true number of mussels across all quadrats 
#   sampled at the site
realnum_til03 <- sum(til03_sys1$Surface_Live) * burialfactor_til03_2
print(realnum_til03)
# Create new object 'surveyarea_til03' that represents the total area of the mussel bed surveyed
surveyarea_til03 <- nrow(til03_sys1) * 0.25
# Create new object 'density_til03' that represents mussel density in the surveyed area
density_til03 <- realnum_til03/surveyarea_til03
print(density_til03)
#determine true count of mussels at the site (including area NOT sampled)
totalcnt_til03 <- 931.84 * density_til03
print(totalcnt_til03)

### I get a count of 1151 when I sum the systematic 1 worksheet and a count of 150 animals in the systematic 2
  ### worksheet?!? There are 2 mussels in the systematic 1 worksheet that were NOT measured... which means I should
  ### expect 1149 rows in the systematic 2... one too many right now!
til03_sys2_CHECK <- til03_sys2 %>%
  group_by(Trans_Num, Quad_Num) %>%
  summarise(n())
# What? Everything seems to match ok b/w the two sheets...?

til03_sys1 %>%
  filter(Surface_Live > 111)

#########################################################################################################
# Change transect_num and quad_num to factors for til03_sys2 tbl
til03_sys2$Trans_Num <- factor(til03_sys2$Trans_Num)
til03_sys2$Quad_Num <- factor(til03_sys2$Quad_Num)
# Check
str(til03_sys2)

# Histogram for TIL03 (ALL MUSSELS SAMPLED IN SURFACE + EXCAVATION PLOTS)
    # What are the min and max values measured?
print(min(til03_sys2$Length_cm))
print(max(til03_sys2$Length_cm))

#Looks like there may be a typo. Min value of 0.08 seems too small? Need to find 
#   where in tbl this value occurs to double check with hard copy data sheet
which.min(til03_sys2$Length_cm)

# Yes, .08 is TOO SMALL... change this to .08
til03_sys2[398,7] <- (til03_sys2[398,7]*10)

# Make a histogram!!!
til03_sys2 %>%
  select(Length_cm) %>%
  ggplot(aes(x = Length_cm)) + geom_histogram()

# Try to mimic histogram in Mazzacano paper
mazzacano_hist <- til03_sys2 %>%
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

mazzacano_hist2 <- mazzacano_hist %>%
  group_by(size_class) %>%
  summarize(count = n()) %>%
  mutate(percent_pop = ((count/1150)*100)) 

row_order <- c("0 - 1.01", "1.01 - 2.00","2.01 - 3.00","3.01 - 4.00","4.01 - 5.00",
               "5.01 - 6.00", "6.01 - 7.00", "7.01 - 8.00", "8.01 - 9.00", 
               "9.01 - 10.00", "10.01 - 11.00", "11.01 - 12.00","12.01 - 13.00")

mazzacano_hist2$size_class <- factor(mazzacano_hist2$size_class, levels = row_order)

ggplot(mazzacano_hist2, aes(x = size_class, y = percent_pop)) + geom_col(fill = "black") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, face = "bold")) + 
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Size Class by Length Categories (cm)") + 
  ylab("Percent of Population (n=1150)") + ggtitle("TIL03 Western Pearlshell Population Size Distribution") +
  theme(plot.title = element_text(face = "bold")) 

# Investigate the different average length of mussels found buried vs. at surface
exc_hist <- til03_sys2 %>%
  group_by(S_or_E)%>%
  summarise(av_shell_length = mean(Length_cm))
# Turn this into a box plot

ggplot(til03_sys2, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + theme_bw() + 
  ggtitle("TIL03: Comparison of Excavated (E) VS Surface (S) 
                             Mussel Lengths (cm)") + labs(x = "Mussel Location", y = "Length (cm)") +
  theme(plot.title = element_text(face = "bold"))

  




