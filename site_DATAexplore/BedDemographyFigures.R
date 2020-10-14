setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")
library(tidyverse)
library(cowplot)

#bring in needed files and clean them up
bky01sys2 <- as.tibble(read.csv("BKY01_sys2.csv", header = TRUE))
bky <- bky01sys2[, 1:7]

til03 <- as.tibble(read.csv("TIL03_systematic02.csv", header = TRUE))
til03 <- til03[, 1:6]

cow <- as.tibble(read.csv("cow01_cen2.csv", header = TRUE))

til02 <- as.tibble(read.csv("til02_cen2.csv", header = TRUE))




################# BKY plot of lengths of live vs dead animals #################################
# Need to change D and L to Live and Dead in rows
bky <- bky %>%
  mutate(L_or_D = ifelse(L_or_D == "L", "Live", "Dead"))

# Turn this into a box plot
bkyplot <- ggplot(bky, aes(as.factor(L_or_D), Length_cm)) + geom_boxplot() + 
  theme_classic() + xlab("BKY01 Margaritifera Bed")
####### HELP why won't this re-format my y axis to extend to 12.5?!? 
bkyplot <- bkyplot + scale_y_continuous(name = "Length (cm)", breaks = seq(0,13,1)) +  
  theme(axis.title.x = element_text(size=10, face="bold"))
#theme(axis.title.x = element_blank())

############# TIL03 plot of lengths of surface vs excavated animals ##################
# Need to change E and S to excavated and surface in rows
til03 <- til03 %>%
  mutate(S_or_E = ifelse(S_or_E == "S", "Surface", "Excavated"))


# Investigate the different average length of mussels found buried vs. at surface
exc_hist <- til03 %>%
  group_by(S_or_E)%>%
  summarise(meanshelllength = mean(Length_cm))
# Turn this into a box plot

til03plot <- ggplot(til03, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + 
  theme_classic() + xlab("TIL05 Margaritifera Bed")

til03plot <- til03plot + scale_y_continuous(name = "Length (cm)", breaks = seq(0,12,1)) +
  theme(axis.title.x = element_text(size=10, face="bold"))
  # + theme(axis.title.x = element_blank())

######################### COW lengths of Excavated VS surface  ##############################
# Need to change E and S to excavated and surface in rows
cow01cen2 <- cow %>%
  mutate(S_or_E = ifelse(S_or_E == "S", "Surface", "Excavated"))
cowexc_hist <- cow01cen2 %>%
  filter(L_or_D == "L") %>%
  group_by(S_or_E)%>%
  summarise(av_shell_length = mean(Length_cm))
# Turn this into a box plot
cow <- ggplot(cow01cen2, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + 
  theme_classic() + xlab("COW01 Margaritifera Bed")

cow <- cow + scale_y_continuous(name = "Length (cm)", breaks = seq(0,12,1)) + #
  theme(axis.title.x = element_text(size=10, face="bold"))
  #theme(axis.title.x = element_blank()) 

################################### TIL02 Ano Plot #############################
# Need to change E and S to excavated and surface in rows
til02 <- til02 %>%
  mutate(S_or_E = ifelse(S_or_E == "S", "Surface", "Excavated"))
til02exc_hist <- cow01cen2 %>%
  filter(L_or_D == "L") %>%
  group_by(S_or_E)%>%
  summarise(av_shell_length = mean(Length_cm))
# Turn this into a box plot
til02boxplot <- ggplot(til02, aes(as.factor(S_or_E), Length_cm)) + geom_boxplot() + 
  theme_classic() + xlab("TIL03 Anodonta Bed")
  

til02boxplot <- til02boxplot + scale_y_continuous(name = "Length (cm)", breaks = seq(0,7,1)) +
  theme(axis.title.x = element_text(size=10, face="bold"))


# + theme(axis.title.x = element_blank()) 



############################ Put it all in 1 plot! ###############################
plot_grid(til03plot, bkyplot, cow, til02boxplot)
