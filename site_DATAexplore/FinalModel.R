setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")

library(tidyverse)
library(MASS)
library(scales)
library(cowplot)
#Bring in needed files...

#finalmodel spreadsheet has all candidate variables
finalmodel <- as.tibble(read.csv("FinalModel.csv"), colnames = TRUE)
#model w/o zinc cr
finalmodel_nozinc <- finalmodel%>%
  filter(!site_id == "ZINCCMP")
  

# Stepwise Regression using all Laura sites from 2018 & 2020
fit <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest + finalmodel$SP_10yr + 
            finalmodel$HUC_Pforest + finalmodel$HUC_Pth + finalmodel$HUC_Pag)
step <- stepAIC(fit, direction = "both")
step$anova  #display results
# Stepwise Regression using all sites EXCEPT Zinc Cr
fit2 <- lm(log(finalmodel_nozinc$total_count + .01) ~ finalmodel_nozinc$DB_Pforest + finalmodel_nozinc$SP_10yr + 
            finalmodel_nozinc$HUC_Pforest + finalmodel_nozinc$HUC_Pth + finalmodel_nozinc$HUC_Pag)
step <- stepAIC(fit2, direction = "both")
step$anova  #display results

#Look at Summaries of Models

#Stepwise Regression Model with all sites
mod1 <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest+ finalmodel$HUC_Pag)
summary(mod1)
#Stepwise Regression Model EXCEPT Zinc Cr
nozinc <- lm(log(finalmodel_nozinc$total_count + .01) ~ finalmodel_nozinc$DB_Pforest+ finalmodel_nozinc$SP_10yr + finalmodel_nozinc$HUC_Pth)
summary(nozinc)
#Model that only includes Drainage Basin % Forest
modA <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest)
summary(modA)
#Model looking at significance of stream power
modB <- lm(log(finalmodel$total_count + .01) ~ finalmodel$SP_10yr)
summary(modB)

#Look at anova results/ Model Comparisons
anova(modA, mod1) #Results: Step-wise regression indicates DB_Pforest and HUC_Pag as contributing to 
#the most explanation of variance in the data. Anova comparing simpler model (only including DB_Pforest)
#to expanded model is borderline significant in support of more complex model. Keeping HUC_Pag variable
#explains about 4% more variation in the data. Stream power performs very poorly. 

anova(nozinc, modA) #Results: If I am reading this correctly, adding the HUC_Pth variable does significantly reduce
#the residuals but the SP_10YR does not

#What about a model with only DB_Pforest and HUC_Pth?
modC <- lm(log(finalmodel_nozinc$total_count + .01) ~ finalmodel_nozinc$DB_Pforest+ finalmodel_nozinc$HUC_Pth)
summary(modC)
anova(nozinc, modC)
# Only look at South Umpqua sites and regress abundance vs river distance
finalmodel_SUMP <- finalmodel[1:46, ]

distmodel <- lm(log(finalmodel_SUMP$total_count + .01) ~ finalmodel_SUMP$dist_km)
summary(distmodel)  

##################### River Distance VS Abundance Plot ###########################################
riverDistplot <- ggplot(finalmodel_SUMP, aes(finalmodel_SUMP$dist_km, finalmodel_SUMP$total_count, color = clams)) + 
  geom_point() +
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("River Distance (km)") +  
  ylab("Mussel Abundance") + #ggtitle("Mussel Abundance & Invasive Asian Clam Presence \nat Sites on the South Umpqua River, OR") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

riverDistplot <- riverDistplot + labs(color = "Asian clams \npresent") 

##################### Drainage Basin Area VS Abundance Plot ###########################################

drainmodel <- lm(log(drainmodel$total_count + .01) ~ drainmodel$drain_area)
summary(drainmodel)


#Bring in site distance / drainage basin data
distarea <- as.tibble(read.csv("dist_area_final.csv"), colnames = TRUE)
# Need to link drainage basin areas with site id

distarea_SUMP <- distarea[1:46,]

drainmodel <- finalmodel %>%
  inner_join(distarea, by = "obs_id")

Drainplot <- ggplot(drainmodel, aes(drain_area, total_count, color = clams)) + 
  geom_point() +
  geom_smooth(method='lm', formula= y~x, aes(group=1)) +
  theme(axis.text.y = element_text(face = "bold")) +
  xlab("Drainage Area (mi^2))") +  
  ylab("Mussel Abundance") + #ggtitle("Mussel Abundance & Invasive Asian Clam Presence \nat Sites on the South Umpqua River, OR") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

Drainplot <- Drainplot + labs(color = "Asian clams \npresent") 


##################### Relationship B/W River Dist & Drain Area ##################################
# Looks like drainage area and river distance highly correlated in the S. Umpqua
ggplot(distarea_SUMP, aes(x = riv_dist_mi, y = drain_area)) + geom_point() + 
  geom_smooth(method='lm', formula= y~x, aes(group=1)) + labs(title = "Distance vs Drainage Area for S. Umpqua Mussel Sites") +
  xlab("River Distance (miles)") + ylab("Drainage Area (square miles)")

#correlation test
cor(distarea_SUMP$drain_area, distarea_SUMP$riv_dist_mi)

##################### Model Variables VS Abundance Plot ###########################################
DBforestPlot <- ggplot(finalmodel, aes(x = DB_Pforest, y = total_count)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, aes(group=1)) + 
  xlab("Drainage Basin Percent Forest") + ylab("Mussel Abundance") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

HUCagPlot <- ggplot(finalmodel, aes(x = HUC_Pag, y = total_count)) + geom_point() +
  xlab("HUC12 Percent Agriculture") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

HUCagPlot <- HUCagPlot + theme(axis.title.y = element_blank(), axis.text.y = element_blank())




ModelPlot <- plot_grid(DBforestPlot, HUCagPlot)

save_plot("ModelPlot.jpeg", ModelPlot, ncol = 2, nrow = 1, base_height = 4,
          base_width = 6)

#################################  Stream Power Figure ############################################

#Bring in larger spreadsheet with species information

allobs <- as.tibble(read.csv("Laura_AllObs_Abundance.csv"), colnames = TRUE)
allobs <- allobs %>%
  dplyr::select(obs_id:AsianClams)

# Join allobs and finalmodel datasets

finalmodel2 <- finalmodel %>%
  dplyr::select(-site_id, -total_count, -DB_Pforest, -HUC_Pforest, -HUC_Pth, -HUC_Pag) %>%
  inner_join(allobs, by = "obs_id")

pearlshell <- finalmodel2 %>%
  filter(Mafa_cnt >= 1) %>%
  mutate(species = c("western pearlshell"))

floater <- finalmodel2 %>%
  filter(Ano_cnt >= 1) %>%
  mutate(species = c("floater"))

floater2 <- finalmodel2 %>%
  filter(Ano_cnt > 5) %>%
  mutate(species = c("floater bed"))

ridged <- finalmodel2 %>%
  filter(Goan_cnt >= 1) %>%
  mutate(species = c("western ridged"))

allspecies <- rbind(pearlshell, floater, ridged)

boxplot <- ggplot(allspecies, aes(species, SP_10yr, color = total_count)) + geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) + ylab("10 YR Specific Stream Power (watts/m^2)") + theme_classic() #+ 
#scale_color_gradientn(name = "log(Mussel Abundance)" +
#theme(legend.title = element_text(size=rel(1.15), hjust=0.5, face="bold"))

mid <- mean(allspecies$total_count)
boxplot <- boxplot + scale_color_gradient2(midpoint = mid, low = "red", mid = "blue", high = "green")


###################### Stream Power as Bivariate Plot #######################################

# This one is pretty good!
ggplot(allspecies, aes(x = SP_10yr, y = total_count, color = species)) + geom_point() +
  geom_smooth(method='lm', formula= y~x, aes(group=1), se = FALSE) + 
  xlab("10 Year Specific Stream Power") + ylab("Mussel Abundance") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()



