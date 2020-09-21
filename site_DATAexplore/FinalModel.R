setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")

library(tidyverse)
library(MASS)
library(scales)
library(cowplot)
library(nlme)
#Bring in needed files...

#finalmodel spreadsheet has all candidate variables
finalmodel <- as.tibble(read.csv("FinalModel.csv"), colnames = TRUE)
#model w/o zinc cr
finalmodel <- finalmodel%>%
  filter(!site_id == "ZINCCMP") %>%
  filter(total_count > 1)  # only aggregations that are more than 1 mussel
  

# Stepwise Regression using all Laura sites from 2018 & 2020
fit <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest + finalmodel$SP_10yr + 
            finalmodel$HUC_Pforest + finalmodel$HUC_Pth + finalmodel$HUC_Pag)
step <- stepAIC(fit, direction = "both")
step$anova  #display results

#Look at Summaries of Models

mod1 <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest + finalmodel$SP_10yr + finalmodel$HUC_Pth)
summary(mod1)


#Assumption of linearity
ggplot(finalmodel) + geom_point(aes(x = DB_Pforest, y = log(total_count))) #Yes
ggplot(finalmodel) + geom_point(aes(x = SP_10yr, y = log(total_count))) #Not much linearity here!!!
ggplot(finalmodel) + geom_point(aes(x = HUC_Pth, y = log(total_count))) #Hard to tell b/c of low sample size


#Examine histogram of the residual values to determine if they are normally distributed
ggplot(mod1) + geom_histogram(aes(mod1$residuals)) + labs(x = "Residual Values", y = "Count") +
  ggtitle("Distribution of Residuals from Log(abundance) ~ Final Model")

# Determine mean of residual values
print(mean(mod1$residuals)) # YES the mean of residuals = 0!!! 

# Assumption of homoscedasticity of residuals
mod1.res <- resid(mod1)
plot(finalmodel$DB_Pforest, mod1.res) #No... greater variance at higher % forest
abline(0,0)
plot(finalmodel$SP_10yr, mod1.res) # Ok... still unequal vertical variance but better horizontal variance
abline(0,0)
plot(finalmodel$HUC_Pth, mod1.res) # Less variance of errors at low percent of timber harvest in the HUC
abline(0,0)








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
#the residuals but including the SP_10YR does not







# Only look at South Umpqua sites and regress abundance vs river distance
finalmodel_SUMP <- finalmodel[1:44, ]

distmodel <- lm(log(finalmodel_SUMP$total_count + .01) ~ finalmodel_SUMP$dist_km)
summary(distmodel) 
distmodel.res <- resid(distmodel)
plot(finalmodel_SUMP$dist_km, distmodel.res)
abline(0,0)
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

#Bring in site distance / drainage basin data
distarea <- as.tibble(read.csv("dist_area_final.csv"), colnames = TRUE)
# Need to link drainage basin areas with site id
drainmodel <- finalmodel %>%
  inner_join(distarea, by = "obs_id")

drain_model <- lm(log(drainmodel$total_count + .01) ~ drainmodel$drain_area)
summary(drain_model)

#Examine histogram of the residual values to determine if they are normally distributed
ggplot(drain_model) + geom_histogram(aes(drain_model$residuals)) + labs(x = "Residual Values", y = "Count") +
  ggtitle("Distribution of Residuals from Log(abundance) ~ Drainage Basin Area Regression")

# Determine mean of residual values
print(mean(drain_model$residuals)) # YES the mean of residuals = 0!!! 

# Assess whether residuals meet assumption of homoscedasticity
drain_model.res <- resid(drain_model)
plot(drainmodel$drain_area, drain_model.res)
abline(0,0) #Looks like greater variance at lower drainage basin areas than larger drainage basin areas... this is also
             # the sites where there were lots of low abundance aggregations but also high abundances, too! All of the 
            # abundances in the larger drainage basin area sites were much more similar to each other (less range b/w
            # aggregation abundance)

# Residual Plot
ggplot(drain_model) + geom_point(aes(x = drain_model$fitted.values, y = drain_model$residuals)) + 
  labs(x = "Predicted log(Mussel Abundance) Values", y = "Residual Values") + ggtitle("Abundance ~ Drainage Basin Area Residual Plot") 




#Try fitting model with Generalized Least Squares
drain_model.gls <- lm.gls(log(drainmodel$total_count + .01) ~ drainmodel$drain_area, weights(drainmodel$drain_area)
summary(drain_model.gls)





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



