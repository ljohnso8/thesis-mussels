setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")

library(tidyverse)
library(MASS)
library(scales)
library(cowplot)
#Bring in needed files...

#finalmodel spreadsheet has all candidate variables
finalmodel <- as.tibble(read.csv("FinalModel.csv"), colnames = TRUE)

# Stepwise Regression
fit <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DP_Pforest + finalmodel$SP_10yr + 
            finalmodel$HUC_Pforest + finalmodel$HUC_Pth + finalmodel$HUC_Pag, data = mydata)
step <- stepAIC(fit, direction = "both")
step$anova  #display results

#Look at Model
mod1 <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest+ finalmodel$HUC_Pag)
summary(mod1)

modA <- lm(log(finalmodel$total_count + .01) ~ finalmodel$DB_Pforest)
summary(modA)

modB <- lm(log(finalmodel$total_count + .01) ~ finalmodel$SP_10yr)
summary(modB)

anova(modA, mod1) #Results: Step-wise regression indicates DB_Pforest and HUC_Pag as contributing to 
#the most explanation of variance in the data. Anova comparing simpler model (only including DB_Pforest)
#to expanded model is borderline significant in support of more complex model. Keeping HUC_Pag variable
#explains about 4% more variation in the data. Stream power performs very poorly. 

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
