setwd("/Users/williamjohnson/Desktop/Laura/Hallett_Lab/Repositories/thesis-mussels/site_DATAexplore")
library(tidyverse)
library(vegan)

#Bring in needed files... site/abundance info, stream power info, land cover info. Env. variables are in wide format.
# Env. variable dataframe observation order exactly matches observation order in "ord_obsabun.csv" file.
obsabun <- as.tibble(read.csv("ord_obsabun.csv"))
streampwr <- as.tibble(read.csv("ord_streamPWR.csv"))
landDB <- as.tibble(read.csv("ord_landcoverDB.csv"))
landhuc12 <- as.tibble(read.csv("ord_landcoverHUC12.csv"))
SPLUdb <- as.tibble(read.csv("ord_SP&LUdb.csv"))
SPLUhuc12 <- as.tibble(read.csv("ord_SP&LUhuc12.csv"))
SPLUdbhuc12 <- as.tibble(read.csv("ord_SP&LUdb&LUhuc12.csv"))

#Modify env. variable dataframes so there are no row/column names in the dataframe
row.names(streampwr) <- obsabun$obs_id
row.names(landDB) <- obsabun$obs_id
row.names(landhuc12) <- obsabun$obs_id
row.names(SPLUdb) <- obsabun$obs_id
row.names(SPLUhuc12) <- obsabun$obs_id
row.names(SPLUdbhuc12) <- obsabun$obs_id

#Delete unneeded columns in env. variable dataframes
streampwr_2 <- streampwr %>% select(av_SLPE_gradient:Sstrpwr_5perc)
landDB_2 <- landDB %>% select(prc_frst:prc_TH)
landhuc12_2 <- landhuc12 %>% select(prc_frst:prc_TH)
SPLUdb_2 <- SPLUdb %>% select(av_SLPE_gradient:prc_TH)
SPLUhuc12_2 <- SPLUhuc12 %>% select(av_SLPE_gradient:prc_TH)
SPLUdbhuc12_2 <- SPLUdbhuc12 %>% select(av_SLPE_gradient:HUC12prc_TH)

# run PCA on streampwr
sp_rda <- rda(na.omit(streampwr_2), scale = TRUE)   

# extract PC values to use later
sp_siteout <- as.data.frame(scores(sp_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
sp_siteout$ID<-rownames(sp_siteout)
sp_siteout$obs_id <- rownames(streampwr)

sp_enviroout<-as.data.frame(scores(sp_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
sp_enviroout$type<-"hydrologic variables"
sp_enviroout$name<-rownames(sp_enviroout)

# merge PC axes with streampower env. data
tog <- left_join(streampwr, sp_siteout) 

##Plot PCA showing spread of env. variables (stream power) by observation locations

ggplot(tog, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = sp_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = sp_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
            size = 6,
            hjust = 0.5, 
            color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",sp_rda$CA$eig["PC1"]/sp_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",sp_rda$CA$eig["PC2"]/sp_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with PC scores
#be sure that you keep the order the same throughout this!!
sp_siteout$abundance <- obsabun$total_count

ggplot(data = sp_siteout, aes(x=PC1, y=abundance))+
  #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, aes(group=1)) +
  geom_point()+
  theme_bw()+
  #scale_color_manual(values=c("tomato", "green3", "dodgerblue"), guide = guide_legend(title = "Treatment"), #change legend title
  #labels=c("Mixed", "Forb", "Grass"))+ #change labels in the legend)+
  xlab("PC1 Scores")+
  ylab("Freshwater Mussel Abundance")+
  #xlim(40,100)+
  #ylim(0,100)
  geom_smooth(method="lm", formula= y ~ x, se=FALSE, color="black", aes(group=1)) +
  scale_y_log10() +
  ggtitle("Stream Power Environmental Variables \nPCA Regression VS Mussel Abundance")

# run PCA on land use at drainage basin scale
landDB_rda <- rda(na.omit(landDB_2), scale = TRUE)   

# extract PC values to use later
landDB_siteout <- as.data.frame(scores(landDB_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
landDB_siteout$ID<-rownames(landDB_siteout)
landDB_siteout$obs_id <- rownames(landDB)

landDB_enviroout<-as.data.frame(scores(landDB_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
landDB_enviroout$type<-"land use variables"
landDB_enviroout$name<-rownames(landDB_enviroout)

# merge PC axes with drainage basin land use env. data
bog <- left_join(landDB, landDB_siteout) 

##Plot PCA showing spread of env. variables (DB land use) by observation locations

ggplot(bog, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = landDB_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = landDB_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
            size = 6,
            hjust = 0.5, 
            color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",landDB_rda$CA$eig["PC1"]/landDB_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",landDB_rda$CA$eig["PC2"]/landDB_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with DB landuse PC scores
#be sure that you keep the order the same throughout this!!
landDB_siteout$abundance <- obsabun$total_count

ggplot(data = landDB_siteout, aes(x=PC1, y=abundance))+
  #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, aes(group=1)) +
  geom_point()+
  theme_bw()+
  #scale_color_manual(values=c("tomato", "green3", "dodgerblue"), guide = guide_legend(title = "Treatment"), #change legend title
  #labels=c("Mixed", "Forb", "Grass"))+ #change labels in the legend)+
  xlab("PC1 Scores")+
  ylab("Freshwater Mussel Abundance")+
  #xlim(40,100)+
  #ylim(0,100)
  geom_smooth(method="lm", formula= y ~ x, se=FALSE, color="black", aes(group=1)) +
  scale_y_log10() + 
  ggtitle("Drainage Basin Land Use Environmental Variables \nPCA Regression VS Mussel Abundance")

