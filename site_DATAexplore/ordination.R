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

############################# run PCA on streampwr ############################################################
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

################################# run PCA on land use at drainage basin scale #########################################
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

################################## run PCA on land use at the HUC12 scale ###########################################
landHUC12_rda <- rda(na.omit(landhuc12_2), scale = TRUE)   

# extract PC values to use later
landHUC12_siteout <- as.data.frame(scores(landHUC12_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
landHUC12_siteout$ID<-rownames(landHUC12_siteout)
landHUC12_siteout$obs_id <- rownames(landhuc12)

landHUC12_enviroout<-as.data.frame(scores(landHUC12_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
landHUC12_enviroout$type<-"HUC 12 land use percentages"
landHUC12_enviroout$name<-rownames(landHUC12_enviroout)

# merge PC axes with HUC12 land use env. data
log <- left_join(landhuc12, landHUC12_siteout) 

##Plot PCA showing spread of env. variables (DB land use) by observation locations

ggplot(log, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = landHUC12_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = landHUC12_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
            size = 6,
            hjust = 0.5, 
            color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",landHUC12_rda$CA$eig["PC1"]/landHUC12_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",landHUC12_rda$CA$eig["PC2"]/landHUC12_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with HUC12 land use PC scores
#be sure that you keep the order the same throughout this!!
landHUC12_siteout$abundance <- obsabun$total_count

ggplot(data = landHUC12_siteout, aes(x=PC1, y=abundance))+
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
  ggtitle("HUC12 Land Use Environmental Variables \nPCA Regression VS Mussel Abundance")

############################# run PCA on stream power AND land use at the drainage basin scale ########################
SPLUdb_rda <- rda(na.omit(SPLUdb_2), scale = TRUE)   

# extract PC values to use later
SPLUdb_siteout <- as.data.frame(scores(SPLUdb_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
SPLUdb_siteout$ID<-rownames(SPLUdb_siteout)
SPLUdb_siteout$obs_id <- rownames(SPLUdb)

SPLUdb_enviroout<-as.data.frame(scores(SPLUdb_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
SPLUdb_enviroout$type<-"Stream Power & DB Land Use Variables"
SPLUdb_enviroout$name<-rownames(SPLUdb_enviroout)

# merge PC axes with Stream Pwr + DB Land Use env. data
jog <- left_join(SPLUdb, SPLUdb_siteout) 

##Plot PCA showing spread of env. variables (SP + DB Land Use) by observation locations

ggplot(jog, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  # geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = SPLUdb_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = SPLUdb_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
           size = 6,
           hjust = 0.5, 
           color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",SPLUdb_rda$CA$eig["PC1"]/SPLUdb_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",SPLUdb_rda$CA$eig["PC2"]/SPLUdb_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with Stream Power + DB land use PC scores
#be sure that you keep the order the same throughout this!!
SPLUdb_siteout$abundance <- obsabun$total_count

ggplot(data = SPLUdb_siteout, aes(x=PC1, y=abundance))+
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
  ggtitle("Stream Pwr + DB Land Environmental Variables \nPCA Regression VS Mussel Abundance")

############################# run PCA on stream power AND land use at the HUC12 scale ########################
SPLUhuc12_rda <- rda(na.omit(SPLUhuc12_2), scale = TRUE)   

# extract PC values to use later
SPLUhuc12_siteout <- as.data.frame(scores(SPLUhuc12_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
SPLUhuc12_siteout$ID<-rownames(SPLUhuc12_siteout)
SPLUhuc12_siteout$obs_id <- rownames(SPLUhuc12)

SPLUhuc12_enviroout<-as.data.frame(scores(SPLUhuc12_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
SPLUhuc12_enviroout$type<-"Stream Power & HUC12 Land Use Variables"
SPLUhuc12_enviroout$name<-rownames(SPLUhuc12_enviroout)

# merge PC axes with Stream Pwr + DB Land Use env. data
cog <- left_join(SPLUhuc12, SPLUhuc12_siteout) 

##Plot PCA showing spread of env. variables (SP + HUC12 Land Use) by observation locations

ggplot(cog, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = SPLUhuc12_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = SPLUhuc12_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
            size = 6,
            hjust = 0.5, 
            color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",SPLUhuc12_rda$CA$eig["PC1"]/SPLUhuc12_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",SPLUhuc12_rda$CA$eig["PC2"]/SPLUhuc12_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with SP & HUC12 land use PC scores
#be sure that you keep the order the same throughout this!!
SPLUhuc12_siteout$abundance <- obsabun$total_count

ggplot(data = SPLUhuc12_siteout, aes(x=PC1, y=abundance))+
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
  ggtitle("Stream Pwr + HUC12 Land Environmental Variables \nPCA Regression VS Mussel Abundance")

######################## run PCA on stream power AND land use at the drainage basin & HUC12 scale ########################
SPLUdbhuc12_rda <- rda(na.omit(SPLUdbhuc12_2), scale = TRUE)   #OK I got 16 error messages when I ran this and all of them
                                                      # said "unkwn or uninitialised column: 'CA'"
                      
# extract PC values to use later
### I got more of the same warning messages as above (unkwn or uninitialised column: 'CA') but it looks fine?
SPLUdbhuc12_siteout <- as.data.frame(scores(SPLUdbhuc12_rda, choices=c(1,2), display=c("sites"))) #these will be your sites
SPLUdbhuc12_siteout$ID<-rownames(SPLUdbhuc12_siteout)
SPLUdbhuc12_siteout$obs_id <- rownames(SPLUdbhuc12)

#More warnings of the same type!!!
SPLUdbhuc12_enviroout<-as.data.frame(scores(SPLUdbhuc12_rda, choices=c(1,2), display=c("species"))) ##these will be your environmental var.
SPLUdbhuc12_enviroout$type<-"Stream Power & DB & HUC12 Land Use Variables"
SPLUdbhuc12_enviroout$name<-rownames(SPLUdbhuc12_enviroout)

# merge PC axes with Stream Pwr + DB Land Use env. data
hog <- left_join(SPLUdbhuc12, SPLUdbhuc12_siteout) # And yes... even more warnings!

##Plot PCA showing spread of env. variables (SP + DB & HUC12 Land Use) by observation locations

ggplot(hog, aes(x=PC1, y=PC2))+ 
  geom_hline(aes(yintercept=0), color="grey") + 
  geom_vline(aes(xintercept=0), color="grey") +
  geom_text(aes(label = obs_id), size = 5) +   #"color = func" removed from inside aes() on this line
  # scale_color_manual(values = c("grey20", "grey70")) +
  geom_segment(data = SPLUdbhuc12_enviroout,
               aes(x = 0, xend =  PC1,
                   y = 0, yend =  PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") + #grid is required for arrow to work.
  geom_text(data = SPLUdbhuc12_enviroout,
            aes(x=  PC1*1.2, y =  PC2*1.2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust
            size = 6,
            hjust = 0.5, 
            color="black") + 
  theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                    text=element_text(size = 20))+ 
  xlab(paste("Axis 1 (",sprintf("%.1f",SPLUdbhuc12_rda$CA$eig["PC1"]/SPLUdbhuc12_rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",SPLUdbhuc12_rda$CA$eig["PC2"]/SPLUdbhuc12_rda$tot.chi*100,3),"%)",sep="")) 

#merge abundance data with Stream Power + DB & HUC12 land use PC scores
#be sure that you keep the order the same throughout this!!
SPLUdbhuc12_siteout$abundance <- obsabun$total_count

ggplot(data = SPLUdbhuc12_siteout, aes(x=PC1, y=abundance))+
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
  ggtitle("Stream Pwr + DB  & HUC12 Land Environmental Variables \nPCA Regression VS Mussel Abundance")

##################### Compare PC1 Axis of Env. Variables to Assess Co-linearity ############################

# Land Use DB Vs Land Use Huc12
ggplot() + geom_point(aes(landDB_siteout$PC1, landHUC12_siteout$PC1)) + 
  ggtitle("Land Use Drainage Basin VS Land Use HUC12 PCA Axis 1 Values")

# Land Use DB Vs Stream Power
ggplot() + geom_point(aes(landDB_siteout$PC1, sp_siteout$PC1)) + 
  ggtitle("Land Use Drainage Basin VS Stream Power PCA Axis 1 Values")

# Land Use HUC12 Vs Stream Power
ggplot() + geom_point(aes(landHUC12_siteout$PC1, sp_siteout$PC1)) + 
  ggtitle("Land Use HUC12 VS Stream Power PCA Axis 1 Values")
