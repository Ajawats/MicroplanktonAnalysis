############################################################################
################# GRAPHS INDIVIDUAL TAXA COUNTS  by ESD ####################
############################################################################

### 11/15/22  Use Grpsesd_reps to graph individual taxa counts, sort by size, 
##  and do another sorted by abundance (counts)

### See MnCt_GrpEsd_Graphs.R

library(tidyverse)

load("data/Grpesd_reps.Rdata")
load("data/ctsum_rep_mnesd_no0.Rdata") # from script, 
### 03_Biomass_Means_Final_Sz_2022_11_09_22
diapenrep_esd <- Grpesd_reps[grep("diatom pennate", Grpesd_reps$grp_esd), ]
diapennate_esd<-  ctsum_rep_mnesd_no0[grep("diatom pennate", 
                                    ctsum_rep_mnesd_no0$grp_esd), ]

diacenrep_esd <- Grpesd_reps[grep("diatom centric", Grpesd_reps$grp_esd), ]
diacen_esd <- ctsum_rep_mnesd_no0[grep("diatom centric", 
                                ctsum_rep_mnesd_no0$grp_esd), ]
unique(diacen_esd$grp_esd)
#SJR2_reps <- subset(SJR2_reps, grp_esd %in% SJR2$grp_esd)

diacen_esdSJR2 <- subset(diacen_esd, samp_ev == "SJR2")
diacenrep_esdSJR2 <- subset(diacenrep_esd, samp_ev == "SJR2")
### The code below is for subsetting or filtering the diacenrep_esdSJR2 file
## to only include organisms that were found in the diacen_esdSJR2 file, so
## basically, only organisms that had more than a zero count
diacenrep_esdSJR2 <- subset(diacenrep_esdSJR2, grp_esd %in% diacen_esdSJR2$grp_esd)

ggplot(data= diacen_esdSJR2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "SJR2 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdSJR2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacen_esdSJR1 <- subset(diacen_esd, samp_ev == "SJR1")
diacenrep_esdSJR1 <- subset(diacenrep_esd, samp_ev == "SJR1")
diacenrep_esdSJR1 <- subset(diacenrep_esdSJR1, grp_esd %in% diacen_esdSJR1$grp_esd)
ggplot(data= diacen_esdSJR1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "SJR1 Cent Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdSJR1, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacen_esdYBP2 <- subset(diacen_esd, samp_ev == "YBP2")
diacenrep_esdYBP2 <- subset(diacenrep_esd, samp_ev == "YBP2")
diacenrep_esdYBP2 <- subset(diacenrep_esdYBP2, grp_esd %in% diacen_esdYBP2$grp_esd)
ggplot(data= diacen_esdYBP2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP2 Cent Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdYBP2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()


diacen_esdYBP1 <- subset(diacen_esd, samp_ev == "YBP1")
diacenrep_esdYBP1 <- subset(diacenrep_esd, samp_ev == "YBP1")
diacenrep_esdYBP1 <- subset(diacenrep_esdYBP1, grp_esd %in% diacen_esdYBP1$grp_esd)
ggplot(data= diacen_esdYBP1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP1 Cent Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdYBP1, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacen_esdLSZ2 <- subset(diacen_esd, samp_ev == "LSZ2")
diacenrep_esdLSZ2 <- subset(diacenrep_esd, samp_ev == "LSZ2")
diacenrep_esdLSZ2 <- subset(diacenrep_esdLSZ2, grp_esd %in% diacen_esdLSZ2$grp_esd)
ggplot(data= diacen_esdLSZ2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "LSZ2 Cent Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdLSZ2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacen_esdWLD2 <- subset(diacen_esd, samp_ev == "WLD2")
diacenrep_esdWLD2 <- subset(diacenrep_esd, samp_ev == "WLD2")
diacenrep_esdWLD2 <- subset(diacenrep_esdWLD2, grp_esd %in% diacen_esdWLD2$grp_esd)
ggplot(data= diacen_esdWLD2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "WLD2 Cent Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esdWLD2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

### Not sure why I have this here. It's all the centric diatoms
ggplot(data= diacen_esd, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                    FUN = sum, decreasing = TRUE), 
                         y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "Centric Diatoms Means Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_continuous(name = "Mean Counts")+
  #geom_point(data=diacen_esdrep_esdWLD2, aes(x= grp_esd, y = (counts)),
  #           shape = 1, size=2)+
  coord_flip()

ggplot(data= diacen_esd, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                    FUN = sum, decreasing = TRUE), 
                         y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "Centric Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep_esd, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

### Pennate Diatoms

diapennate_esdSJR2 <- subset(diapennate_esd, samp_ev == "SJR2")
diapenrep_esdSJR2 <- subset(diapenrep_esd, samp_ev == "SJR2")
diapenrep_esdSJR2 <- subset(diapenrep_esdSJR2, grp_esd %in% diapennate_esdSJR2$grp_esd)
ggplot(data= diapennate_esdSJR2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                            FUN = sum, decreasing = TRUE), 
                                 y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "SJR2 Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=6),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdSJR2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennate_esdSJR1 <- subset(diapennate_esd, samp_ev == "SJR1")
diapenrep_esdSJR1 <- subset(diapenrep_esd, samp_ev == "SJR1")
diapenrep_esdSJR1 <- subset(diapenrep_esdSJR1, grp_esd %in% diapennate_esdSJR1$grp_esd)
ggplot(data= diapennate_esdSJR1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                            FUN = sum, decreasing = TRUE), 
                                 y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "SJR1 Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=6),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdSJR1, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennate_esdYBP2 <- subset(diapennate_esd, samp_ev == "YBP2")
diapenrep_esdYBP2 <- subset(diapenrep_esd, samp_ev == "YBP2")
diacenrep_esdYBP2 <- subset(diapenrep_esdYBP2, grp_esd %in% diapennate_esdYBP2$grp_esd)
ggplot(data= diapennate_esdYBP2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                            FUN = sum, decreasing = TRUE), 
                                 y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP2 Pennate ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdYBP2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()


diapennate_esdYBP1 <- subset(diapennate_esd, samp_ev == "YBP1")
diapenrep_esdYBP1 <- subset(diapenrep_esd, samp_ev == "YBP1")
diapenrep_esdYBP1 <- subset(diapenrep_esdYBP1, grp_esd %in% diapennate_esdYBP1$grp_esd)
ggplot(data= diapennate_esdYBP1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP1 Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdYBP1, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennate_esdLSZ2 <- subset(diapennate_esd, samp_ev == "LSZ2")
diapenrep_esdLSZ2 <- subset(diapenrep_esd, samp_ev == "LSZ2")
diapenrep_esdLSZ2 <- subset(diapenrep_esdLSZ2, grp_esd %in% diapennate_esdLSZ2$grp_esd)
ggplot(data= diapennate_esdLSZ2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                                FUN = sum, decreasing = TRUE), 
                                     y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "LSZ2 Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdLSZ2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennate_esdWLD2 <- subset(diapennate_esd, samp_ev == "WLD2")
diapenrep_esdWLD2 <- subset(diapenrep_esd, samp_ev == "WLD2")
diapenrep_esdWLD2 <- subset(diapenrep_esdWLD2, grp_esd %in% diapennate_esdWLD2$grp_esd)
ggplot(data= diapennate_esdWLD2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                                FUN = sum, decreasing = TRUE), 
                                     y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "WLD2 Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esdWLD2, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

### Below is just an extra set
ggplot(data= diacen_esd, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                    FUN = sum, decreasing = TRUE), 
                         y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  labs(x="", title = "Centric Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        plot.title=element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  #geom_point(data=diacenrep_esd, aes(x= grp_esd, y = (counts)),
  # shape = 1, size=2)+
  coord_flip()

ggplot(data= diapennate_esd, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "Pennate Diatoms ESD Mn Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=5),
        plot.title=element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep_esd, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  #geom_point(data=diacenrep_esd, aes(x= grp_esd, y = (counts)),
  # shape = 1, size=2)+
  coord_flip()
