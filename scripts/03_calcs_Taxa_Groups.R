############################################################################
##################### GRAPHS INDIVIDUAL TAXA COUNTS ########################
############################################################################

### 11/15/22  Use Grpsz_reps to graph indivudual taxa counts, sort by size, 
##  and do another sorted by abundance (counts)

### See MnCt_GrpTyp_Graphs.R

library(tidyverse)

load("data/Grpsz_reps.Rdata")
load("data/ctsum_rep_mn_no0.Rdata")
diapenrep <- Grpsz_reps[grep("diatom pennate", Grpsz_reps$grp_sz), ]
diapennate<-  ctsum_rep_mn_no0[grep("diatom pennate", 
                                   ctsum_rep_mn_no0$grp_sz), ]

diacenrep <- Grpsz_reps[grep("diatom centric", Grpsz_reps$grp_sz), ]
diacen <- ctsum_rep_mn_no0[grep("diatom centric", 
                                ctsum_rep_mn_no0$grp_sz), ]
unique(diacen$grp_sz)
#SJR2_reps <- subset(SJR2_reps, grp_sz %in% SJR2$grp_sz)

diacenSJR2 <- subset(diacen, samp_ev == "SJR2")
diacenrepSJR2 <- subset(diacenrep, samp_ev == "SJR2")
### The code below is for subsetting or filtering the diacenrepSJR2 file
## to only include organisms that were found in the diacenSJR2 file, so
## basically, only organisms that had more than a zero count
diacenrepSJR2 <- subset(diacenrepSJR2, grp_sz %in% diacenSJR2$grp_sz)

ggplot(data= diacenSJR2, aes(x= reorder(grp_sz, -mn_ct_ml, 
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
  geom_point(data=diacenrepSJR2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenSJR1 <- subset(diacen, samp_ev == "SJR1")
diacenrepSJR1 <- subset(diacenrep, samp_ev == "SJR1")
diacenrepSJR1 <- subset(diacenrepSJR1, grp_sz %in% diacenSJR1$grp_sz)
ggplot(data= diacenSJR1, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "SJR1 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepSJR1, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenYBP2 <- subset(diacen, samp_ev == "YBP2")
diacenrepYBP2 <- subset(diacenrep, samp_ev == "YBP2")
diacenrepYBP2 <- subset(diacenrepYBP2, grp_sz %in% diacenYBP2$grp_sz)
ggplot(data= diacenYBP2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP2 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepYBP2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()


diacenYBP1 <- subset(diacen, samp_ev == "YBP1")
diacenrepYBP1 <- subset(diacenrep, samp_ev == "YBP1")
diacenrepYBP1 <- subset(diacenrepYBP1, grp_sz %in% diacenYBP1$grp_sz)
ggplot(data= diacenYBP1, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP1 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepYBP1, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenLSZ2 <- subset(diacen, samp_ev == "LSZ2")
diacenrepLSZ2 <- subset(diacenrep, samp_ev == "LSZ2")
diacenrepLSZ2 <- subset(diacenrepLSZ2, grp_sz %in% diacenLSZ2$grp_sz)
ggplot(data= diacenLSZ2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "LSZ2 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepLSZ2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenWLD2 <- subset(diacen, samp_ev == "WLD2")
diacenrepWLD2 <- subset(diacenrep, samp_ev == "WLD2")
diacenrepWLD2 <- subset(diacenrepWLD2, grp_sz %in% diacenWLD2$grp_sz)
ggplot(data= diacenWLD2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "WLD2 Cent Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepWLD2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

### Not sure why I have this here. It's all the centric diatoms
ggplot(data= diacen, aes(x= reorder(grp_sz, -mn_ct_ml, 
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
  #geom_point(data=diacenrepWLD2, aes(x= grp_sz, y = (counts)),
  #           shape = 1, size=2)+
  coord_flip()

ggplot(data= diacen, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "Centric Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrep, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

### Pennate Diatoms

diapennateSJR2 <- subset(diapennate, samp_ev == "SJR2")
diapenrepSJR2 <- subset(diapenrep, samp_ev == "SJR2")
diapenrepSJR2 <- subset(diapenrepSJR2, grp_sz %in% diapennateSJR2$grp_sz)
ggplot(data= diapennateSJR2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "SJR2 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=6),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrepSJR2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennateSJR1 <- subset(diapennate, samp_ev == "SJR1")
diapenrepSJR1 <- subset(diapenrep, samp_ev == "SJR1")
diapenrepSJR1 <- subset(diapenrepSJR1, grp_sz %in% diapennateSJR1$grp_sz)
ggplot(data= diapennateSJR1, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "SJR1 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=6),
        title = element_text(size=10))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrepSJR1, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diapennateYBP2 <- subset(diapennate, samp_ev == "YBP2")
diapenrepYBP2 <- subset(diapenrep, samp_ev == "YBP2")
diacenrepYBP2 <- subset(diapenrepYBP2, grp_sz %in% diapennateYBP2$grp_sz)
ggplot(data= diapennateYBP2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP2 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapennateYBP2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()


diacenYBP1 <- subset(diacen, samp_ev == "YBP1")
diacenrepYBP1 <- subset(diacenrep, samp_ev == "YBP1")
diacenrepYBP1 <- subset(diacenrepYBP1, grp_sz %in% diacenYBP1$grp_sz)
ggplot(data= diacenYBP1, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "YBP1 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepYBP1, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenLSZ2 <- subset(diacen, samp_ev == "LSZ2")
diacenrepLSZ2 <- subset(diacenrep, samp_ev == "LSZ2")
diacenrepLSZ2 <- subset(diacenrepLSZ2, grp_sz %in% diacenLSZ2$grp_sz)
ggplot(data= diacenLSZ2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "LSZ2 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepLSZ2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

diacenWLD2 <- subset(diacen, samp_ev == "WLD2")
diacenrepWLD2 <- subset(diacenrep, samp_ev == "LSZ2")
diacenrepWLD2 <- subset(diacenrepWLD2, grp_sz %in% diacenWLD2$grp_sz)
ggplot(data= diacenWLD2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                        FUN = sum, decreasing = TRUE), 
                             y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 4)+
  labs(x="", title = "WLD2 Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=8),
        plot.title=element_text(size=8),
        axis.title.x = element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diacenrepWLD2, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

ggplot(data= diacen, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                    FUN = sum, decreasing = TRUE), 
                         y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  labs(x="", title = "Centric Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=10),
        plot.title=element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  #geom_point(data=diacenrep, aes(x= grp_sz, y = (counts)),
            # shape = 1, size=2)+
  coord_flip()

ggplot(data= diapennate, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                    FUN = sum, decreasing = TRUE), 
                         y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "Pennate Diatoms Mean Cts per mL w/Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=5),
        plot.title=element_text(size=8))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL w/Reps, log10")+
  geom_point(data=diapenrep, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  #geom_point(data=diacenrep, aes(x= grp_sz, y = (counts)),
  # shape = 1, size=2)+
  coord_flip()
