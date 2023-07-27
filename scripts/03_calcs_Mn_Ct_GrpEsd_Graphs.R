############################################################################
############# GRAPHS SAMPLING EVENTS CATEGORIES COUNTS  ESD ################
############################################################################

### This code is adapted from MnCt_GrpTyp_Graphs.R, but uses the data frame
## that has the ESD

library(tidyverse)
library(writexl)

load("data/ctsum_rep_mnesd_no0.Rdata")

# Split into sampling events
SJR1 = subset(ctsum_rep_mnesd_no0, samp_ev == "SJR1")
YBP2 = subset(ctsum_rep_mnesd_no0, samp_ev == "YBP2")
YBP1 = subset(ctsum_rep_mnesd_no0, samp_ev == "YBP1")
WLD2 = subset(ctsum_rep_mnesd_no0, samp_ev == "WLD2")
SJR2 = subset(ctsum_rep_mnesd_no0, samp_ev == "SJR2")
LSZ2 = subset(ctsum_rep_mnesd_no0, samp_ev == "LSZ2")

load("data/volbio_all.Rdata")

### Remove the site and IC samples from volbio_all
volbio_all_fc_exp <- subset(volbio_all, exp!= "site") %>% 
  subset(exp!="IC")

write_csv(volbio_all_fc_exp, "data/volbio_all_fc_exp.csv")

names(volbio_all_fc_exp)
Grpesd_reps <- subset(volbio_all_fc_exp, 
                     select = c(samp_ev, exp, rep, grp_esd, counts_per_ml))

#write.csv(Grpsesd_reps, "data/Grpesd_reps.csv")
save(Grpesd_reps, file = "data/Grpesd_reps.Rdata")

### Make data frames of the sampling events that have the reps
WLD2_reps = subset(Grpesd_reps, samp_ev == "WLD2")
WLD2_reps = subset(WLD2_reps, grp_esd %in% WLD2$grp_esd)

YBP2_reps = subset(Grpesd_reps, samp_ev == "YBP2")
YBP2_reps = subset(YBP2_reps, grp_esd %in% YBP2$grp_esd)

SJR1_reps = subset(Grpesd_reps, samp_ev == "SJR1")
SJR1_reps = subset(SJR1_reps, grp_esd %in% SJR1$grp_esd)

YBP1_reps = subset(Grpesd_reps, samp_ev == "YBP1")
YBP1_reps = subset(YBP1_reps, grp_esd %in% YBP1$grp_esd)

LSZ2_reps = subset(Grpesd_reps, samp_ev == "LSZ2") 
LSZ2_reps = subset(LSZ2_reps, grp_esd %in% LSZ2$grp_esd)

SJR2_reps = subset(Grpesd_reps, samp_ev == "SJR2")
SJR2_reps <- subset(SJR2_reps, grp_esd %in% SJR2$grp_esd)


ggplot(data= SJR2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  #geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR2 ESD Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL with Reps")+
  geom_point(data=SJR2_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()
#(df[which(df$x>0),],aes(x,y))+

ggplot(data= LSZ2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  #geom_smooth(formula = y~x, method = "lm")+
  ## take above out and see what happens
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "LSZ2 ESD Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=LSZ2_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()


ggplot(data= WLD2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  geom_smooth(formula = y~x, method = "lm")+
  theme_bw()+
  labs(x="", title = "WLD2 ESD Mn_Ct per mL and Reps")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=WLD2_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

ggplot(data= SJR1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2.5)+
  labs(x="", title = "SJR1 ESD Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=SJR1_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()

ggplot(data= YBP2, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "YBP2 ESD Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=YBP2_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()
nrow(YBP2_reps)

ggplot(data= YBP1, aes(x= reorder(grp_esd, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  geom_smooth(formula = y~x, method = "lm")+
  labs(x="", title = "YBP1 ESD Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=5))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=YBP1_reps, aes(x= grp_esd, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()
