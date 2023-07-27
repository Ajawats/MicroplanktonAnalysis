############################################################################
############### GRAPHS SAMPLING EVENTS CATEGORIES COUNTS ###################
############################################################################

### This code is adapted from the code Dylan gave me to make a graph of
##   each sampling event, the main organism categories and their counts
##   of the three experiments, FC, IC, T24, averaged across the replicates.
##   ctbmn_grptyp_mnct.Rdata comes from

library(tidyverse)
library(writexl)
# look for Wim's graph theme code 
#load("Wim")

### Updated 11/15/22. Some of the notes below are old.

### 11/10/22 I made some changes to the data so that I can include the 
##  dimensions with the organism groups, so the below code isn't necessary
##  anymore. This data file used as its source, volbio_all, from 
##  03_calcs_volbio_100400.R, and includes all the zero counts, since
##  those are necessary to get a true mean across all three replicates.
##  Use ctsum_rep_mn_no0, from 03_Biomass_Means_Final_Sz_2022_11_09.R

load("data/ctsum_rep_mn_no0.Rdata")

#ctbmn_grptyp_mnct_grped <- ctbmn_grptyp_mnct %>%
 # group_by(grp_typ, exp, samp_ev)%>%
  #summarise_at(vars(tot_ct), list(mean = mean)) %>% 
  #subset(exp!="site") %>% 
  #subset(exp!= "IC")
# log values
#grouped_ctbmn_grptyp$log_av <- log(grouped_ctbmn_grptyp$average)
# Split into sampling events
# No need to filter out by 0, since ctsum_rep_mn_n0 already 
# removed the means that were 0, since that means that there were no
# counts of that organism in any replicate.
# SJR2 = filter(SJR2, mean_count >= 0)


SJR1 = subset(ctsum_rep_mn_no0, samp_ev == "SJR1")
YBP2 = subset(ctsum_rep_mn_no0, samp_ev == "YBP2")
YBP1 = subset(ctsum_rep_mn_no0, samp_ev == "YBP1")
WLD2 = subset(ctsum_rep_mn_no0, samp_ev == "WLD2")
SJR2 = subset(ctsum_rep_mn_no0, samp_ev == "SJR2")
LSZ2 = subset(ctsum_rep_mn_no0, samp_ev == "LSZ2")


load("data/volbio_all.Rdata")
volbio_all_fc_exp <- subset(volbio_all, exp!= "site") %>% 
  subset(exp!="IC")

volbio_all_fc_exp$size <- (paste(volbio_all_fc_exp$sa, volbio_all_fc_exp$la))
volbio_all_fc_exp$grp_sz <- (paste(volbio_all_fc_exp$grp_typ, 
                                   volbio_all_fc_exp$size))

write_csv(volbio_all_fc_exp, "data/volbio_all_fc_exp.csv")

names(volbio_all_fc_exp)
Grpsz_reps <- subset(volbio_all_fc_exp, 
                      select = c(samp_ev, exp, rep, grp_sz, counts_per_ml))

write.csv(Grpsz_reps, "data/Grpsz_reps.csv")
save(Grpsz_reps, file = "data/Grpsz_reps.Rdata")

SJR1 = subset(ctsum_rep_mn_no0, samp_ev == "SJR1")
YBP2 = subset(ctsum_rep_mn_no0, samp_ev == "YBP2")
YBP1 = subset(ctsum_rep_mn_no0, samp_ev == "YBP1")
WLD2 = subset(ctsum_rep_mn_no0, samp_ev == "WLD2")
SJR2 = subset(ctsum_rep_mn_no0, samp_ev == "SJR2")
LSZ2 = subset(ctsum_rep_mn_no0, samp_ev == "LSZ2")



WLD2_reps = subset(Grpsz_reps, samp_ev == "WLD2")
WLD2_reps = subset(WLD2_reps, grp_sz %in% WLD2$grp_sz)

YBP2_reps = subset(Grpsz_reps, samp_ev == "YBP2")
YBP2_reps = subset(YBP2_reps, grp_sz %in% YBP2$grp_sz)

SJR1_reps = subset(Grpsz_reps, samp_ev == "SJR1")
SJR1_reps = subset(SJR1_reps, grp_sz %in% SJR1$grp_sz)

YBP1_reps = subset(Grpsz_reps, samp_ev == "YBP1")
YBP1_reps = subset(YBP1_reps, grp_sz %in% YBP1$grp_sz)

LSZ2_reps = subset(Grpsz_reps, samp_ev == "LSZ2") 
LSZ2_reps = subset(LSZ2_reps, grp_sz %in% LSZ2$grp_sz)

SJR2_reps = subset(Grpsz_reps, samp_ev == "SJR2")
SJR2_reps <- subset(SJR2_reps, grp_sz %in% SJR2$grp_sz)

# No, don't do this. below, filter out all grp_typs that have fewer than 10 counts
#SJR2_reps = filter(SJR2_reps)
# Don't do this: SJR2_full = SJR2_full[SJR2_full$tot_ct > 9, ] 

#LSZ2_reps = subset(Grpsz_reps, samp_ev == "LSZ2")
#LSZ2_reps <- subset(LSZ2_reps, grp_sz %in% LSZ2$grp_sz)
#LSZ2_reps = filter(LSZ2_reps, counts >=10)
#WLD2_reps = subset(Grpsz_reps, samp_ev == "WLD2")
#WLD2_reps = filter(WLD2_reps, counts >=10)
#YBP2_reps = subset(Grpsz_reps, samp_ev == "YBP2")
#YBP2_reps = filter(YBP2_reps, counts >=10)
#SJR1_reps = subset(Grpsz_reps, samp_ev == "SJR1")
#SJR1_reps = filter(SJR1_reps, counts >=10)
#YBP1_reps = subset(Grpsz_reps, samp_ev == "YBP1")
#YBP1_reps = filter(YBP1_reps, counts >=10)
#write_xlsx(YBP1_reps, "data/YBP1_reps.xlsx")

ggplot(data= SJR2, aes(x= reorder(grp_sz, -mn_ct_ml, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  #geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR2 Mn_Ct per mL and Reps")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
       axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Cts per mL with Reps")+
  geom_point(data=SJR2_reps, aes(x= grp_sz, y = (counts_per_ml)),
             shape = 1, size=2)+
  coord_flip()
#(df[which(df$x>0),],aes(x,y))+

ggplot(data= LSZ2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mn_ct_ml, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  #geom_smooth(formula = y~x, method = "lm")+
  ## take above out and see what happens
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "LSZ2 Means and Replicates Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=LSZ2_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()


ggplot(data= WLD2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  geom_smooth(formula = y~x, method = "lm")+
  theme_bw()+
  labs(x="", title = "WLD2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=WLD2_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()

ggplot(data= SJR1, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 2.5)+
  labs(x="", title = "SJR1 Mean Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=SJR1_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()

ggplot(data= YBP2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  labs(x="", title = "YBP2 Mean Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(), 
        axis.text.y = element_text(size=4))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=YBP2_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()
nrow(YBP2_reps)

ggplot(data= YBP1, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 2)+
  geom_smooth(formula = y~x, method = "lm")+
  labs(x="", title = "YBP1 Mean Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=5))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=YBP1_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()
nrow(YBP1_reps)

### This code below is what I wrote before adding the replicate counts
##  to the code

SJR2 = subset(ctsum_rep_mn_no0, samp_ev == "SJR2")
max(SJR2$mean_count)
which.max(SJR2$mean_count)
unique(SJR2$grp_sz) 

WLD2 = subset(ctsum_rep_mn_no0, samp_ev == "WLD2")
#WLD2 = filter(WLD2, mean_count >= 0)
max(WLD2$mean_count)
which.max(WLD2$mean_count)
unique(WLD2$grp_sz)

YBP1 = subset(ctsum_rep_mn_no0, samp_ev == "YBP1")
#YBP1 = filter(YBP1, mean_count >= 10)
max(YBP1$mean_count)
which.max(YBP1$mean_count)
unique(YBP1$grp_sz)
#write_xlsx(YBP1, "data/YBP1.xlsx")

YBP2 = subset(ctsum_rep_mn_no0, samp_ev == "YBP2")
#YBP2 = filter(YBP2, mean_count >= 10)
max(YBP2$mean_count)
which.max(YBP2$mean_count)
unique(YBP2$grp_sz)

SJR1 = subset(ctsum_rep_mn_no0, samp_ev == "SJR1")
#SJR1 = filter(SJR1, mean_count >= 10)
max(SJR1$mean_count)
which.max(SJR1$mean_count)
unique(SJR1$grp_sz)

### These graphs below don't have the replicate counts on them.
ggplot(data= LSZ2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  #geom_smooth(formula = y~x, method = "lm")+
  ## take above out and see what happens
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "LSZ2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()


ggplot(data= WLD2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "WLD2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= SJR1, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR1 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        xis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= YBP2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= YBP1, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= SJR2, aes(x= reorder(grp_sz, -mean_count, 
                                  FUN = sum, decreasing = TRUE), 
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.text.y = element_text(size=8))+
  theme_bw()+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()
