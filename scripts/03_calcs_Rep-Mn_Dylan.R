###########################################################################
########### GRAPHS SAMPLING EVENTS REPS AND MEANS COUNTS ###################
########################### EDITED CODE FROM DYLAN #######################
############################################################################
 ### As of 11/13/22


### These are my edits of the code Dylan sent. I needed to edit it because he
##  used the wrong source file for SJR2. See below for his original code.

load("data/Grpsz_reps.Rdata")
load("data/ctsum_rep_mn_no0.Rdata")
SJR2_reps = subset(Grpsz_reps, samp_ev == "SJR2")
SJR2 = subset(ctsum_rep_mn_no0, samp_ev == "SJR2")
SJR2 <- SJR2[!(SJR2$exp == "IC"), ]
#SJR2$grp_sz = SJR2$category
SJR2_reps <- subset(SJR2_reps, grp_sz %in% SJR2$grp_sz)
# make quality pdf file
pdf(file = "Figures/SJR2 means and replicates.pdf",
    width =10,
    height = 8)

ggplot(data= SJR2, aes(x= reorder(grp_sz, mean_count,
                                  FUN = sum, decreasing = FALSE),
                       y=mean_count, color=exp))+
  ## decreasing = FALSE puts the highest values at the top of the page.
  geom_point(stat="identity", position='identity', size = 3)+
  labs(x="", title = "SJR2 Means and Replicates Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  #scale_x_discrete(limits=c(-1,1000))
  geom_point(data=SJR2_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()
dev.off()


### This is the code that Dylan used. He used the wrong source file for
## SJR2, ctbmn_grptyp_mnct, when he was supposed to use ctsum_rep_mn_no0.

load("data/ctbmn_grptyp_mnct.Rdata")

SJR2_reps = subset(Grpsz_reps, samp_ev == "SJR2")
SJR2 = subset(ctbmn_grptyp_mnct, samp_ev == "SJR2")
SJR2 <- SJR2[!(SJR2$exp == "IC"), ]
SJR2$grp_sz = SJR2$category
SJR2_reps <- subset(SJR2_reps, grp_sz %in% SJR2$grp_sz)
# make quality pdf file
pdf(file = "Figures/SJR2 means and replicates.pdf",
    width =10,
    height = 8)

ggplot(data= SJR2, aes(x= reorder(grp_sz, mean_count,
                                  FUN = sum, decreasing = TRUE),
                       y=mean_count, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  labs(x="", title = "SJR2 Means and Replicates Counts")+
  theme_bw()+
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(size=6))+
  scale_color_manual(values=c('#E69F00','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean Counts with Replicate Counts")+
  geom_point(data=SJR2_reps, aes(x= grp_sz, y = (counts)),
             shape = 1, size=2)+
  coord_flip()
dev.off()


