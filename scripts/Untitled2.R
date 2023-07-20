############################################################################
############### GRAPHS SAMPLING EVENTS CATEGORIES COUNTS ###################
############################################################################

### This code is adapted from the code Dylan gave me to make a graph of
##   each sampling event, the main organism categories and their counts
##   of the three experiments, FC, IC, T24, averaged across the replicates.
##   ctbmn_grptyp_mnct.Rdata comes from

load("data/ctbmn_grptyp_mnct.Rdata")

ctbmn_grptyp_mnct_grped <- ctbmn_grptyp_mnct %>%
  group_by(grp_typ, exp, samp_ev)%>%
  summarise_at(vars(tot_ct), list(mean = mean)) %>% 
subset(exp!="site")
# log values
#grouped_ctbmn_grptyp$log_av <- log(grouped_ctbmn_grptyp$average)
# Split into sampling events
SJR2 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "SJR2")
LSZ2 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "LSZ2")
WLD2 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "WLD2")
YBP1 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "YBP1")
YBP2 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "YBP2")
SJR1 = subset(ctbmn_grptyp_mnct_grped, samp_ev == "SJR1")

ggplot(data= LSZ2, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  #geom_smooth(formula = y~x, method = "lm")+
  ## take above out and see what happens
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "LSZ2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

### Add 

ggplot(data= WLD2, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "WLD2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= SJR1, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR1 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= YBP2, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= YBP1, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "YBP1 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
  coord_flip()

ggplot(data= SJR2, aes(x= reorder(grp_typ, -mean, FUN = sum, decreasing = TRUE), 
                       y=mean, color=exp))+
  geom_point(stat="identity", position='identity', size = 3)+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", title = "SJR2 Mean Counts")+
  theme(axis.text.x = element_text(angle = 90), axis.text.y = element_text(size=8))+
  scale_color_manual(values=c('#E69F00', '#999999','#56B4E9'))+
  #scale_y_continuous(name = "Mean of Total Counts", breaks = seq(0,1080, 50))
  scale_y_log10(name = "Mean, Total Counts, log10 scale")+
 coord_flip()
  