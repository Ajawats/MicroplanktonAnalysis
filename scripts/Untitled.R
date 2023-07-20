###For now use this dataframe for everything
ctmb_grptype <- read.csv("Clean Data/ctbmn_grptype.csv")
load("data/ctbmn_grptyp_mnct.Rdata")
#### Split data into sampling events
library(tidyverse)
grouped_ctbmn_grptyp <- ctbmn_grptyp_mnct %>%
  group_by(grp_typ, exp, samp_ev, mn_ct) #%>%
  #summarise_at(vars(tot_ct), list(average = mean))
# make mean and log values
#grouped_ctbmn_grptyp$log_av <- log(grouped_ctbmn_grptyp$average)
# Split into sampling events
LSZ2 = subset(grouped_ctbmn_grptyp, samp_ev == "LSZ2")
WLD2 = subset(grouped_ctbmn_grptyp, samp_ev == "WLD2")
YBP1 = subset(grouped_ctbmn_grptyp, samp_ev == "YBP1")
YBP2 = subset(grouped_ctbmn_grptyp, samp_ev == "YBP2")
SJR1 = subset(grouped_ctbmn_grptyp, samp_ev == "SJR1")
SJR2 = subset(grouped_ctbmn_grptyp, samp_ev == "SJR2")
####SJR1 stacked barchart with experiments
library(ggplot2)
ggplot(data= LSZ2, aes(x= reorder(category, mn_ct, FUN = sum), y=mn_ct, color=exp))+
  geom_point(stat="identity", position='identity')+
  geom_smooth(formula = y~x, method = "lm")+
  #  coord_flip(ylim=c(0,75))+
  labs(x="", y="Mean Counts")+
  ggtitle("LSZ2 Mean counts Organism/Size Categories")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9')) 
