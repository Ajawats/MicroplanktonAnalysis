############################################################################
######## FIND TOP 10 IN SINGLE SAMPLE, SINGLE SAMPLING EVENT ###############
############################################################################

### 11/30/22, See notes from 11/30 email from Wim: Find out what is in
##  the top 10 of any set of samples (means of initials, experimentals,
##  site) separately for each event, or top 8 or 5 if it's too many taxa
## to put in a graph, or whatever gives you a total of maybe 20 to work with.

library(tidyverse)
library(writexl)

load("data/ctsum_rep_mnesd_no0.Rdata") 
#from 03_Biomass_Means_Final_Sz_2022_11_09_22.R
# Make sure to use the file that includes all samples

### Make a new data frame from ctsum_rep_mnesd_no0.Rdata and rename columns
meanCPMall <- ctsum_rep_mnesd_no0 
meanCPMall <- select(meanCPMall, sampEvent = samp_ev, sample=exp,
                     groupESD=grp_esd, meanCPM = mn_ct_ml)
meanCPMall$meanCPM<-formattable(meanCPMall$meanCPM,
                                format="f",digits=2)

# Split into sampling events
# Split sampling events into samples, in=initials, fn=finals, t24=t24
SJR1 = subset(meanCPMall, sampEvent == "SJR1")
SJR1 <- SJR1[order(SJR1$meanCPM,decreasing = TRUE),]
SJR1fn = subset(SJR1, sample == "FC")
SJR1in = subset(SJR1, sample == "IC")
SJR1t24 = subset(SJR1, sample == "T24")
# Take only the top 15 meanCPM organisms
SJR1fn15 <- slice(SJR1fn, 1:15) 
SJR1in15 <- slice(SJR1in, 1:15)
SJR1t2415 <- slice(SJR1t24, 1:15)

YBP2 = subset(meanCPMall, sampEvent == "YBP2")
YBP2 <- YBP2[order(YBP2$meanCPM,decreasing = TRUE),]
YBP2fn = subset(YBP2, sample == "FC")
YBP2in = subset(YBP2, sample == "IC")
YBP2t24 = subset(YBP2, sample == "T24")
# Take only the top 15 meanCPM organisms
YBP2fn15 <- slice(YBP2fn, 1:15)
YBP2in15 <- slice(YBP2in, 1:15)
YBP2t2415 <- slice(YBP2t24, 1:15)

YBP1 = subset(meanCPMall, sampEvent == "YBP1")
YBP1 <- YBP1[order(YBP1$meanCPM,decreasing = TRUE),]
YBP1fn = subset(YBP1, sample == "FC")
YBP1in = subset(YBP1, sample == "IC")
YBP1t24 = subset(YBP1, sample == "T24")
YBP1fn15 <- slice(YBP1fn, 1:15)
YBP1in15 <- slice(YBP1in, 1:15)
YBP1t2415 <- slice(YBP1t24, 1:15)

WLD2 = subset(meanCPMall, sampEvent == "WLD2")
WLD2 <- WLD2[order(WLD2$meanCPM,decreasing = TRUE),]
WLD2fn = subset(WLD2, sample == "FC")
WLD2in = subset(WLD2, sample == "IC")
WLD2t24 = subset(WLD2, sample == "T24")
WLD2fn15 <- slice(WLD2fn, 1:15)
WLD2in15 <- slice(WLD2in, 1:15)
WLD2t2415 <- slice(WLD2t24, 1:15)

SJR2 = subset(meanCPMall, sampEvent == "SJR2")
SJR2 <- SJR2[order(SJR2$meanCPM,decreasing = TRUE),]
SJR2fn = subset(SJR2, sample == "FC")
SJR2in = subset(SJR2, sample == "IC")
SJR2t24 = subset(SJR2, sample == "T24")
SJR2fn15 <- slice(SJR2fn, 1:15)
SJR2in15 <- slice(SJR2in, 1:15)
SJR2t2415 <- slice(SJR2t24, 1:15)

LSZ2 = subset(meanCPMall, sampEvent == "LSZ2")
LSZ2 <- LSZ2[order(LSZ2$meanCPM,decreasing = TRUE),]
LSZ2fn = subset(LSZ2, sample == "FC")
LSZ2in = subset(LSZ2, sample == "IC")
LSZ2t24 = subset(LSZ2, sample == "T24")
LSZ2fn15 <- slice(LSZ2fn, 1:15)
LSZ2in15 <- slice(LSZ2in, 1:15)
LSZ2t2415 <- slice(LSZ2t24, 1:15)
#____________________________________________
### Make a Bar Plot
### Make a bar plot of the top 10

library(RColorBrewer)

#coul <- brewer.pal(10, "Paired")
#SJR1fn10 <- SJR1fn %>% 
 # slice(1:10) 

sjr1fn <-ggplot(SJR1fn10, aes(x=reorder(group, -meanCPM),y=meanCPM, fill=group))+
  #geom_point()+
  geom_bar(position='stack', stat='identity')+
  theme_minimal()+
  labs(x="", title = "SJR1 FN Top 10 Mean CPM")+
  theme(axis.text.x = element_text(angle = 90, size = 10))+
  theme(axis.title.y = element_text(size = 10))+
  scale_fill_brewer(palette = "RdYlBu")
  p
  #wimGraph()+
  #wimPalettes()
#p + scale_fill_brewer(palette = "RdYlBu")

library(ggpubr)
figure <- ggarrange(ybp2in, ybp2t24, ybp2fn, ybp1in, ybp1t24, ybp1fn,
                      ncol = 3, nrow = 2,
                    common.legend = TRUE, legend = "right")
figure
   
par(mfrow = c(2,3))
barplot(wld2in)
barplot(wld2fn)
barplot(wld2t24)
barplot(lsz2in)
barplot(lsz2fn)
barplot(lsz2t24)

#___________________________________
### Save top ten files as excel docs so I can pring
write_xlsx(SJR1fn15, "data/TopTen/SJR1fn15.xlsx")
write_xlsx(SJR1in15, "data/TopTen/SJR1in15.xlsx")
write_xlsx(SJR1t2415, "data/TopTen/SJR1t2415.xlsx")

write_xlsx(SJR2fn15, "data/TopTen/SJR2fn15.xlsx")
write_xlsx(SJR2in15, "data/TopTen/SJR2in15.xlsx")
write_xlsx(SJR2t2415, "data/TopTen/SJR2t2415.xlsx")

write_xlsx(YBP1fn15, "data/TopTen/YBP1fn15.xlsx")
write_xlsx(YBP1in15, "data/TopTen/YBP1in15.xlsx")
write_xlsx(YBP1t2415, "data/TopTen/YBP1t2415.xlsx")

write_xlsx(YBP2fn15, "data/TopTen/YBP2fn15.xlsx")
write_xlsx(YBP2in15, "data/TopTen/YBP2in15.xlsx")
write_xlsx(YBP2t2415, "data/TopTen/YBP2t2415.xlsx")

write_xlsx(WLD2fn15, "data/TopTen/WLD2fn15.xlsx")
write_xlsx(WLD2in15, "data/TopTen/WLD2in15.xlsx")
write_xlsx(WLD2t2415, "data/TopTen/WLD2t2415.xlsx")

write_xlsx(LSZ2fn15, "data/TopTen/LSZ2fn15.xlsx")
write_xlsx(LSZ2in15, "data/TopTen/LSZ2in15.xlsx")
write_xlsx(LSZ2t2415, "data/TopTen/LSZ2t2415.xlsx")

