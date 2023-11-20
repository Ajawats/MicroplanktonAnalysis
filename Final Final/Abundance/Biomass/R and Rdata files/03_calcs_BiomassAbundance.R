############################################################################
#################### ABUNDANCE in terms of BIOMASS   #####################
############################################################################

### 8/9/23 Changed data to plot µgC L^-1 instead of pgC ml-1, and changed plot titles
##  no need to put the word "Abundance" in there
### 8/7/23  INCLUDES PLOTS  ####################
############################################################

######## 8/7/23  NOTE!!  I'm doing abundance by biomass, not counts ##########


### 7/28/23 Used 03_calcs_Abun_Other.R in scripts folder as a template for this
##  Need to recalculate abundance in terms of biomass, not cell counts.
##  Use the top 5 plus other taxa group data. Save all in new folder, "Final Final"

### 7/24/23 updated files to include the YBP1 centric diatom large exp rep 2 count to 1
##  and created a new data folder data7_24 because I lost the data folder when I made a
##  Git Hub repository.

### Accuracy checked all the scripts 06/23
### Several Calculations for looking at IR, CR, abundance, etc., to be able
##  to whittle down groups for final analysis
library(tidyverse)
library(writexl)

### Use the df that has the top 5 taxa, plus "other" This includes
##  all remaining 12 taxa groups in the other category, no taxa are excluded
load("data7_24/FinalAnalysis/baseTop5.Rdata") # from 03_calcs_Top5.R

### 4/25/23 Make a df that has CR, IRbio, mn pgC per cell and abundance, cpm initials

######## 8/7/23  NOTE!!  I'm doing abundance by biomass, not counts ##########
### 4/21/23
##  Abundance, cells per mL, by sampling events and taxa groups. Use
##  initial samples cpm. First sum the cpm of the groups that make up the
##  group_size groups.
### Reminder that Small  is esd < 15µm, and Large is esd >= 15 µm


### 8/1/23 I lost the work I did yesterday, and thought this way below
##  might be better, but I'm not sure. I'm taking a break from it now

######################### INITIAL SAMPLES ############################
### Filter for the intial samples only
abundanceI <- baseTop5 %>% 
  filter(exp == "I")

### NO!!!take the mean of all the biomass in pgC per ml, by event and taxaGroup, top 5 and other
## This is not correct. Some taxaGroups are comprised of multiple entries of different sized
## organisms, like CenDiaSm has separate rows for the 5.5x5.5 diatoms and the 9.5x9.5 diatoms
## Those two need to be added together per replicate to get the total for that replicate
## Then those three replicates are added together to calculate the mean.
## The way this aggregate code works is to add all six rows of the three replicates (that
## consist of two rows each for 5.5 and 9.5) and take the mean of those. This 
## 3 results in a number that's half of what it's supposed to be.
AImnAgg5 <- aggregate(bio_pgC_ml ~ samp_ev + taxaGroup, 
                     data = abundanceI, mean)
### Add a column of biomass in ugC per Liter
AImnAgg5 <- AImnAgg5 %>% 
  rename(mnBioPgMl = bio_pgC_ml) %>% 
  mutate(mnBioUgL = mnBioPgMl*.001)
save(AImnAgg5, file = "Final Final/Abundance/AImnAgg5.Rdata")
write_xlsx(AImnAgg5, "Final Final/Abundance/AImnAgg5.xlsx")

### sum of all the biomass in pgC per ml, by event and taxaGroup, top 5 and other
AISumAgg5 <- aggregate(bio_pgC_ml ~ samp_ev + taxaGroup, 
                      data = abundanceI, sum)
### Add a column of biomass in ugC per Liter
AISumAgg5 <- AISumAgg5 %>% 
  rename(BioPgMl = bio_pgC_ml) %>% 
  mutate(BioUgL = BioPgMl*.001)
save(AISumAgg5, file = "Final Final/Abundance/AISumAgg5.Rdata")
write_xlsx(AISumAgg5, "Final Final/Abundance/AISumAgg5.xlsx")

### do same as above but with the 17 taxa groups, group_size
AImnAgg17 <- aggregate(bio_pgC_ml ~ samp_ev + group_size, 
                      data = abundanceI, mean)
AImnAgg17 <- AImnAgg17 %>% 
  rename(mnBioPgMl = bio_pgC_ml) %>% 
  mutate(mnBioUgL = mnBioPgMl*.001)
save(AImnAgg17, file = "Final Final/Abundance/AImnAgg17.Rdata")
write_xlsx(AImnAgg17, "Final Final/Abundance/AImnAgg17.xlsx")

### Sum biomass for the 17 taxa groups, group_size
AISumAgg17 <- aggregate(bio_pgC_ml ~ samp_ev + group_size, 
                       data = abundanceI, sum)
AISumAgg17 <- AISumAgg17 %>% 
  rename(BioPgMl = bio_pgC_ml) %>% 
  mutate(BioUgL = BioPgMl*.001)
AISumAgg17 <- AISumAgg17 %>% 
  rename(taxaGroup = group_size)
save(AISumAgg17, file = "Final Final/Abundance/AISumAgg17.Rdata")
write_xlsx(AISumAgg17, "Final Final/Abundance/AISumAgg17.xlsx")

### Make another column with the proportion of the whole
## 1) Get the total biomass pgC ml-1 for each event and taxa group
AIbioTotEvTx5 <- abundanceI %>% 
  group_by(samp_ev) %>% 
  summarise(TotEvBioPgCm = sum(bio_pgC_ml[bio_pgC_ml>=0])) %>% 
    ungroup
## 2) Join it with the taxaGroup totals
AI5Tot <- left_join(AISumAgg5, AIbioTotEvTx5)
## Put it in µgC L
AI5Tot <- AI5Tot %>% 
  mutate(TotEvBioUgCL = TotEvBioPgCm*.001) 
## 3) Calculate the proportion that each taxaGroup biomass contributed to total
AI5TotProp <- AI5Tot %>% 
  mutate(PropBioPgCm = BioPgMl/TotEvBioPgCm,
         PropBioUgCl = BioUgL/TotEvBioUgCL) %>% 
  rename(event = samp_ev)
save(AI5TotProp, file = "Final Final/Abundance/AI5TotProp.Rdata") 
load("Final Final/Abundance/Biomass/AI5TotProp.Rdata")
write_xlsx(AI5TotProp, "Final Final/Abundance/Biomass/AI5TotProp.xlsx")

### Do the proportions for all 17 taxa groups
## Join AISumAGg17 with AIbioTotEvTx5
### rename samp_ev to event
AIbioTotEvTx5 <- AIbioTotEvTx5 %>% 
  rename(event = samp_ev)
AI17Tot <- left_join(AISumAgg17, AIbioTotEvTx5)
## Put it in µgC L
AI17Tot <- AI17Tot %>% 
  mutate(TotEvBioUgCL = TotEvBioPgCm*.001)
## 3) Calculate the proportion that each taxaGroup biomass contributed to total
AI17TotProp <- AI17Tot %>% 
  mutate(PropBioPgCm = BioPgMl/TotEvBioPgCm,
         PropBioUgCl = BioUgL/TotEvBioUgCL)
save(AI17TotProp, file = "Final Final/Abundance/AI17TotProp.Rdata") 
load("Final Final/Abundance/Biomass/AI17TotProp.Rdata")
write_xlsx(AI17TotProp, "Final Final/Abundance/Biomass/AI17TotProp.xlsx")

################################# PLOT IT ##################################
############################################################################
source("scripts/01_function_wimGraph and Palettes.R")

### Proportions
ggplot(AI17TotProp, aes(fill=taxaGroup, y=PropBioPgCm, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    #limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "Other"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Relative Carbon Biomass")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()


### Absolute
ggplot(data= AI5TotProp, aes(taxaGroup, BioUgL, color = taxaGroup)) + 
  geom_point()+
  scale_color_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagSm" = "#85B22C", "Other" = "dimgrey"))+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  xlab(NULL)+
  ylab("Biomass, µgC"~L^-1)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        legend.position = "none")+
  wimGraph()
### saved plot as 5x5

### Relative biomass abundance showed >10% "Other" in SJR2, WLD2, YBP1 and YBP2,
##  so break them out individually
load("Final Final/Abundance/AISumAgg17.Rdata")
ggplot(subset(AISumAgg17, samp_ev %in%"SJR1"), 
       aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                            "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                            "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
           y=BioUgL, color = group_size)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", 
                                "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagSm" = "#85B22C",  "dimgrey", "dimgrey", "dimgrey",
                                "dimgrey", "dimgrey", "dimgrey", "dimgrey",  "dimgrey",
                                "dimgrey", "dimgrey", "dimgrey", "dimgrey"))+
  #scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
  xlab("Taxa Groups")+
  ylab("µgC"~L^-1)+
  ggtitle("SJR1")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 9),
        #axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9),
        legend.position = "none")+
  wimGraph()
### save as 4 X 5

### Also plot all 17 taxa groups, Absolute
ggplot(data= AISumAgg17,
       aes(x = factor(taxaGroup, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                            "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                            "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                             y=BioUgL, color = taxaGroup)) + 
  geom_point()+
  scale_color_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagSm" = "#85B22C",  "dimgrey", "dimgrey", "dimgrey",
                               "dimgrey", "dimgrey", "dimgrey", "dimgrey",  "dimgrey",
                               "dimgrey", "dimgrey", "dimgrey", "dimgrey"))+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+#,
                     #limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  xlab(NULL)+
  ylab("µgC"~L^-1)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        legend.position = "none")+
  wimGraph()
### Plot saved as 6 x 5.5

### Plots saved in MicroplanktonAnalysis/Final Final/Abundance/

### Plot individual AISumAgg17 using paginate
library(ggforce)

load("Final Final/Abundance/AISumAgg17.Rdata")
abSJR2 <-ggplot(data= AISumAgg17,
       aes(x = factor(taxaGroup, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                            "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                            "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
           y=BioUgL, color = taxaGroup)) + 
  geom_point()+
  scale_color_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagSm" = "#85B22C",  "dimgrey", "dimgrey", "dimgrey",
                                "dimgrey", "dimgrey", "dimgrey", "dimgrey",  "dimgrey",
                                "dimgrey", "dimgrey", "dimgrey", "dimgrey"))+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+#,
  #limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  #facet_wrap(~samp_ev, ncol = 2, scales = "free")+
  facet_wrap_paginate(~ event, ncol = 1, nrow =1, page = 3, scales = "free")+
  xlab(NULL)+
  ylab("µgC"~L^-1)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        legend.position = "none")+
  wimGraph()
abSJR2
### Plot saved as 6 x 5.5

############################ SITE WATER BIOMASS ABUNDANCE ###############################
#########################################################################################
### Proportions
ggplot(siteBioUgLTot, aes(fill=group_size, y=tot_bio_ug, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Site Water, Carbon Biomass")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()


