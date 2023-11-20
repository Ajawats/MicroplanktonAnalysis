############################################################################
########################## ABUNDANCE PLOTS AUGUST 2023  ####################
############################################################################


#################################### READ THIS FIRST !!!!! ##################
### The most updated plots are in the calculations file, not here!
##  in 03_calcs_BiomassAbundance.R ####################################

### 8/9/23 Replot with µgC L^-1, 

### 8/8/23 Plot code also found in 03_calcs_BiomassAbundance.R
### Plots saved in MicroplanktonAnalysis/Final Final/Abundance/
### 8/7/23

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")

### Plot Abundance in biomass, absolute and proportionate by event, Top 5 + Other taxa groups
load("Final Final/Abundance/AI5TotProp.Rdata") # For proportions and absolute, from 03_calcs_BiomassAbundance.R

### Proportions
ggplot(AI5TotProp, aes(fill=taxaGroup, y=PropBioUgCl, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagSm" = "#85B22C", "Other" = "peachpuff"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "Other"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Relative Carbon Biomass")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()


### Absolute, 17 taxa groups
load("Final Final/Abundance/AISumAgg17.Rdata")
ggplot(data= AISumAgg17,aes(x = factor(taxaGroup, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                             "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                             "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                            y=BioUgL)) +  
  geom_point()+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
                   #  limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  xlab("Taxa Groups")+
  ylab("Biomass, µgC"~L^-1)+
  #ggtitle("Biomass")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6))+
  wimGraph()

### Absolute, Top 5 + Other taxa groups
ggplot(data= AI5TotProp,aes(x = factor(taxaGroup, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "Other")),
                            color = taxaGroup, y=BioUgL)) +  
  geom_point()+
  scale_color_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                                "FlagSm" = "#85B22C", "Other" = "dimgrey"))+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+#,
   # limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  xlab("Taxa Groups")+
  ylab("Biomass, µgC"~L^-1)+
  #ggtitle("Biomass")+
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
ggplot(subset(AISumAgg17, samp_ev %in%"LSZ2"), 
       aes(x = factor(group_size, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                            "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                            "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
           y=BioPgMl)) + 
  geom_point( size = 2)+
  #scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
  xlab("Taxa Groups")+
  ylab("pgC"~ml^-1)+
  ggtitle("LSZ2 Biomass Abundance")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9))+
  wimGraph()

### Plots saved in MicroplanktonAnalysis/Final Final/Abundance/

### Plot individual AISumAgg17 using paginate
library(ggforce)

load("Final Final/Abundance/AISumAgg17.Rdata")
ggplot(data= AISumAgg17,aes(x = factor(taxaGroup, level = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm",
                                                            "ChlLg","ChlSm","ChnDiaLg","ChnDiaSm","CyanoLg","CyanoSm",
                                                            "DinoLg","FlagLg","PenDiaLg","PenDiaSm","UnidLg","UnidSm")),
                            y=BioUgL)) +  
  geom_point()+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)))+
  #  limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  
  facet_wrap(~event, ncol = 2, scales = "free")+
  #facet_wrap_paginate(~ event, ncol = 2, nrow =2, page = 3)
  xlab("Taxa Groups")+
  ylab("Biomass, µgC"~L^-1)+
  #ggtitle("Biomass")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6))+
  wimGraph()

