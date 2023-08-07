############################################################################
########################## ABUNDANCE PLOTS AUGUST 2023  #########################
############################################################################

### 8/7/23

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")

### Plot Abundance in biomass, absolute and proportionate by event, Top 5 + Other taxa groups
load("Final Final/Abundance/AI5TotProp.Rdata") # For proportions and absolute, from 03_calcs_BiomassAbundance.R

### Proportions
ggplot(AI5TotProp, aes(fill=taxaGroup, y=PropBioPgCm, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagSm" = "#85B22C", "Other" = "peachpuff"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "Other"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Biomass Abundance, pgC"~mL^-1)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()


### Absolute
ggplot(data= AI5TotProp, aes(taxaGroup, BioPgMl)) + 
  geom_point()+
  scale_y_continuous(expand=expansion(mult=c(.1,0.15)),
                     limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm"))+
  facet_wrap(~event, ncol = 2, scales = "free")+
  xlab("Taxa Groups")+
  ylab("pgC"~ml^-1)+
  ggtitle("Biomass Abundance")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6))+
  wimGraph()

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

