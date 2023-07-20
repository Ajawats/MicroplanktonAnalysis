############################################################################
################# INGESTION RATES PLOTS, FACET_WRAPPED #####################
############################################################################

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")

### Plot for ingestion rates, cells, all events and taxa groups
## and ingestion rates, biomass

### Cells, means only
p <- 		ggplot(CpmFRMn_GrpSz, aes(x=group_size, FRmnCellsCd)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous()+#breaks = seq(-35, 70, by=10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Cells"~copepod^-1~d^-1) +
  ggtitle("Ingestion Rates, Cells")+
  geom_hline(yintercept = 0, color = "blue")+
  #geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = (13)))
p

load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata") #Ingestion Rates,
### biomass, means only, µg C and pg C
load( "data/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

### Biomass, reps and means?
p <- 		ggplot(BugFRMn_GrpSz, aes(x=group_size, FRmnUgCcd)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous()+#breaks = seq(-35, 70, by=10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("µg C"~copepod^-1~d^-1) +
  ggtitle("Ingestion Rates, Biomass")+
  geom_hline(yintercept = 0, color = "blue")+
  #geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = (13)))
p

### Biomass, reps, flipped coordinates to see better the taxa groups by size of rate
## It changed the order but I'm not sure by which event or rates. 
## Below, try to do it with the individual plots using  facet_wrap_paginate
## It didn't work, but see 04_plots_CRFR__Samp_Ev.R for how it works with the 
##   individual event dfs.
p <- 		ggplot(BugFRMn_GrpSz, aes(x=reorder(group_size, -FRmnUgCcd),  y=FRmnUgCcd)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous()+#breaks = seq(-35, 70, by=10))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("µg C"~copepod^-1~d^-1) +
  ggtitle("Ingestion Rates, Biomass")+
  geom_hline(yintercept = 0, color = "blue")+
  #geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = (13)))
p


### Try with facet_wrap_paginate(~ event, ncol = 2, nrow =1, page = 3)
p <- 		ggplot(BugFRMn_GrpSz, aes(x=reorder(group_size, -FRmnUgCcd),  y=FRmnUgCcd)) +
  geom_point(size=2, color="coral") +
  scale_x_discrete ("") +
  scale_y_continuous()+#breaks = seq(-35, 70, by=10))+
  #facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("µg C"~copepod^-1~d^-1) +
  ggtitle("Ingestion Rates, Biomass")+
  geom_hline(yintercept = 0, color = "blue")+
  facet_wrap_paginate(~ event, ncol = 1, nrow =1, page = 3, scales = "free")+
  #geom_hline(yintercept = 5, color = "cyan")+
  wimGraph(textSize = 6)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 6),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = (13)))
p
