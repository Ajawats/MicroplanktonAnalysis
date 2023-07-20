#################################################################
########## PLOT TEST OF NEW CR W/ NAS #########################
#################################################################

##### 6/5/23
library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
source("scripts/01_function_Scale Break.R")
load("data/Clearance Rates 2/sumCpm_CRmn.Rdata") # Clearance Rates means only

### Make a test df with just LSZ2, of just the means, sumCpm_CRmn

LSZ2_CrMn <- sumCpm_CRmn %>% 
  filter(event =="LSZ2")
 ### Plot, this works, the NA taxa have no bars
p <- ggplot(LSZ2_CrMn, aes(x=group_size, CrMNmlcd))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Clearance Rate, Cells per mL")+
  wimGraph()
p

### Try CR with all events. This works
p <- ggplot(sumCpm_CRmn, aes(x=group_size, CrMNmlcd))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_x_discrete ("") +
  scale_y_continuous(trans = "log10") +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 11.5))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Clearance Rate, Cells per mL, log scale")+
  wimGraph()
p

### IR bio
#load("data/Clearance Rates 2/IrCrBio_Rep_Mn.Rdata") # Clearance rates reps and means,
#  with ingestion rates, biomass, reps and means, pgC and µgC
### Don't use ths for plotting IRbio means. For some reason, it messed up the numbers
p <- ggplot(IrCrBio_Rep_Mn, aes(x=group_size, FRmnUgCcd))+
  geom_bar(stat = "identity", fill = "darkgray", width = 0.5)+
  scale_x_discrete ("") +
  scale_y_continuous()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 11.5))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Ingestion Rate, µgC per day")+
  wimGraph()
p

### IR bio, this one has breaks and limits for SJR2
p <- ggplot(IrCrBio_Rep_Mn, aes(x=group_size, FRmnUgCcd))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  scale_x_discrete ("") +
  scale_y_continuous(breaks = c(-0.006, 0, 0.01, .02, .04, .05), limits = c(-.007, .05))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 11.5))+
  #facet_wrap(~ event, ncol= 2, scales="free") +
  facet_wrap_paginate(~ event, ncol = 1, nrow =1, page = 3)+
  xlab("Taxa Groups") +
  ylab("Ingestion Rate, µgC per day")+
  wimGraph()
p

# Ingestion rates cells, reps and means
load("data/Clearance Rates 2/Feeding Rates/FRCells_Rep_Mn.Rdata") 
p <- ggplot(FRCells_Rep_Mn, aes(x=group_size, FRmnCellsCd))+
  geom_bar(stat = "identity", fill = "darksalmon")+
  #scale_y_continuous(breaks = FRmnCellsCd, 
                    # labels = format(FRmnCellsCd, scientific=TRUE, digits=4))+
  scale_x_discrete ("") +
  scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 11.5))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Taxa Groups") +
  ylab("Ingestion Rate, Cells per day")+
  wimGraph()
p

### CR means, taxa only
p <- ggplot(sumCRmnTaxa, aes(x=group_size, MeanCR))+
  geom_bar(stat = "identity", fill = "gray")+
  scale_x_discrete ("") +
  scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Groups") +
  ylab("Clearance Rate, Cells per mL")+
  wimGraph()
p

### CR means, events only
p <- ggplot(sumCRmnEventsOnly, aes(x=event, MeanCR))+
  geom_bar(stat = "identity", fill = "gray")+
  scale_x_discrete ("") +
  scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Sampling Event") +
  ylab("Clearance Rate, Cells per mL")+
  wimGraph()
p

### IR means, events only
p <- ggplot(FRBio_Event, aes(x=event, FRmnUgCcd))+
  geom_bar(stat = "identity", fill = "gray")+
  scale_x_discrete ("") +
  scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Sampling Event") +
  ylab("Ingestion Rate, µgC per day")+
  wimGraph()
p

### IR means, taxa only
p <- ggplot(FRBio_Taxa, aes(x=group_size, FRmnUgCcd))+
  geom_bar(stat = "identity", fill = "gray")+
  scale_x_discrete ("") +
  scale_y_continuous() +#, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14))+
  xlab("Taxa Group") +
  ylab("Ingestion Rate, µgC per day")+
  wimGraph()
p

a <- ggplot(data=FRBio_Taxa, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates, Biomass")+
  scale_y_continuous() +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### IR bio means, dot plot
a <- ggplot(data=BugFRMn_GrpSz, aes(group_size, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 3)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  xlab("Taxon Group") +
  ggtitle("Ingestion Rates, Biomass")+
  scale_y_continuous() +
  facet_wrap(~ event, ncol= 2, scales="free") +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a
