############################################################################
########################## BIOMASS PLOTS JULY 2023  #########################
############################################################################

### 7/13/23

library(tidyverse)
library(writexl)
source("scripts/01_function_wimGraph and Palettes.R")
load("data/Clearance Rates 2/CrIrCntMnTots2.Rdata")
### Biomass
### Put numbers on top of bars
#geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
#         vjust = -.25, size = 3)
### Make it so numbers on highest bars don't get cut off
# scale_y_continuous(trans= "log10", expand = expansion(mult = c(0, 0.1)))+
### Round the numbers to 3 decimal places so they can be read on the plot
IrMnTotRound <- CrIrCntMnTots2 %>% 
  mutate_at(6, round, 3)
p <- ggplot(subset(IrMnTotRound, event %in% "YBP1"), aes(group_size, FRUgMn))+
  geom_bar(stat = "identity", fill = "grey")+
  geom_text(aes(label=FRUgMn), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  ggtitle("YBP1 Ingestion Rates, Biomass")+
  scale_x_discrete ("", expand=expansion(mult=c(0.07,0.07))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9.5),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 11),
        title = element_text(size = 12))+
  ylab("µg C"~d^-1) +
  xlab("Taxa Groups")
p
### Save the plot as size 6 x 5.5, in data/Ingestion Rates July 2023

### Try all events with facet_wrap
p <- ggplot(IrMnTotRound, aes(group_size, FRUgMn))+
  geom_bar(stat = "identity", fill = "grey")+
  geom_text(aes(label=FRUgMn), position = position_dodge(width = 0.9), 
            vjust = -1, angle = 35, size = 2.5)+
  facet_wrap(~ event, ncol= 2, scales="free") +
  ggtitle("Ingestion Rates, Biomass")+
  scale_x_discrete ("", expand=expansion(mult=c(0.07,0.07))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.25))) +
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 7.5),
        strip.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 11),
        title = element_text(size = 12))+
  ylab("µg C"~d^-1) +
  xlab("Taxa Groups")
p
### Save the plot as size 8 x 6.5, in data/Ingestion Rates July 2023


########################## 7/14/23 ##########################################
### Plot the mean IR bio per event, take the means of the replicates for each 
##  taxa group per event, and average them, exclude from the sum any negative 
##  rates, also exclude the NA values
load("data/Clearance Rates 2/CrIrCntRepMnTots.Rdata")
MeanIRplotEvents <- CrIrCntMnTots2 %>% 
  group_by(event) %>% 
  summarise(FRUgMnEvent = mean(FRUgMn[FRUgMn >= 0 ], na.rm = TRUE))
### Put numbers on top of bars
#geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
#         vjust = -.25, size = 3)
### Make it so numbers on highest bars don't get cut off
# scale_y_continuous(trans= "log10", expand = expansion(mult = c(0, 0.1)))+
MeanIrplotEventsRnd <- MeanIRplotEvents %>% 
  mutate_at(2, round, 3)
p <- ggplot(MeanIrplotEventsRnd, aes(event, FRUgMnEvent))+
  geom_bar(stat = "identity", fill = "grey")+
  geom_text(aes(label=FRUgMnEvent), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 3)+
  ggtitle("Mean Biomass Ingestion Rates")+
  scale_x_discrete ("", expand=expansion(mult=c(0.07,0.07))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
  wimGraph()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9.5),
    strip.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 11),
    title = element_text(size = 11))+
  ylab("µg C"~d^-1) +
  xlab(NULL)
p


### Plot the mean IRbio per taxa group, take the means of the replicates for each 
##  taxa group across all events, and average them, exclude from the sum any 
##  negative rates, also exclude the NA values
load("data/Clearance Rates 2/CrIrCntRepMnTots.Rdata")
MeanIRplotTaxa <- CrIrCntMnTots2 %>% 
  group_by(group_size) %>% 
  summarise(FRUgMnTaxa = mean(FRUgMn[FRUgMn >= 0 ], na.rm = TRUE))
### Put numbers on top of bars
#geom_text(aes(label=TotalCpmI), position = position_dodge(width = 0.9), 
#         vjust = -.25, size = 3)
### Make it so numbers on highest bars don't get cut off
# scale_y_continuous(trans= "log10", expand = expansion(mult = c(0, 0.1)))+
MeanIRplotTaxaRnd <- MeanIRplotTaxa %>% 
  mutate_at(2, round, 3)
p <- ggplot(MeanIRplotTaxaRnd, aes(group_size, FRUgMnTaxa))+
  geom_bar(stat = "identity", fill = "grey")+
  geom_text(aes(label=FRUgMnTaxa), position = position_dodge(width = 0.9), 
            vjust = -.25, size = 2.5)+
  ggtitle("Mean Biomass Ingestion Rates")+
  scale_x_discrete ("", expand=expansion(mult=c(0.07,0.07))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 9.5),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 11),
        title = element_text(size = 11))+
  ylab("µg C"~d^-1) +
  xlab(NULL)
p

############ OLD CODE ##########################
### Biomass; Used the facet_wrap_paginate to get the separate event plots
p <- ggplot(FrGrps, aes(x=FRmnBpm, group_size))+
  geom_bar(stat = "identity", fill = "darkseagreen3")+
  scale_y_discrete ("") +
  scale_x_continuous(trans = "log10") + #, breaks = c(2, 10, 25, 100, 200, 500, 2500, 10000))+
  wimGraph()+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 12),
        strip.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 6))+
  facet_wrap(~ event, ncol= 2, scales="free") +
  xlab("Ingestion Rate, pgC per copepod per day") +
  ylab("Taxa Groups with Sizes")+
  facet_wrap_paginate(~ event, ncol = 2, nrow =1, page = 3)
p


