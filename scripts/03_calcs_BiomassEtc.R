############################################################################
####### BIOMASS PER CELL, PER GROUP_SIZE, AND INGESTION RATES CALCS ########
############################################################################


### Make a df of biomass per cell, mean, 
##  since there are multiple sizes of cells per group_size

### And a df and plot of mean biomass across all samples

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_Scale Break.R")

### Biomass per cell, mean across groups_size
bpcGrpSz <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, group_size, biomass_cell_pgC, bio_ugC_l) %>% 
  group_by(group_size) %>% 
  summarise(pgC_cell_Mn = mean(biomass_cell_pgC), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame()

save(bpcGrpSz, file = "data/Calculations/bpcGrpSz.Rdata")
write_xlsx(bpcGrpSz, "data/Calculations/BioPerCellGroupSize.xlsx" )

### Ingestion rates of Biomass across all Sampling events, means

load("data/Clearance Rates/Feeding Rates/BpmFRMn_GrpSz.Rdata")
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata")

IRbioOverall <- BpmFRMn_GrpSz %>% 
  group_by(event, group_size) %>% 
  summarise(FrmnUgCcd = FRmnpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame()
IRbioOverall <- IRbioOverall %>% 
  group_by(group_size) %>% 
  summarise(FrmnUgCcd = mean(FrmnUgCcd) ,
            .groups = 'drop') %>% 
  as.data.frame()
save(IRbioOverall, file = "data/Clearance Rates/Feeding Rates/IRbioOverall.Rdata")
write_xlsx(IRbioOverall, "data/Clearance Rates/Feeding Rates/IRbioOverall.xlsx")
           
#Plot, but see plot with scale break below this one
p <- ggplot(IRbioOverall, aes(x=group_size, FrmnUgCcd))+
  geom_point(size = 3)+
  scale_x_discrete () +
  scale_y_continuous(limits = c(-.05, .75), 
                     breaks = c(-.04, - 0, .03, .15, .5, 0.67)) +
  ggtitle("Ingestion Rate, All Sampling Events")+
  xlab("Sampling Event") +
  ylab("µg C"~copepod^-1~d^-1)+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        legend.position = "none")+
  wimGraph()
p

### Use Scale Break function to see dots better
Break      <- 0.058
rescale    <- 20
ylabels    <- c(-.04, - 0, .03, .15, .5, 0.67)
brks       <- scaleBreak(ylabels, Break, rescale)

IRbioOverall$ySquish <- scaleBreak(IRbioOverall$FrmnUgCcd, 
                                   Break=Break, rescale=20)

a <- ggplot(data=IRbioOverall, aes(group_size, ySquish)) +
  geom_point(aes(color = FrmnUgCcd>0), size = 3)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(shape=16, color="navy", size = 3) +
  ##geom_point(stat = "identity",  color = "blue4", size = 4)+
  scale_y_continuous(breaks=brks, labels=ylabels) +
  geom_hline(yintercept= Break, color="green4", linewidth=1, linetype=2) +
  ggtitle("Ingestion Rate, All Sampling Events Combined")+
  xlab("Taxa Groups") +
  ylab("µg C"~copepod^-1~d^-1)+
  theme(plot.title = element_text(face = "bold", size = (14)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 10),
        strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        legend.position = "none")+
  wimGraph()
a 

### 

### Make df's for each samp ev IR bio µgC
load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata")
FRmnWLD2 <- BugFRMn_GrpSz %>% 
  filter(event == "WLD2")
write_xlsx(FRmnWLD2, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnWLD2.xlsx")

FRmnLSZ2 <- BugFRMn_GrpSz %>% 
  filter(event == "LSZ2")
write_xlsx(FRmnLSZ2, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnLSZ2.xlsx")

FRmnSJR2 <- BugFRMn_GrpSz %>% 
  filter(event == "SJR2")
write_xlsx(FRmnSJR2, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnSJR2.xlsx")

FRmnYBP2 <- BugFRMn_GrpSz %>% 
  filter(event == "YBP2")
write_xlsx(FRmnYBP2, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnYBP2.xlsx")

FRmnYBP1 <- BugFRMn_GrpSz %>% 
  filter(event == "YBP1")
write_xlsx(FRmnYBP1, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnYBP1.xlsx")

FRmnSJR1 <- BugFRMn_GrpSz %>% 
  filter(event == "SJR1")
write_xlsx(FRmnSJR1, "data/Clearance Rates/Feeding Rates/Means/Bio_SampEv/FRmnSJR1.xlsx")





