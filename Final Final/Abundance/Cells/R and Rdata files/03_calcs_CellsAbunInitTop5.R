#########################################################################
########## CELL ABUNDANCE INITIALS; AND RELATIVE ABUNDANCE############
########################## WITH PLOTS ###################################
#########################################################################

### 11/16/23 Copied from 03_calcs_BiomassAbunInitTop5.R and modified for
##  calculating cell counts abundance instead of biomass

library(tidyverse)
library(writexl)

### Use the df that has the top 5 taxa, plus "other" This includes
##  all remaining 12 taxa groups in the other category, no taxa are excluded
## It's the same as volbio_all_cr.Rdata, but has a column for the top 5 + other 
##  taxa group categories
load("data7_24/FinalAnalysis/baseTop5.Rdata") # from 03_calcs_Top5.R

### R data files created in this script:
load("Final Final/Abundance/Cells/R and Rdata files/AI5CellsTotByTxSE.Rdata")
load("Final Final/Abundance/Cells/R and Rdata files/AI5CellsTotProp.Rdata")

######################### INITIAL SAMPLES ############################
### Filter for the intial samples only
abundanceI <- baseTop5 %>% 
  filter(exp == "I")

### Sum the multiple entries of the same rep as in the example above
AIsumCellsTaxaGroups <- aggregate(cpm ~ samp_ev + rep+ taxaGroup, 
                                data = abundanceI, sum)
### Take the means of the reps to get the useable, correct number 
AI5CellsTotByTxSE <- aggregate(cpm ~ samp_ev + taxaGroup, 
                             data = AIsumCellsTaxaGroups, mean) %>% 
  rename(TotCpm = cpm)
save(AI5CellsTotByTxSE, file = "Final Final/Abundance/Cells/R and Rdata files/AI5CellsTotByTxSE.Rdata")
write_xlsx(AI5CellsTotByTxSE, "Final Final/Abundance/Cells/Excel Docs/AI5CellsTotByTxSE.xlsx")

## 1) Get the total cpm for each event
AI5CellsTotEvTx <- AI5CellsTotByTxSE %>% 
  group_by(samp_ev) %>% 
  summarise(TotEvCpmSum = sum(TotCpm)) %>% 
  ungroup
## 2) Join it with the taxaGroup totals
AI5CellsAll <- left_join(AI5CellsTotByTxSE, AI5CellsTotEvTx)
## 3) Calculate the proportion that each taxaGroup cpm contributed to total
AI5CellsTotProp <- AI5CellsAll %>% 
  mutate(PropCpm = TotCpm/TotEvCpmSum) %>% 
  rename(event = samp_ev)
AI5CellsTotProp <- AI5CellsTotProp %>% 
  mutate(PercentCpm = PropCpm*100)

save(AI5CellsTotProp, file = "Final Final/Abundance/Cells/R and Rdata files/AI5CellsTotProp.Rdata") 
load("Final Final/Abundance/Cells/R and Rdata files/AI5CellsTotProp.Rdata")
write_xlsx(AI5CellsTotProp, "Final Final/Abundance/Cells/Excel Docs/AI5CellsTotProp.xlsx")


################################# PLOT IT ##################################
############################################################################
source("scripts/01_function_wimGraph and Palettes.R")

### Proportions

ggplot(AI5BioTotProp, aes(fill=taxaGroup, y=PropBioPgCm, x=event)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = 
                      c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue",
                        "CilLg" = "salmon3", "CilSm" = "salmon1","FlagSm" = "#85B22C", 
                        "Other" = "peachpuff"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "Other"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Initials Relative Biomass Abundance, pgC"~mL^-1)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()



### Absolute
ggplot(data= AI5BioTotProp, aes(taxaGroup, BioUgL, color = taxaGroup)) + 
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

