#########################################################################
########## BIOMASS ABUNDANCE INITIALS; AND RELATIVE ABUNDANCE############
########################## WITH PLOTS ###################################
#########################################################################

### 11/14/23 Need to make changes as noted below around line 25 or 30. This is a copy of the
## original R script, 03_calcs_BiomassAbundance.R, that I am modifying
###  11/16/23 Further corrections made to sum of reps and sums of taxa group
##  biomass per events

### For Abundance of all 17 taxa groups and for site water, go to
##  03_calcs_BiomassAbundance.R, but check for corrections first. Corrections
##  not made as of 11/16/23

library(tidyverse)
library(writexl)

### Use the df that has the top 5 taxa, plus "other" This includes
##  all remaining 12 taxa groups in the other category, no taxa are excluded
## It's the same as volbio_all_cr.Rdata, but has a column for the top 5 + other 
##  taxa group categories
load("data7_24/FinalAnalysis/baseTop5.Rdata") # from 03_calcs_Top5.R

### R data files created in this script:
load("Final Final/Abundance/Biomass/R and Rdata files/AI5BioTotByTxSE.Rdata")
load("Final Final/Abundance/Biomass/R and Rdata files/AI5BioTotProp.Rdata")

######################### INITIAL SAMPLES ############################
### Filter for the intial samples only
abundanceI <- baseTop5 %>% 
  filter(exp == "I")

### Per accuracy check 11/14/23,  Some taxaGroups are comprised of multiple entries of 
##  different sized organisms, like CenDiaSm has separate rows for the 5.5x5.5 diatoms 
##  and the 9.5x9.5 diatoms. Those two need to be added together per replicate to get 
##  the total for that replicate. Then those three replicates are added together to 
##  calculate the mean. The way this aggregate code works is to add all six rows of 
##  the three replicates (that consist of two rows each for 5.5 and 9.5) and take the 
##  mean of those. This  3 results in a number that's half of what it's supposed to be.

### Sum the multiple entries of the same rep as in the example above
AIsumBioTaxaGroups <- aggregate(bio_pgC_ml ~ samp_ev + rep+ taxaGroup, 
                             data = abundanceI, sum)
### Take the means of the reps to get the useable, correct number 
AI5BioTotByTxSE <- aggregate(bio_pgC_ml ~ samp_ev + taxaGroup, 
                      data = AIsumBioTaxaGroups, mean)
### Add a column of biomass in ugC per Liter
AI5BioTotByTxSE <- AI5BioTotByTxSE %>% 
  rename(TotBioPgMl = bio_pgC_ml) %>% 
  mutate(TotBioUgL = TotBioPgMl*.001)
save(AI5BioTotByTxSE, file = "Final Final/Abundance/Biomass/R and Rdata files/AI5BioTotByTxSE.Rdata")
write_xlsx(AI5BioTotByTxSE, "Final Final/Abundance/Biomass/Excel Docs/AI5BioTotByTxSE.xlsx")


## 1) Get the total biomass pgC ml-1 for each event
AI5BioTotEvTx <- AI5BioTotByTxSE %>% 
  group_by(samp_ev) %>% 
  summarise(TotEvBioPgCm = sum(TotBioPgMl[TotBioPgMl>=0])) %>% 
  ungroup
## 2) Join it with the taxaGroup totals
AI5BioAll <- left_join(AI5BioTotByTxSE, AI5BioTotEvTx)
## Put it in µgC L
AI5BioAll <- AI5BioAll %>% 
  mutate(TotEvBioUgCL = TotEvBioPgCm*.001) 
## 3) Calculate the proportion that each taxaGroup biomass contributed to total
AI5BioTotProp <- AI5BioAll %>% 
  mutate(PropBioPgCm = TotBioPgMl/TotEvBioPgCm,
         PropBioUgCl = TotBioUgL/TotEvBioUgCL) %>% 
  rename(event = samp_ev)
AI5BioTotProp <- AI5BioTotProp %>% 
  mutate(PercentBioPgCm = PropBioPgCm*100,
         PercentBioUgCl = PropBioUgCl*100)

save(AI5BioTotProp, file = "Final Final/Abundance/Biomass/R and Rdata files/AI5BioTotProp.Rdata") 
load("Final Final/Abundance/Biomass/R and Rdata files/AI5BioTotProp.Rdata")
write_xlsx(AI5BioTotProp, "Final Final/Abundance/Biomass/Excel Docs/AI5BioTotProp.xlsx")


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

