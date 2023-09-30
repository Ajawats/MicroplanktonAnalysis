##############################################################################
  ############### INTIAL TOP 5+OTHER CPM TOTALS #########################
##############################################################################

### Code copied from 03_calcs_SiteWaterAbund.R, then modified to include initials
##  top 5 + other abundance and relative abundance
library(tidyverse)
library(writexl)

############## INITIALS, TOP 5 + OTHER, WITH PROPORTIONS #####################
##############################################################################
### Use baseTop5.Rdata as the base. It's the same as volbio_all_cr.Rdata, but
##  has a column for the top 5 + other taxa group categories
load("data7_24/FinalAnalysis/baseTop5.Rdata")
initials5cpm <- baseTop5 %>%
  filter(exp == "I") %>% 
  select(samp_ev, exp, rep, grp_sz, counts, cpm, group_size, taxaGroup)

### take the mean of the 3 replicates of each taxaGroup
I5cpmTot <- initials5cpm %>% 
  group_by(samp_ev, grp_sz) %>% 
  mutate(cpmTotMnofReps = mean(cpm))
### Remove the unneeded columns
I5cpmTot2 <- I5cpmTot %>% 
  subset(select = c(samp_ev, grp_sz, taxaGroup, cpmTotMnofReps))
### Remove the rows that are "duplicates" because I only want to keep one
##  mean value, not the mean value for all three replicates
I5cpmTot3 <- I5cpmTot2 %>% distinct()
### Sum the counts that make up each of the 5+other taxa groups, sum
##  cpmTotMnofReps per event per taxa group
I5cpmTot4 <- I5cpmTot3 %>% 
  group_by(samp_ev, taxaGroup) %>% 
  mutate(totCpmTxEv = sum(cpmTotMnofReps)) %>% 
  ungroup
### Remove grp_sz and cpmMnTot... columns, then remove the "duplicates"
##  so I have just one entry per event per taxa group, and not the extra rows
##  that were the reps
I5cpmTot5<- I5cpmTot4 %>% 
  subset(select = c(samp_ev, taxaGroup, totCpmTxEv)) %>% 
  distinct()
save(I5cpmTot5, file = "Final Final/Abundance/Cells/Top5/I5cpmTot5.Rdata")
write_xlsx(I5cpmTot5, "Final Final/Abundance/Cells/Top5/I5cpmTot5.xlsx")

### Add a column of total cpm, all taxa, per sampling event
I5cpmTotEverything <- I5cpmTot5 %>% 
  group_by(samp_ev) %>% 
  mutate(cpmTotallTxperEv = sum(totCpmTxEv)) %>% 
  ungroup
save(I5cpmTotEverything, file = "Final Final/Abundance/Cells/Top5/I5cpmTotEverything.Rdata")
write_xlsx(I5cpmTotEverything, "Final Final/Abundance/Cells/Top5/I5cpmTotEverything.xlsx")

### Calculate proportions
### Calculate the proportion of the total cpm that each top5+other taxa group
##  contributed to total cpm per event
I5cpmProp <- I5cpmTotEverything %>% 
  mutate(IcpmProp = totCpmTxEv/cpmTotallTxperEv)
### Test IcpmProp
testI5cpmProp <- I5cpmProp %>% 
  filter(samp_ev =="YBP2")
### summed the prop column, it checks out, sums to 1
save(I5cpmProp, file = "Final Final/Abundance/Cells/I5cpmProp.Rdata")
write_xlsx(I5cpmProp, "Final Final/Abundance/Cells/I5cpmProp.xlsx")
load("Final Final/Abundance/Cells/I5cpmProp.Rdata")


### Plot
source("scripts/01_function_wimGraph and Palettes.R")

plot_I5cpmProp <-ggplot(I5cpmProp, aes(fill=taxaGroup, y=IcpmProp, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values =  
                      c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", 
                        "CilLg" = "salmon3", "CilSm" = "salmon1",  
                        "FlagSm" = "#85B22C", "Other" = "peachpuff"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", 
                               "Other"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Initial Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()
plot_I5cpmProp

load("/Users/allisonadams/My files/Thesis/Microplankton/MicroplanktonAnalysis/Final Final/Abundance/Cells/IcpmProp.Rdata")
