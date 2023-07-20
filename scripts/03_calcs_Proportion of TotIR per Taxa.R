#########################################################################################
############ PROPORTION OF TOTAL BIOMASS IR CONTRIBUTED PER TAXA GROUP ##################
#########################################################################################

###########################################################################
### Total Biomass ingestion rate for all taxa groups together, and also for only 
### the taxa groups I kept after excluding some for low total counts or low or 
### neg CR IR
###########################################################################

library(tidyverse)
library(writexl)
load("data/Clearance Rates 2/CrIrCntRepMnTots.Rdata") # accuracy check done
### this df above contains the clearance rate reps and means; biomass ingestion rate reps and means;
##  in pgC and µgC, counts per ml per row of event, taxa rep; total counts, same as previous; 
##  total counts of taxa across all events; mean clearance rate of each taxa group across all events,
##  calculated from taking the mean of all the individual reps, and also taking the mean of the means 
##  of the reps per event
### The above data frame was created/calculated in 03_calcs_CR_IR_New_06_03.R

### First remove the columns I don't need from CrIrCntRepMnTots: rep, cpmE, Cmn,
##CRmlcd, CrMNmlcd, FRpgCcd, FRUgCcd, TotCpm, TotCt, TotCtXallEvents, MnCrXbyReps, MnCrXbyMns
IrMns <- subset(CrIrCntRepMnTots, select = c(event, group_size, FRUgMn))
### Remove the duplicates that are there as ghosts of the three replicates
duplicated(IrMns)
IrMns <- IrMns %>% distinct()
write_xlsx(IrMns, "data/Clearance Rates 2/IrMns.xlsx")

### Calculate the total biomass FR Ug per copepod per day, X all events and taxa
IrTot <- IrMns
# note_ this is how Wim said to do it: sum(FRUgMn[FRUgMn >= 0 ]) 
IrTotAll <- IrTot %>% 
  mutate(IrTotAllUgC = sum(FRUgMn[FRUgMn >= 0 ], na.rm = TRUE))
save(IrTotAll, file = "data/Clearance Rates 2/IrTotAll.Rdata")
write_xlsx(IrMnsTots, "data/Clearance Rates 2/IrTotAll.xlsx")
load("data/Clearance Rates 2/IrTotAll.Rdata")

### Remove the taxa groups that I'm excluding from reporting, per 6/14 notes: 
##   ChnDiaSm, DinoLg, CyanoLg, CyanoSm
IrTotAllTaxaKept <- IrTotAll %>% 
  filter(group_size %in% c("CenDiaLg", "CenDiaSm", "ChlLg", "ChlSm", "ChnDiaLg", "CilLg", "CilSm",  
                           "FlagLg",  "FlagSm", "PenDiaLg", "PenDiaSm", "UnidLg", "UnidSm" ))
### Calculate the proportion of the total FR UgC that each group_size contributed to total
IrTotAllTaxaKeptProp <- IrTotAllTaxaKept %>% 
  mutate(PropIRbioUgC = FRUgMn/IrTotAllUgC)
### But need to not include the negative numbers in the division calc.
IrTotAllTaxaKeptProp <- IrTotAllTaxaKept %>% 
  mutate(PropIRbioUgC =  ifelse(FRUgMn>=0, FRUgMn/IrTotAllUgC, NA))
sum(IrTotAllTaxaKeptProp$PropIRbioUgC, na.rm = TRUE)

### Also calculate total IR bio for each sampling event
IrTotAllTaxaKeptProp <- IrTotAllTaxaKeptProp %>% 
  group_by(event) %>% 
  mutate(IrTotUgCEvent = sum(FRUgMn[FRUgMn >= 0 ], na.rm = TRUE)) %>% 
  ungroup
### And the proportion of IR for each taxa per event, not per all IRbio
IrTotAllTaxaKeptProp <- IrTotAllTaxaKeptProp %>% 
  group_by(event) %>% 
  mutate(PropIRrBuTaxaPerEvent =  ifelse(FRUgMn>=0, FRUgMn/IrTotUgCEvent, NA)) %>% 
  ungroup

### Add the proportion of total IR bio that each event made up
IrTotAllTaxaKeptProp <- IrTotAllTaxaKeptProp %>% 
  group_by(event) %>% 
  mutate(PropIrBuEvent =  ifelse(IrTotUgCEvent>=0, IrTotUgCEvent/IrTotAllUgC, NA)) %>%
  ungroup

### Add the proportion of total IR bio that each taxa group made up
##  First sum the total IRbio of each taxa group
IrTotAllTaxaKeptProp <- IrTotAllTaxaKeptProp %>% 
  group_by(group_size) %>% 
  mutate(IrTotUgCTaxa = sum(FRUgMn[FRUgMn >= 0 ], na.rm = TRUE)) %>% 
  ungroup
### Then calculate the proportion
IrTotAllTaxaKeptProp <- IrTotAllTaxaKeptProp %>% 
  mutate(PropIrBuTaxaTot =  ifelse(IrTotUgCTaxa>=0, IrTotUgCTaxa/IrTotAllUgC, NA)) %>%
  ungroup

save(IrTotAllTaxaKeptProp, file = "data/Clearance Rates 2/IrTotAllTaxaKeptProp.Rdata")
write_xlsx(IrTotAllTaxaKeptProp, "data/Clearance Rates 2/IrTotAllTaxaKeptProp.xlsx")

### Drop rows/taxa with NA IR
IrTaxaPropNoNas <- IrTotAllTaxaKeptProp %>%
  drop_na(PropIRbioUgC)
sum(IrTaxaPropNoNas$PropIRbioUgC, na.rm = TRUE)
save(IrTaxaPropNoNas, file = "data/Clearance Rates 2/IrTaxaPropNoNas.Rdata")
write_xlsx(IrTaxaPropNoNas, "data/Clearance Rates 2/IrTaxaPropNoNas.xlsx")

### Plot the above, but maybe I need to plot with all the taxa, so that they all
##   appear on all event bars?
load("data/Clearance Rates 2/IrTotAllTaxaKeptProp.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")
xPalette <- wimPalettes("P")
ggplot(IrTotAllTaxaKeptProp, aes(x = event, y = group_size, fill = PropIRbioUgC)) +
  geom_col()
### NO , see new plot from fiverr: scripts/04_plots_IrBioRelAbunRainbow.R
p <- ggplot(IrTotAllTaxaKeptProp, aes(x = event, y = group_size, fill = PropIRbioUgC)) +
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_manual(values=xPalette)+
  scale_y_continuous(limits = c(0, .05))+
  xlab("Sampling Event")+ 
  ylab("Taxa Group Relative Abundance")+
  wimGraph()
p

### Proportion of total IRbio that each event made up
IrBioPropEvents <- IrTotAllTaxaKeptProp %>% 
  group_by(event) %>% 
  mutate()






