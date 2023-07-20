############################################################################
#################### ABUNDANCE and pgC per cell Mean   #####################
############################################################################

library(tidyverse)
library(writexl)
### Accuracy checked all the scripts 06/23
### Several Calculations for looking at IR, CR, abundance, etc., to be able
##  to whittle down groups for final analysis

### Also, use the df that has the top 5 taxa, plus "other"
load("data/FinalAnalysis/baseTop5.Rdata")
### 4/25/23 Make a df that has CR, IRbio, mn pgC per cell and abundance, cpm initials

### 4/21/23
##  Abundance, cells per mL, by sampling events and taxa groups. Use
##  initial samples cpm. First sum the cpm of the groups that make up the
##  group_size groups.
### Reminder that Small  is esd < 15µm, and Large is esd >= 15 µm

### Use volbio_all_cr.Rdata as the base

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")

abundanceI <- volbio_all_cr %>% 
  filter(exp == "I")
save(abundanceI, file = "data/Abundance/abundanceI.Rdata")
abunISum <- abundanceI %>% 
  group_by(samp_date, samp_ev, group_size) %>% 
  summarise(TotalCpmI = sum(cpm), # sum the counts per ml by group_size, per samp_ev
            .groups = 'drop') %>% 
  as.data.frame() %>% 
  rename(event = samp_ev)


### Abundance totals, taxa groups across all sampling events
AbunISumOverall <- abunISum %>% 
  group_by(group_size) %>% 
  summarise(TotalCpmIAll = sum(TotalCpmI), # sum the counts per ml by group_size, per samp_ev
            .groups = 'drop') %>% 
  as.data.frame()
save(AbunISumOverall, file = "data/Abundance/AbunISumOverall.Rdata")
write_xlsx(AbunISumOverall, "data/Abundance/AbunISumOverall.xlsx")
load("data/Abundance/AbEventsOnly.Rdata")

### Abundance totals, events only
AbTotalsEventsOnly <- abundance %>% 
  group_by(samp_ev) %>% 
  summarise(TotalCpmI = sum(TotalCpmI),
            .groups = 'drop') %>% 
  as.data.frame()
save(AbTotalsEventsOnly, file = "data/Abundance/AbTotalsEventsOnly.Rdata")
write_xlsx(AbTotalsEventsOnly, "data/Abundance/AbTotalsEventsOnly.xlsx")

### Mean pg C per cell by group_size
PgCell <- volbio_all_cr %>% 
  group_by(group_size) %>% 
  summarise(pgCcellMn = mean(biomass_cell_pgC))

### Join these with the CR and IR df
load( "data/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

CrIrAb <- left_join(CR_IRbio_mn, abunISum, 
                    by = c("event", "group_size"))
CrIrAbCell <- left_join(CrIrAb, PgCell,
                        by = "group_size")
save(CrIrAbCell, file = "data/FinalAnalysis/CrIrAbCell.Rdata")
write_xlsx(CrIrAbCell, "data/FinalAnalysis/CrIrAbCell.xlsx")
load("data/FinalAnalysis/CrIrAbCell.Rdata")

### Calculate total biomass IR per day for each sampling event by adding up
##  the biomass IRs of all the group_sizes. This will tell us how much in total
##  the copepods were eating per day at that site

IRtotByEvent <- CrIrAb %>% 
  group_by(event) %>% 
  summarise(TotIRbio = sum(FRmnUgCcd[FRmnUgCcd > 0]))
save(IRtotByEvent, file = "data/FinalAnalysis/IRtotByEvent.Rdata")
### Add this to CrIrAbCell
CrIrTotAbCell <- left_join(CrIrAbCell, IRtotByEvent,
                           by = c("event"))
save(CrIrAbCell, file = "data/FinalAnalysis/CrIrTotAbCell.Rdata")
write_xlsx(CrIrAbCell, "data/FinalAnalysis/CrIrTotAbCell.xlsx")

### See what prey items were > 200µm
over200 <- volbio_all_cr %>% 
  filter(la >200 & counts >= 1)
save(over200, file = "data/Abundance/over200.Rdata")
write_xlsx(over200, "data/Abundance/over200.xlsx")

### What is the median esd per taxa group?
medEsd <- volbio_all_cr %>% 
  group_by(group_size) %>% 
  summarise(medEsd = median(esd)) 
min(volbio_all_cr$esd) # = 3.1
max(volbio_all_cr$esd) # = 160

### Use baseTop5.Rdata as the base, for top 5 taxa groups, plus "other"
load("data/FinalAnalysis/baseTop5.Rdata")

abundance5I <- baseTop5 %>% 
  filter(exp == "I")

abun5ISum <- abundance5I %>% 
  group_by(samp_ev, taxaGroup) %>% 
  summarise(TotalCpmI = sum(cpm), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame() %>% 
  rename(event = samp_ev)
save(abun5ISum, file = "data/Abundance/abun5ISum.Rdata")
write_xlsx(abun5ISum, "data/Abundance/abundance5.xlsx")
load("data/Abundance/abundance5.Rdata")

### Abundance totals across all sampling events of the top 5 taxa groups plus other
abun5TaxaTotals <- abun5ISum %>% 
  group_by(taxaGroup) %>% 
  summarise(TotalCpmI= sum(TotalCpmI),
            .groups ='drop') %>% 
  as.data.frame()
save(abun5TaxaTotals, file = "data/Abundance/abun5TaxaTotals.Rdata")
write_xlsx(abun5TaxaTotals, "data/Abundance/abun5TaxaTotals.xlsx")

### Mean pg C per cell by group_size
PgCell5 <- baseTop5 %>% 
  group_by(taxaGroup) %>% 
  summarise(pgCcellMn = mean(biomass_cell_pgC))

### Join these with the CR and IR df
load( "data/Clearance Rates/CR5_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

CrIrAb5 <- left_join(CR5_IRbio_mn, abun5ISum, 
                    by = c("event", "taxaGroup"))
CrIrAb5Cell <- left_join(CrIrAb5, PgCell5,
                        by = "taxaGroup")
save(CrIrAb5Cell, file = "data/FinalAnalysis/CrIrAb5Cell.Rdata")
write_xlsx(CrIrAb5Cell, "data/FinalAnalysis/CrIrAb5Cell.xlsx")
load("data/FinalAnalysis/CrIrAb5Cell.Rdata")


###### ###### ###### ###### 
##  Abundance, all events combined, with taxa groups ordered
##  in large and small grouping
###### ###### ###### ###### 
load("data/Abundance/abundance.Rdata")
abundance_Size <- abundance %>% 
  mutate(taxaGroup = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(abundance_Size, file = "data/Abundance/abundance_Size.Rdata")
write_xlsx(abundance_Size, "data/Abundance/abundance_Size.xlsx")
load("data/Abundance/abundance_Size.Rdata")


### Need to take the mean of the abundance per taxon across all events
## Not sure why I need the mean abundance????
ABmean_allEvents <- abundance_Size %>% 
  group_by(taxaGroup) %>% 
  summarise(TotalCpmI = mean(TotalCpmI)) %>% 
  ungroup()
save(ABmean_allEvents, file = "data/Abundance/ABmean_allEvents.Rdata")
write_xlsx(ABmean_allEvents, "data/Abundance/ABmean_allEvents.xlsx")
### See 04_plots_IR_Various.R for plotting it.

### Abundance totals,means by events only
AbMnEventsOnly <- abundance %>% 
  group_by(samp_ev) %>% 
  summarise(TotalCpmI = mean(TotalCpmI),
            .groups = 'drop') %>% 
  as.data.frame()
save(AbMnEventsOnly, file = "data/Abundance/AbMnEventsOnly.Rdata")
write_xlsx(AbMnEventsOnly, "data/Abundance/AbMnEventsOnly.xlsx")
load("data/Abundance/AbMnEventsOnly.Rdata")

### 6/8/23 Look at total counts for the taxa whose CR has NA in at least one rep
abun_NA <- abundanceI %>% 
  filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
                           "CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                           "CenDiaLg")) %>% 
  select(samp_ev, exp, rep, group_size, counts)

## Sum the total counts for each NA group_size
abun_NA_sums <- abun_NA %>% 
  group_by(group_size) %>% 
  summarise(totCt = sum(counts),
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(abun_NA_sums, "data/Abundance/abun_NA_sums.xlsx")

## Sum the total counts for ALL group_sizes
abun_totCt_All <- abundanceI %>% 
  select(samp_ev, group_size, counts)
abun_totCt_All<-  abun_totCt_All %>% 
  group_by(group_size) %>% 
  summarise(totCt = sum(counts),
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(abun_totCt_All, "data/Abundance/abun_totCt_All.xlsx")



