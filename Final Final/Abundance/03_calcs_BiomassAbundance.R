############################################################################
#################### ABUNDANCE in terms of BIOMASS   #####################
############################################################################

### 7/28/23 Used 03_calcs_Abun_Other.R in scripts folder as a template for this
##  Need to recalculate abundance in terms of biomass, not cell counts.
##  Use the top 5 plus other taxa group data. Save all in new folder, "Final Final"

### 7/24/23 updated files to include the YBP1 centric diatom large exp rep 2 count to 1
##  and created a new data folder data7_24 because I lost the data folder when I made a
##  Git Hub repository.

### Accuracy checked all the scripts 06/23
### Several Calculations for looking at IR, CR, abundance, etc., to be able
##  to whittle down groups for final analysis
library(tidyverse)
library(writexl)

### Use the df that has the top 5 taxa, plus "other"
load("data7_24/FinalAnalysis/baseTop5.Rdata")

### 4/25/23 Make a df that has CR, IRbio, mn pgC per cell and abundance, cpm initials

### 4/21/23
##  Abundance, cells per mL, by sampling events and taxa groups. Use
##  initial samples cpm. First sum the cpm of the groups that make up the
##  group_size groups.
### Reminder that Small  is esd < 15µm, and Large is esd >= 15 µm


### 8/1/23 I lost the work I did yesterday, and thought this way below
##  might be better, but I'm not sure. I'm taking a break from it now
######################### INITIAL SAMPLES ############################
### Filter for the intial samples only
abundanceI <- baseTop5 %>% 
  filter(exp == "I")
### take the mean of all the biomass in pgC per ml, by event and taxaGroup, top 5 and other
AImnAgg5 <- aggregate(bio_pgC_ml ~ samp_ev + taxaGroup, 
                     data = abundanceI, mean)
### Add a column of biomass in ugC per Liter
AImnAgg5 <- AImnAgg5 %>% 
  rename(mnBioPgMl = bio_pgC_ml) %>% 
  mutate(mnBioUgL = mnBioPgMl*.001)
save(AImnAgg5, file = "Final Final/Abundance/AImnAgg5.Rdata")
### do same as above but with the 17 taxa groups, group_size
AImnAgg17 <- aggregate(bio_pgC_ml ~ samp_ev + group_size, 
                      data = abundanceI, mean)
AImnAgg17 <- AImnAgg17 %>% 
  rename(mnBioPgMl = bio_pgC_ml) %>% 
  mutate(mnBioUgL = mnBioPgMl*.001)
save(AImnAgg17, file = "Final Final/Abundance/AImnAgg17.Rdata")


### sum up the counts per ml by event and taxaGroup-- don't need to do this right now
#AItotCpmAgg <- aggregate(cpm ~ samp_ev + taxaGroup, 
                       #  data = abundanceI, sum)
### Join the two data sets together and rename samp_ev and mean biomass
#AIbioMnCpm <- left_join(AImnAgg, AItotCpmAgg) %>% 
#  rename(mnBioPgMl=bio_pgC_ml) %>% 
#  rename(event = samp_ev)
### Add a column of biomass in ugC per Liter
#AIbioMnCpm <- AIbioMnCpm %>% 
#  mutate(mnBioUgL = mnBioPgMl*.001)

### This above worked, but need to take the means of the replicates,
##  not all the biomass data
### Filter for the intial samples only
#abundanceI <- baseTop5 %>% 
#  filter(exp == "I")
### take the mean of all the biomass in pgC per ml, by event and taxaGroup
#AImnRepsAgg <- aggregate(bio_pgC_ml ~ samp_ev + szesd, 
#                     data = abundanceI, mean)
### sum up the counts per ml by event and taxaGroup
#AItotCpmAgg <- aggregate(cpm ~ samp_ev + taxaGroup, 
#                         data = abundanceI, sum)
### Join the two data sets together and rename samp_ev and mean biomass
#AIbioMnCpm <- left_join(AImnRepsAgg, AItotCpmAgg) %>% 
#  rename(mnBioPgMl=bio_pgC_ml) %>% 
#  rename(event = samp_ev)
### Add a column of biomass in ugC per Liter
#AIbioMnCpm <- AIbioMnCpm %>% 
 # mutate(mnBioUgL = mnBioPgMl*.001)




###Calculate the means of the replicates of biomass pgC mL-1 per entry 
##  (per organism + dimensions), then sum those replicate means. Add a column
##  for biomass in µgC per liter, and a column of total cell counts
RepMnsTestAbun <-	abundanceI %>% 
  group_by(samp_ev, szesd) %>% 
  mutate(mnBpmITxEv = mean(bio_pgC_ml),
         mnBulITxEv = mean(bio_ugC_l)) %>% 
  ungroup 
 # rename(event = samp_ev)
###Calculate the means of the replicates of biomass pgC mL-1 per entry 
##  (per organism + dimensions) with the szesd column
AImnBioTxEv2 <- aggregate(bio_pgC_ml ~ samp_ev + szesd, 
          data = abundanceI, mean)
### Take away the entries that have 0 biomass
AImnBioTxEv2 <- AImnBioTxEv2 %>% 
  filter(bio_pgC_ml != 0)
### Add the Top 5 + Other taxaGroups
AImnBioTxEv2 <- AImnBioTxEv2 %>% 
  mutate(taxaGroup = group_size)

baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "ChlLg"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "ChlSm"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "FlagLg"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "PenDiaLg"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "PenDiaSm"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "ChnDiaLg"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "UnidLg"] <- "Other"
baseTop5kept["taxaGroup"][baseTop5kept["taxaGroup"] == "UnidSm"] <- "Other"
### keep only the columns I want
AImnBioTxEv2 <- subset(AImnBioTxEv2,
                       select = c(event, taxaGroup,szesd, mnBpmITxEv, 
                                  mnBulITxEv,cpm, counts))
### Sum the cpm and counts
AIMnBioTxEvCt <-AImnBioTxEv2 %>% 
  group_by(event, taxaGroup) %>% 
  mutate(TotCpm = sum(cpm),
         TotCts = sum(counts)) %>% 
  ungroup

abunISum <- abundanceI %>% 
  group_by(samp_ev, group_size) %>% 
  summarise(TotalCpmI = sum(), # sum the counts per ml by group_size, per samp_ev
            .groups = 'drop') %>% 
  as.data.frame() %>% 
  rename(event = samp_ev)


### Abundance totals, taxa groups across all sampling events
AbunISumOverall <- abunISum %>% 
  group_by(group_size) %>% 
  summarise(TotalCpmIAll = sum(TotalCpmI), # sum the counts per ml by group_size, per samp_ev
            .groups = 'drop') %>% 
  as.data.frame()
save(AbunISumOverall, file = "data7_24/Abundance/AbunISumOverall.Rdata")
write_xlsx(AbunISumOverall, "data7_24/Abundance/AbunISumOverall.xlsx")
load("data7_24/Abundance/AbEventsOnly.Rdata")

### Abundance totals, events only
AbTotalsEventsOnly <- abundance %>% 
  group_by(samp_ev) %>% 
  summarise(TotalCpmI = sum(TotalCpmI),
            .groups = 'drop') %>% 
  as.data.frame()
save(AbTotalsEventsOnly, file = "data7_24/Abundance/AbTotalsEventsOnly.Rdata")
write_xlsx(AbTotalsEventsOnly, "data7_24/Abundance/AbTotalsEventsOnly.xlsx")

### Mean pg C per cell by group_size
PgCell <- volbio_all_cr %>% 
  group_by(group_size) %>% 
  summarise(pgCcellMn = mean(biomass_cell_pgC))

### Join these with the CR and IR df
load( "data7_24/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

CrIrAb <- left_join(CR_IRbio_mn, abunISum, 
                    by = c("event", "group_size"))
CrIrAbCell <- left_join(CrIrAb, PgCell,
                        by = "group_size")
save(CrIrAbCell, file = "data7_24/FinalAnalysis/CrIrAbCell.Rdata")
write_xlsx(CrIrAbCell, "data7_24/FinalAnalysis/CrIrAbCell.xlsx")
load("data7_24/FinalAnalysis/CrIrAbCell.Rdata")

### Calculate total biomass IR per day for each sampling event by adding up
##  the biomass IRs of all the group_sizes. This will tell us how much in total
##  the copepods were eating per day at that site

IRtotByEvent <- CrIrAb %>% 
  group_by(event) %>% 
  summarise(TotIRbio = sum(FRmnUgCcd[FRmnUgCcd > 0]))
save(IRtotByEvent, file = "data7_24/FinalAnalysis/IRtotByEvent.Rdata")
### Add this to CrIrAbCell
CrIrTotAbCell <- left_join(CrIrAbCell, IRtotByEvent,
                           by = c("event"))
save(CrIrAbCell, file = "data7_24/FinalAnalysis/CrIrTotAbCell.Rdata")
write_xlsx(CrIrAbCell, "data7_24/FinalAnalysis/CrIrTotAbCell.xlsx")

### See what prey items were > 200µm
over200 <- volbio_all_cr %>% 
  filter(la >200 & counts >= 1)
save(over200, file = "data7_24/Abundance/over200.Rdata")
write_xlsx(over200, "data7_24/Abundance/over200.xlsx")

### What is the median esd per taxa group?
medEsd <- volbio_all_cr %>% 
  group_by(group_size) %>% 
  summarise(medEsd = median(esd)) 
min(volbio_all_cr$esd) # = 3.1
max(volbio_all_cr$esd) # = 160

### Use baseTop5.Rdata as the base, for top 5 taxa groups, plus "other"
load("data7_24/FinalAnalysis/baseTop5.Rdata")

abundance5I <- baseTop5 %>% 
  filter(exp == "I")

abun5ISum <- abundance5I %>% 
  group_by(samp_ev, taxaGroup) %>% 
  summarise(TotalCpmI = sum(cpm), # sum the counts per ml by group_size
            .groups = 'drop') %>% 
  as.data.frame() %>% 
  rename(event = samp_ev)
save(abun5ISum, file = "data7_24/Abundance/abun5ISum.Rdata")
write_xlsx(abun5ISum, "data7_24/Abundance/abundance5.xlsx")
load("data7_24/Abundance/abundance5.Rdata")

### Abundance totals across all sampling events of the top 5 taxa groups plus other
abun5TaxaTotals <- abun5ISum %>% 
  group_by(taxaGroup) %>% 
  summarise(TotalCpmI= sum(TotalCpmI),
            .groups ='drop') %>% 
  as.data.frame()
save(abun5TaxaTotals, file = "data7_24/Abundance/abun5TaxaTotals.Rdata")
write_xlsx(abun5TaxaTotals, "data7_24/Abundance/abun5TaxaTotals.xlsx")

### Mean pg C per cell by group_size
PgCell5 <- baseTop5 %>% 
  group_by(taxaGroup) %>% 
  summarise(pgCcellMn = mean(biomass_cell_pgC))

### Join these with the CR and IR df
load( "data7_24/Clearance Rates/CR5_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

CrIrAb5 <- left_join(CR5_IRbio_mn, abun5ISum, 
                     by = c("event", "taxaGroup"))
CrIrAb5Cell <- left_join(CrIrAb5, PgCell5,
                         by = "taxaGroup")
save(CrIrAb5Cell, file = "data7_24/FinalAnalysis/CrIrAb5Cell.Rdata")
write_xlsx(CrIrAb5Cell, "data7_24/FinalAnalysis/CrIrAb5Cell.xlsx")
load("data7_24/FinalAnalysis/CrIrAb5Cell.Rdata")


###### ###### ###### ###### 
##  Abundance, all events combined, with taxa groups ordered
##  in large and small grouping
###### ###### ###### ###### 
load("data7_24/Abundance/abundance.Rdata")
abundance_Size <- abundance %>% 
  mutate(taxaGroup = factor(group_size,
                            levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                     "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                     "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                     "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(abundance_Size, file = "data7_24/Abundance/abundance_Size.Rdata")
write_xlsx(abundance_Size, "data7_24/Abundance/abundance_Size.xlsx")
load("data7_24/Abundance/abundance_Size.Rdata")


### Need to take the mean of the abundance per taxon across all events
## Not sure why I need the mean abundance????
ABmean_allEvents <- abundance_Size %>% 
  group_by(taxaGroup) %>% 
  summarise(TotalCpmI = mean(TotalCpmI)) %>% 
  ungroup()
save(ABmean_allEvents, file = "data7_24/Abundance/ABmean_allEvents.Rdata")
write_xlsx(ABmean_allEvents, "data7_24/Abundance/ABmean_allEvents.xlsx")
### See 04_plots_IR_Various.R for plotting it.

### Abundance totals,means by events only
AbMnEventsOnly <- abundance %>% 
  group_by(samp_ev) %>% 
  summarise(TotalCpmI = mean(TotalCpmI),
            .groups = 'drop') %>% 
  as.data.frame()
save(AbMnEventsOnly, file = "data7_24/Abundance/AbMnEventsOnly.Rdata")
write_xlsx(AbMnEventsOnly, "data7_24/Abundance/AbMnEventsOnly.xlsx")
load("data7_24/Abundance/AbMnEventsOnly.Rdata")

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
write_xlsx(abun_NA_sums, "data7_24/Abundance/abun_NA_sums.xlsx")

## Sum the total counts for ALL group_sizes
abun_totCt_All <- abundanceI %>% 
  select(samp_ev, group_size, counts)
abun_totCt_All<-  abun_totCt_All %>% 
  group_by(group_size) %>% 
  summarise(totCt = sum(counts),
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(abun_totCt_All, "data7_24/Abundance/abun_totCt_All.xlsx")



