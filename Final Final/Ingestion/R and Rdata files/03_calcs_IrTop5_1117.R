######################################################################
############# INGESTION  RATE CALCULATIONS ############################
####################  FOR TOP 5 + OTHER  #############################
######################################################################

### 8/21/23, 9/4/23
### 11/17/23 Needed to re-run the code using the corrected baseTop5.Rdata file
##  that had the correction of adding "1" to YBP1 E rep 2 CenDiaLg so CR and IR
##  wouldn't be NA. See "Thesis Project and Analysis Notes.docx" for notes on
##  why and confirmation with Wim that this is right.

library(tidyverse)
library(writexl)
load("Final Final/baseTop5.Rdata")
source("scripts/01_function_feedingRate.R")

### Data files made here:
load("Final Final/Ingestion/R and Rdata files/IrTop5RepMns.Rdata")
load("Final Final/Ingestion/R and Rdata files/IrTotPerEvent.Rdata")

### IR means with reps

### Use baseTop5.Rdata as the base file, because it has the taxaGroup column with 
##  just top 5 + other

### Select only the columns needed for IR
baseTop5IR <- baseTop5 %>% 
  select(samp_ev, exp, rep,  taxaGroup, bio_pgC_ml)

### Sum up the biomass, pgC mL^1 (Bpm) for the top 5 + other taxaGroups, for only the
## initial samples, adding all bpm for organisms that fall into those  categories, 
## such as all the small centric diatoms in a sampling event, experimental bottle. 
##  Need to use pgC mL^1 because CR are in units of mL copepod^1 d^1, so the biomass 
##  units need to correspond. If I want to plot in terms of ugC L^1, I can do the conversion then.

sumBpmI <- baseTop5IR %>% 
  filter(exp == "I") %>% 
  group_by(samp_ev, exp, rep, taxaGroup) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

sumBpm_Imn <- sumBpmI %>% 
  group_by(samp_ev, exp, taxaGroup) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Rename the samp_ev to event
names(sumBpm_Imn)
sumBpm_Imn <- sumBpm_Imn %>% 
  rename(event = samp_ev)

### Join sumBpm_Imn with the df that has clearance rates, reps, of Top 5 + other 
### CR_Reps_Top5.Rdata, from 03_calcs_CrTop5_1117.R
load("Final Final/Clearance/R and Rdata files/CR_Reps_Top5.Rdata")
sumBpm_cr_Imn <- 	left_join(CR_Reps_Top5, sumBpm_Imn, 
                            by = c("event", "taxaGroup"))

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")
IrTop5 <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(IRpgCd = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))

### Remove the exp column and the ImnBpm column
IrTop5 <- select(IrTop5, event, taxaGroup, CRmlcd, IRpgCd)

### Calculate mean IRpgCd
IrTop5 <- IrTop5 %>% 
  group_by(event, taxaGroup) %>% 
  mutate(IrMnpgCd = mean(IRpgCd)) %>% 
  ungroup

### Add a column for ingestion rate in µgC L^1
IrTop5RepMns <- IrTop5 %>% 
  mutate(IRµgCd = IRpgCd*.000001, IrMnµgCd = IrMnpgCd*.000001)
save(IrTop5RepMns, file = "Final Final/Ingestion/R and Rdata files/IrTop5RepMns.Rdata")
write_xlsx(IrTop5RepMns, "Final Final/Ingestion/Excel Files/IrTop5RepMns.xlsx")
load("Final Final/Ingestion/IrTop5RepMns.Rdata")

### Make a df that sums the IR of all taxa groups per sampling event
### First make a df that doesn't have the IR means in triplicate
IrMnsOnly <- subset(IrTop5RepMns, select = c(event, taxaGroup, IrMnpgCd, IrMnµgCd))
IrMnsOnly <- IrMnsOnly %>% distinct()
save(IrMnsOnly, file = "Final Final/Ingestion/R and Rdata files/IrMnsOnly.Rdata")  
write_xlsx(IrMnsOnly, "Final Final/Ingestion/Excel Files/IrMnsOnly.xlsx")

IrTotPerEvent <- IrMnsOnly %>% 
  group_by(event) %>%
  summarise(TotalIrpg = sum(IrMnpgCd),
            TotalIRug=sum(IrMnµgCd),
            .groups = 'drop') %>% 
  as.data.frame()
save(IrTotPerEvent, file = "Final Final/Ingestion/R and Rdata files/IrTotPerEvent.Rdata")  
write_xlsx(IrTotPerEvent, "Final Final/Ingestion/Excel Files/IrTotPerEvent.xlsx")

