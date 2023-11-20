######################################################################
############# INGESTION  RATE CALCULATIONS ############################
####################  FOR TOP 5 + OTHER  #############################
######################################################################

### 8/21/23, 9/4/23
library(tidyverse)
library(writexl)
load("data7_24/FinalAnalysis/baseTop5.Rdata")
source("scripts/01_function_feedingRate.R")

### Data files made here:
### IrTop5RepMns.Rdata, ingestion rates of top 5 taxa groups + other, reps with means
### Not found here, but made somewhere: IrTop5Mns.Rdata, ingestion rates top 5 taxa
##   groups + other, means of taxa groups, no reps.

### IR means with reps

### Use baseTop5.Rdata as the base file, because it has the taxaGroup column with just top 5 + other
### Select only the columns needed for CR
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


### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_Imn)
sumBpm_Imn <- sumBpm_Imn %>% 
  rename(event = samp_ev)

### Join sumBpm_Imn with the df that has clearance rates, reps, of Top 5 + other 
### CR_Rep_Mn_Top5.Rdata, from 03_calcs_CrTi=op5_0821.R
load("Final Final/Clearance/CR_Rep_Mn_Top5.Rdata")
### In CR_Rep_Mn, change group_size column name to taxaGroup

sumBpm_cr_Imn <- 	left_join(CR_Rep_Mn_Top5, sumBpm_Imn, 
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

save(IrTop5RepMns, file = "Final Final/Ingestion/IrTop5RepMns.Rdata")
write_xlsx(IrTop5RepMns, "Final Final/Ingestion/IrTop5RepMns.xlsx")
load("Final Final/Ingestion/IrTop5RepMns.Rdata")

### Only ingestion rate values greater than 0
IrTop5RepMns2 <- IrTop5RepMns %>% 
  select(c(event, taxaGroup, IRµgCd, IrMnµgCd))
IrTop5RepMns2 <- subset(IrTop5RepMns2, IRµgCd>0 & IrMnµgCd>0)
save(IrTop5RepMns2, file = "Final Final/Ingestion/IrTop5RepMns2.Rdata")
write_xlsx(IrTop5RepMns2, "Final Final/Ingestion/IrTop5RepMns2.xlsx")

### Make a df that sums the IR of all taxa groups per sampling event
IrTotPerEvent <- IrTop5 %>% 
  group_by(event) %>%
  summarise(TotalIrpg = sum(IRpgCd),
            TotalIRug=sum(IRµgCd),
            .groups = 'drop') %>% 
  as.data.frame()
save(IrTotPerEvent, file = "Final Final/Ingestion/R data/IrTotPerEvent.Rdata")  
write_xlsx(IrTotPerEvent, "Final Final/Ingestion/Excel Files/IrTotPerEvent.xlsx")

