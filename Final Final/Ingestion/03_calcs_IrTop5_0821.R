######################################################################
############# INGESTION  RATE CALCULATIONS ############################
####################  FOR TOP 5 + OTHER  #############################
######################################################################

### 8/21/23
library(tidyverse)
library(writexl)
load("data7_24/FinalAnalysis/baseTop5.Rdata")
source("scripts/01_function_feedingRate.R")

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

### Join sumBpm_Imn with the df that has clearance rates
load("Final Final/Clearance/CrMnTop5.Rdata")
sumBpm_cr_Imn <- 	left_join(CrMnTop5, sumBpm_Imn, 
                            by = c("event", "taxaGroup"))


### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")
IrTop5 <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(IRpgCd = fr_func(CR=CrMNmlcd, initialMnCt = ImnBpm))

### Remove the exp column and the ImnBpm column
IrTop5 <- select(IrTop5, event, taxaGroup, CrMNmlcd, IRpgCd)

### Add a column for ingestion rate in µgC L^1
IrTop5 <- IrTop5 %>% 
  mutate(IRµgCd = IRpgCd*.000001)

save(IrTop5, file = "Final Final/Ingestion/IrTop5.Rdata")
write_xlsx(IrTop5, "Final Final/Ingestion/IrTop5.xlsx")
load("Final Final/Ingestion/IrTop5.Rdata")
