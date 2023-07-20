############################################################################
################### CALCULATE INGESTION RATE PER REP  ######################
############################################################################

### 5/26/23 Using 03_calcs_CR_FR.R as the base for this script
##  The purpose is to calculate ingestion rates per replicate, for each sampling 
##  event, so that I have an IR for each replicate, 1, 2, and 3. 
##  Then calculate the mean from there.
##  Wim said to do this in our meeting on 5/24/23

### Notes:
##  CR = V/T * (ln(mean control samples) - ln(experimental samples))/n
##  Clearance rate = (Volume of experimental container--595 ml)/ Time of experiment *
##  (ln of the mean of the control samples cpm) - (ln of the experimental samples cpm)/ 
##  the number of copepods in the experimental bottle. 
##  Units of the result are mL per copepod per day

### Then calculate the initial samples cpm or biomass to be used to calculate ingestion 
##  feeding) rate, I = F x mean IC, Ingestion rate = Clearance rate  * 
##  the mean initial samples (counts or biomass)
##  Then calculate the Ingestion Rate
### Ingestion Rates units are in cells per copepod per day, or pgC  or µg C biomass per copepod per day,
##  I need to also calculate the median clearance rate for putting into the
##  feeding rate equation.


library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
source("scripts/01_function_feedingRate.R")

### ____________  CREATE THE BASE DATA FRAME ___________________

baseRep <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, group_size, cpm, bio_pgC_ml)
#save(base, file = "data/Clearance Rates/base.Rdata")

###### _________________CLEARANCE RATE _________________________###
##################################################################

### Sum up the cpm for all the sampling events, individual reps 1, 2 and 3
sumCpmRep <- baseRep %>% 
  group_by(samp_ev, exp, rep) %>% 
  summarise(TotalCpm=sum(cpm),
            .groups = 'drop') %>% 
  as.data.frame()

## Create the base data frame that has only the control and experimental cpm
sumCpmRep_CE <- sumCpmRep %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
sumCpmRep_C <- sumCpmRep_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = TotalCpm)

### Create a df with only experimental samples
sumCpmRep_E <- sumCpmRep_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = TotalCpm) 

### Create a df with only initial samples (for the ingestion rates)
sumCpmRep_I <- sumCpmRep%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm) 

### Take the mean of the control cpm
sumCpmRep_Cmn <- sumCpmRep_C %>% 
  group_by(samp_ev) %>% 
  summarise(CmnCpm=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Join the control means to the experimental sums 
sumCpmRepEC <- 	left_join(sumCpmRep_E, sumCpmRep_Cmn, 
                          by = c("samp_ev"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumCpmRepEC)
sumCpmRepECmn <- select(sumCpmRepEC, 
                      event = samp_ev,
                      rep, cpmE, CmnCpm)

### Calculate clearance rates. The resulting data frame includes all three replicates
## for each sampling event, since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sumCpmRep_cr <- rowwise(sumCpmRepECmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = CmnCpm, expCt = cpmE))
write_xlsx(sumCpmRep_cr, "data/Clearance Rates/sumCpmRep_cr.xlsx")
save(sumCpmRep_cr, file = "data/Clearance Rates/sumCpmRep_cr.Rdata")

### Take the mean CR for each sampling_event
CRmn_Events <- sumCpmRep_cr %>% 
  group_by(event) %>% 
  summarise(MeanCR = mean(CRmlcd),
            .groups = 'drop') %>% 
  as.data.frame()
save(CRmn_Events, file = "data/Clearance Rates/By Replicates/CRmn_Events.Rdata")
write_xlsx(CRmn_Events,"data/Clearance Rates/By Replicates/CRmn_Events.xlsx")

### As a note of comparison, this is from 03_calcs_CR_FR.R, which is CR mean,
## events only, calculated as the means of the taxa group sums
load("data/Clearance Rates/sumCpm_CRmn.Rdata") # Clearance Rates means only
sumCRmnEventsOnly <- sumCpm_CRmn %>% 
  group_by(event) %>% 
  summarise(MeanCR = mean(CrMNmlcd),
            .groups = 'drop') %>% 
  as.data.frame()

################################################################################
### _________INGESTION (FEEDING) RATE BY BIOMASS ______________________________
################################################################################

## Create the base data frame that has only the biomass 
sumBpmRep <- baseRep %>% 
  group_by(samp_ev, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sumBpmRep_I <- sumBpmRep %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep,TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumBpmRep_Imn <- sumBpmRep_I %>% 
  group_by(samp_ev) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpmRep_Imn)
sumBpmRep_Imn <- select(sumBpmRep_Imn, 
                     event = samp_ev,ImnBpm)

### Join sumBpm_Imn with the df that has clearance rates, sumCpm_cr
#load("data/Clearance Rates/sumCpmRep_cr.Rdata")
sumBpmRep_cr_Imn <- 	left_join(sumCpmRep_cr, sumBpmRep_Imn, 
                            by = c("event"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpmRep_cr_Imn)
sumBpmRep_cr_Imn <- select(sumBpmRep_cr_Imn, 
                        event, rep, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumBpmRep_FR <- rowwise(sumBpmRep_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sumBpmRep_FR <- sumBpmRep_FR %>% 
  rename("FRpgCcd" = "FR")

## Add a column with the conversion from pg to µg
BugRep_FR <- sumBpmRep_FR %>%
  group_by(event, rep, ImnBpm, CRmlcd,FRpgCcd) %>%
  summarise(FRUgCcd = FRpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 

### Calculate the means across the sampling events, for FR in pgC
BpmFRMn_Rep <- sumBpmRep_FR %>%
  group_by(event) %>%
  summarise(FRmnpgCcd = mean(FRpgCcd),
            .groups = 'drop') %>% 
  as.data.frame()

### Calculate the means across the sampling events for FR in µgC
BugRep_FRmn <- BugRep_FR %>%
  group_by(event) %>%
  summarise(FRmnUgCcd = mean(FRUgCcd),
            .groups = 'drop') %>% 
  as.data.frame()

### Combine BpmFRmn_Rep with BugRep_FR so the FR means are in the same df
##  for my convenience
FrCrBio_Rep <- left_join(BugRep_FR, BpmFRMn_Rep, 
                         by = c("event"))
### Combine the above with BugRep_FRmn so the mean FR in µgC are there too
FrCrBioRep_All <- left_join(FrCrBio_Rep, BugRep_FRmn,
                         by = "event")

### Add the mean CR to the above df
FrCrBioRep_All <- left_join(FrCrBioRep_All, CRmn_Events,
                           by = "event")
### Reorder the columns
#FrCrBio_RepMn <- FrCrBio_RepMn %>% 
col_order <- c("event", "rep", "ImnBpm", "CRmlcd","MeanCR", "FRpgCcd",
               "FRUgCcd", "FRmnpgCcd", "FRmnUgCcd")
FrCrBioRep_All <- FrCrBioRep_All[, col_order]
write_xlsx(FrCrBioRep_All, "data/Clearance Rates/Feeding Rates/FrCrBioRep_All.xlsx")

