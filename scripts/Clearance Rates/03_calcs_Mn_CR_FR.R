############################################################################
### CONTROL MEANS CPM and BPCM, INITIAL CPM and BPCM, CR CALCS, FR CALCS###
############################################################################
### 2/6/23

### UPDATE on 2/15/23, I made another script to calculate CR and IR using
##  a df of the organisms in their 8 taxa groups, which is now the final script,
##  as fo 4/4/23.

### This script is cleaned up from 03_calcs_CtrlMean_Cr.R, and is the final script
##  for calculating the means of the control samples counts per ml (cpm) or biomass
##  and then joining that df with another df of the experimental samples cpm or biomass
##  (individual per replicate) and then using that data to calculate the clearance rates.
##  CR = V/T * (ln(mean control samples) - ln(experimental samples))/n
##  Clearance rate = (Volume of experimental container - 595 mL)/ Time of experiment *
##  (ln of the mean of the control samples) - (ln of the experimental samples)/ the number
##  of copepods in the experimental bottle

### Then calculate the initial samples cpm or biomass to be used to calculate ingestion (feeding)
##  rate, I = F x mean IC, Ingestion rate = Clearance rate (in counts or biomass) * 
##  the mean initial samples (counts or biomass)
##  Then calculate the Ingestion Rate
### Ingestion Rates units are in counts per copepod per day, or pgC biomass per copepod per day,
##  which may be converted to ugC per liter by dividing CR by 1000
##  I need to also calculate the median clearance rate for putting into the
##  feeding rate equation.
##  Units of results are ml per copepod per day

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")

###_____________________CLEARANCE RATES____________________________

## Create the base data frame that has only the controls and experimentals
## I included the volume per cell so I could easily look at it in reference
##  to the counts and cpm
cr_cpm_CE <- volbio_all_cr %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, Group, type, sa, la, wi, esd, cpm, vol_per_cell_um3, size)
### cpm= counts per milliliter

### Create another df from the above, with only the control samples
cr_cpm_C <- cr_cpm_CE %>% 
  filter(exp == "C") 

### Create a df with only experimental samples
cr_cpm_E <- cr_cpm_CE %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp,  rep, Group, type, sa, la, wi, esd, vol_per_cell_um3, size) %>% 
  ungroup

### Apply the mean function to get control mean counts per ml across
##  the three replicates
cr_cpm_Cmn <- cr_cpm_C %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, esd, vol_per_cell_um3, size) %>%
  summarize(CmnCpm = mean(cpm),
            n=length(cpm)) %>% 
  ungroup

### Join the experimental sample df with the control means df
cr_cpm_CmnE <- 	left_join(cr_cpm_E, cr_cpm_Cmn, 
                          by = c("samp_ev", "Group", "type", "sa", "la", "wi", "esd"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(cr_cpm_CmnE)
cr_cpm_CmnE <- select(cr_cpm_CmnE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = Group,
                   type, sa, la, wi, esd,
                   vpc = vol_per_cell_um3.x,
                   cpmE = cpm,
                   CmnCpm,
                   size = size.x)

### Calculate clearance rates
##  CR = clearance rate
##  Remember CR units are ml per copepod per day
source("scripts/01_function_clearanceRates.R")
cr_cpm <- rowwise(cr_cpm_CmnE) %>% 
  mutate(CR = cr_func(controlMnCt = CmnCpm, expCt = cpmE))
save(cr_cpm, file = "data/Clearance Rates/cr_cpm.Rdata")
write_xlsx(cr_cpm, "data/Clearance Rates/cr_cpm.xlsx")

###  Take the mean of the CR; don't group by "rep" because if you do, it will keep all the rep rows
##  and it won't take the mean of the CR of the three reps, but will just fill in the CR for each rep
##  Remember CR units are ml per copepod per day
crmn_cpm <- cr_cpm %>% 
  group_by(event, sample, group, type, sa, la, wi, esd,
           vpc, size) %>% 
  summarize(crMn = mean(CR))
crmn_cpm <- crmn_cpm %>% 
  drop_na(crMn)
save(crmn_cpm, file = "data/Clearance Rates/crmn_cpm.Rdata")

###_____________________INGESTION RATES FOR COUNTS (CELLS) PER ML____________________________

### Prepare dfs for calculating ingestion rate (or feeding rate, fr)
## Create the base data frame that has only the initial samples
fr_cpm_I <- volbio_all_cr %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, type, sa, la, wi, esd, cpm, vol_per_cell_um3, size)

### Apply the mean function to the initial mean counts per ml across
## the three replicates
fr_cpm_Imn <- fr_cpm_I %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, esd, vol_per_cell_um3, size) %>%
  summarize(ImnCpm = mean(cpm),
            n=length(cpm)) %>% 
  ungroup

### Remove unneeded columns, and rename and re-order remaining columns
names(fr_cpm_Imn)
fr_cpm_Imn <- select(fr_cpm_Imn, 
                    event = samp_ev,
                    sample = exp,
                    group = Group,
                    type, sa, la, wi, esd,
                    vpm = vol_per_cell_um3,
                    ImnCpm,
                    size = size)

### Join fr_cpm_Imn with the df that has clearance rates, cr_cpm
load("data/Clearance Rates/cr_cpm.Rdata")

fr_cr_cpm <- 	left_join(cr_cpm, fr_cpm_Imn, 
                        by = c("event", "group", "type", "sa", "la", "wi", "esd", "size"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(fr_cr_cpm)
fr_cr_cpm <- select(fr_cr_cpm, 
                    event, group,
                    type, sa, la, wi, esd,
                    size, vpm, ImnCpm, CR)

### Calculate ingestion (feeding) rate
## Units are cells per copepod per day
source("scripts/01_function_feedingRate.R")

fr_cpm <- rowwise(fr_cr_cpm) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = ImnCpm))
save(fr_cpm, file = "data/Clearance Rates/Feeding Rates/fr_cpm.Rdata")
write_xlsx(fr_cpm, "data/Clearance Rates/Feeding Rates/fr_cpm.xlsx")

### Add columns for the mean CR and the mean FR so I can look at them all together
fr_cpm_mnCrFr <- fr_cpm %>% 
  group_by(event, group, type, sa, la, wi, esd, size, vpm, ImnCpm) %>%
  summarize(CRmn = mean(CR), FRmn = mean(FR)) 
fr_cpm_mnCrFr <- fr_cpm_mnCrFr %>% 
  drop_na(CRmn, FRmn)
fr_cpm_mnCrFr <- fr_cpm_mnCrFr %>% 
  mutate(Frm_L = FRmn/1000)
save(fr_cpm_mnCrFr, file = "data/Clearance Rates/Feeding Rates/fr_cpm_mnCrFr.Rdata")
write_xlsx(fr_cpm_mnCrFr,  "data/Clearance Rates/Feeding Rates/fr_cpm_mnCrFr.xlsx")

##  Ingestion rate, I = F x mean IC, 
##  Ingestion rate = Clearance rate (in counts or biomass) * 
##  the mean initial samples (counts or biomass)

#fr_cpm_noNA <- fr_cpm %>% 
#  drop_na(c(CR, FR))

### Calculate the median clearance rates across the E samples
### Apply the median function to the CR across the three replicates
#fr_cpm_medians <- fr_cpm %>% 
#  group_by(event, group, type, sa, la, wi, size) %>%
#  summarize(CRmed = median(CR), FRmd = median(FR))
#fr_cpm_medians <- fr_cpm_medians %>% 
#  drop_na(c(CR, FR))


load("data/Clearance Rates/cr_bio_CR.Rdata")

###_____________________INGESTION RATES FOR BIOMASS PGC ML____________________________

### Calculate the initial samples bpcm 
fr_bio_I <- volbio_all_cr %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, type, sa, la, wi, esd, bio_pgC_ml, size)

### Apply the mean function to the initial mean bpcm across
## the three replicates
fr_bio_Imn <- fr_bio_I %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, esd, size) %>%
  summarize(Imnbpm = mean(bio_pgC_ml),
            n=length(bio_pgC_ml)) %>% 
  ungroup

### Remove unneeded columns, and rename and re-order remaining columns
names(fr_bio_Imn)
fr_bio_Imn <- select(fr_bio_Imn, 
                    event = samp_ev,
                    sample = exp,
                    group = Group,
                    type, sa, la, wi, esd,
                    Imnbpm,
                    size = size)

### Join fr_bio_Imn with the df that has clearance rates, cr_bio_CR
load("data/Clearance Rates/cr_bio_CR.Rdata")

### Join the clearance rate df with the initial bio means df
fr_cr_bio <- 	left_join(cr_cpm, fr_bio_Imn, 
                        by = c("event", "group", "type", "sa", "la", "wi", "esd", "size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(fr_cr_bio)
fr_cr_bio <- select(fr_cr_bio, 
                     event, group,
                     type, sa, la, wi, esd,
                     size, Imnbpm, CR)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

fr_bio <- rowwise(fr_cr_bio) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imnbpm))
save(fr_bio, file = "data/Clearance Rates/Feeding Rates/fr_bio.Rdata")
write_xlsx(fr_bio, "data/Clearance Rates/Feeding Rates/fr_bio.xlsx")

### Add columns for the mean CR and the mean FR so I can look at them all together
fr_bio_mnCrFr <- fr_bio %>% 
  group_by(event, group, type, sa, la, wi, esd, size, Imnbpm) %>%
  summarize(CRmn = mean(CR), FRmn = mean(FR)) 
fr_bio_mnCrFr <- fr_bio_mnCrFr %>% 
  drop_na(CRmn, FRmn)
fr_bio_mnCrFr <- fr_bio_mnCrFr %>% 
  mutate(Frm_L = FRmn/1000)
save(fr_bio_mnCrFr, file = "data/Clearance Rates/Feeding Rates/Means/fr_bio_mnCrFr.Rdata")
write_xlsx(fr_bio_mnCrFr, "data/Clearance Rates/Feeding Rates/Means/fr_bio_mnCrFr.xlsx")

### Calculate the median clearance rates across the E samples
### Apply the median function to the CR across the three replicates
#fr_bio_means <- fr_bio %>% 
##  select(-Imn) %>% 
##  group_by(event, group, type, sa, la, wi, size, FR) %>%#
#  summarize(CRmn = mean(CR))
#fr_bio_means <- fr_bio_means %>% 
#  drop_na(CRmn)
# fr_bio_means <- fr_bio_means %>% 
#  group_by(event, group, type, sa, la, wi, size, CRmn) %>%
#  summarize(FRmn = mean(FR))
#fr_bio_means <- fr_bio_means %>% 
#  summarise(FRmns = mean(FRmn), CRmns = mean(CRmn))
#write_xlsx(fr_bio_means, "data/Clearance Rates/Feeding Rates/fr_bio_means.xlsx")
#fr_bio_medians_noNA <- fr_bio_medians %>% 
#  drop(CRmed)

##  Ingestion rate, I = F x mean IC, 
##  Ingestion rate = Clearance rate (in counts or biomass) * 
##  the mean initial samples (counts or biomass)

### _________ Make a df that combines rep CR with mean CR_________________
###_______________________________________________________________________
### Use cr_cpm, and drop all rows that have NA
cr_cpm_noNA <- cr_cpm %>% 
  drop_na(CR)
### Join with crmn_cpm, which has the CR means, and already dropped the NAs
cr_cpm_mn <- 	left_join(cr_cpm_noNA, crmn_cpm, 
                        by = c("event", "sample", "group", "type", "sa", "la", "wi", 
                               "esd", "size"))
### Remove the unneeded columns
names(cr_cpm_mn)
cr_cpm_mn <- select(cr_cpm_CmnE, 
                      event,
                      sample,
                      rep,
                      group,
                      type, sa, la, wi, esd,
                      CR, crMn,
                      size)

save(cr_cpm_mn, file = "data/Clearance Rates/cr_cpm_mn.Rdata")

###__________________Make individual dfs for each sampling event___________

load("data/Clearance Rates/Feeding Rates/fr_bio.Rdata")
load("data/Clearance Rates/Feeding Rates/fr_cpm.Rdata")

fr_cpm_SJR2 <- fr_cpm_mnCrFr %>%
  filter(event == "SJR2")
write_xlsx(fr_cpm_SJR2, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_SJR2.xlsx")

fr_cpm_YBP1 <- fr_cpm_mnCrFr %>%
  filter(event == "YBP1")
write_xlsx(fr_cpm_YBP1, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_YBP1.xlsx")

fr_cpm_LSZ2 <- fr_cpm_mnCrFr %>%
  filter(event == "LSZ2")
write_xlsx(fr_cpm_LSZ2, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_LSZ2.xlsx")

fr_cpm_WLD2 <- fr_cpm_mnCrFr %>%
  filter(event == "WLD2")
write_xlsx(fr_cpm_WLD2, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_WLD2.xlsx")

fr_cpm_SJR1 <- fr_cpm_mnCrFr %>%
  filter(event == "SJR1")
write_xlsx(fr_cpm_SJR1, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_SJR1.xlsx")

fr_cpm_YBP2 <- fr_cpm_mnCrFr %>%
  filter(event == "YBP2")
write_xlsx(fr_cpm_YBP2, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_YBP2.xlsx")

### Biomass
fr_bio_SJR2 <- fr_bio_mnCrFr %>%
  filter(event == "SJR2")
write_xlsx(fr_bio_SJR2, "data/Clearance Rates/Feeding Rates/Means/fr_bio_SJR2.xlsx")

fr_bio_YBP1 <- fr_bio_mnCrFr %>%
  filter(event == "YBP1")
write_xlsx(fr_bio_YBP1, "data/Clearance Rates/Feeding Rates/Means/fr_bio_YBP1.xlsx")

fr_bio_LSZ2 <- fr_bio_mnCrFr %>%
  filter(event == "LSZ2")
write_xlsx(fr_bio_LSZ2, "data/Clearance Rates/Feeding Rates/Means/fr_bio_LSZ2.xlsx")

fr_bio_WLD2 <- fr_bio_mnCrFr %>%
  filter(event == "WLD2")
write_xlsx(fr_bio_WLD2, "data/Clearance Rates/Feeding Rates/Means/fr_bio_WLD2.xlsx")

fr_bio_SJR1 <- fr_bio_mnCrFr %>%
  filter(event == "SJR1")
write_xlsx(fr_bio_SJR1, "data/Clearance Rates/Feeding Rates/Means/fr_bio_SJR1.xlsx")

fr_bio_YBP2 <- fr_bio_mnCrFr %>%
  filter(event == "YBP2")
write_xlsx(fr_bio_YBP2, "data/Clearance Rates/Feeding Rates/Means/fr_bio_YBP2.xlsx")

### _________Test, Look at just diatoms, run CR FR with different bimass conversions

### Current biomass conversion: (volume > 3000)) biomass = .117 * volume^.881
##  (volume <= 3000) biomass = .287 * volume^.811

fr_cpm_diatoms <- fr_cpm_mnCrFr %>% 
  filter(group =="diatom")
write_xlsx(fr_cpm_diatoms, "data/Clearance Rates/Feeding Rates/Means/fr_cpm_diatoms.xlsx")

### Below is a df using the protist plankton biomass conversion for both small and large diatoms
fr_cpm_diatoms2 
fr_cpm_diatoms2 <- fr_cr_cpm %>% 
  filter(group == "diatom")

#<- rowwise(fr_cr_bio) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imn))
save(fr_bio, file = "data/Clearance Rates/Feeding Rates/fr_bio.Rdata")
write_xlsx(fr_bio, "data/Clearance Rates/Feeding Rates/fr_bio.xlsx")
