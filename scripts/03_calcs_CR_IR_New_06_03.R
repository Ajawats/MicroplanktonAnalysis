############################################################################
############### CALCULATE CLEARANCE RATE AND INGESTION RATE  ###############
###################### DON'T REPLACE NAS WITH ZEROS  #######################
############################################################################

### 7/24/23 updated files to include the YBP1 centric diatom large exp rep 2 count to 1
##  and created a new data folder data7_24 because I lost the data folder when I made a
##  Git Hub repository.

### 6/2/23, + 6/7, + 6/8, + 6/9
### DFs created here:
load("data7_24/Clearance Rates 2/sumCpm_cr.Rdata") #Clearance Rates with reps, no means
load("data7_24/Clearance Rates 2/sumCpm_CRmn.Rdata") # Clearance Rates means only
load("data7_24/Clearance Rates 2/CR_Rep_Mn.Rdata") # Clearance rates reps and means
load("data7_24/Clearance Rates 2/IrCrBio_Rep_Mn.Rdata") # Clearance rates reps and means,
###  with ingestion rates, biomass, reps and means, pgC and µgC
load("data7_24/Clearance Rates 2/Feeding Rates/FRCells_Rep_Mn.Rdata") # Ingestion rates,
### cells, reps and means
load("data7_24/Clearance Rates 2/CR_IRbio_mn.Rdata") # Clearance rates and 
### ingestion rates, means only. This df keeps the NAs and NaNs.
load("data7_24/Clearance Rates 2/CrIrCntRepMn.Rdata") # Clearance rates, ingestion rates
### ugC, reps and means, rep numbers, total counts, counts per ml
load("data7_24/Clearance Rates 2/CrIrCntRepMn_NAs.Rdata") # The above df, but with only
### the taxa groups that had an NA CR.
load("data7_24/Clearance Rates 2/CrIrCntMnTots2.Rdata")

### Also, around line 700, some work with the NA CR taxa groups
### And around line 726, some work with total biomass per taxa groups,
##  of all taxa, and also of taxa exluding the taxa I excluded for low numbers and oher reasons




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
load("data7_24/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
source("scripts/01_function_feedingRate.R")


#load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
#load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata") #Ingestion Rates,
### biomass, means only, µg C and pg C
#load( "data/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

### ____________  CREATE THE BASE DATA FRAME ___________________

base <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, esd, group_size, cpm, bio_pgC_ml)
save(base, file = "data7_24/Clearance Rates 2/base.Rdata")

###### _________________CLEARANCE RATE _________________________###
##################################################################

### Sum up the cpm for the 15 group_size groups, adding all cpm for organisms
##  that fall into those 15 categories, such as, all the small centric diatoms
##  in a sampling event, experimental bottle
sumCpm <- base %>% 
  group_by(samp_ev, group_size, exp, rep) %>% 
  summarise(TotalCpm=sum(cpm),
            .groups = 'drop') %>% 
  as.data.frame()

## Create the base data frame that has only the control and experimental cpm
sumCpm_CE <- sumCpm %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
sumCpm_C <- sumCpm_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = TotalCpm)

### Create a df with only experimental samples
sumCpm_E <- sumCpm_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = TotalCpm) 

### Create a df with only initial samples (for the ingestion rates)
sumCpm_I <- sumCpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sumCpm_Cmn <- sumCpm_C %>% 
  group_by(samp_ev, group_size, exp) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sumCpmE_Cmn <- 	left_join(sumCpm_E, sumCpm_Cmn, 
                          by = c("samp_ev", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
##  Do I need the exp column?--Can't keep it since E is one column and C
## is another column
names(sumCpmE_Cmn)
sumCpmE_Cmn <- select(sumCpmE_Cmn, 
                      event = samp_ev,
                      group_size, rep, cpmE,
                      Cmn)

### Calculate clearance rates. The resulting data frame includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sumCpm_cr <- rowwise(sumCpmE_Cmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = Cmn, expCt = cpmE))
save(sumCpm_cr, file = "data7_24/Clearance Rates 2/sumCpm_cr.Rdata")
write_xlsx(sumCpm_cr, "data7_24/Clearance Rates 2/sumCpm_cr.xlsx")
load("data7_24/Clearance Rates 2/sumCpm_cr.Rdata")
### Extract the taxa groups with NA
Cpm_cr_NA <- sumCpm_cr[rowSums(is.na(sumCpm_cr)) > 0,]
write_xlsx(Cpm_cr_NA, "data7_24/Clearance Rates 2/Cpm_cr_NA.xlsx")

###  Take the mean of the CR per taxa group (group_size)
sumCpm_CRmn <- sumCpm_cr %>% 
  group_by(event, group_size) %>% 
  summarize(CrMNmlcd = mean(CRmlcd, na.rm = TRUE))
write_xlsx(sumCpm_CRmn, "data7_24/Clearance Rates 2/sumCpm_CRmn.xlsx")
save(sumCpm_CRmn,file =  "data7_24/Clearance Rates 2/sumCpm_CRmn.Rdata")
load("data7_24/Clearance Rates 2/sumCpm_CRmn.Rdata")

### Take the mean of all the CR taxa group means across all sampling events
##  to get an overall CR of each taxa group as a whole.
sumCRmnTaxa <- sumCpm_CRmn %>% 
  group_by(group_size) %>% 
  summarise(MeanCR = mean(CrMNmlcd, na.rm = TRUE), ## use na.rm = TRUE so it will calculate
            ## the means of the real numbers and ignore the NAs
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(sumCRmnTaxa, "data7_24/Clearance Rates 2/sumCRmnTaxa.xlsx")
save(sumCRmnTaxa, file = "data7_24/Clearance Rates 2/sumCRmnTaxa.Rdata")
load("data7_24/Clearance Rates 2/sumCRmnTaxa.Rdata")
### Sum the CR means by sampling event, no taxa groups
sumCRmnEventsOnly <- sumCpm_CRmn %>% 
  group_by(event) %>% 
  summarise(MeanCR = mean(CrMNmlcd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(sumCRmnEventsOnly, "data7_24/Clearance Rates 2/sumCRmnEventsOnly.xlsx")
save(sumCRmnEventsOnly, file = "data7_24/Clearance Rates 2/sumCRmnEventsOnly.Rdata")
### _________ Make a df that combines rep CR with mean CR_________________
###_______________________________________________________________________
### Use this to plot the CR means with the reps so I can see if the outliers
##  have one crazy CR that's pulling up or down the means
names(sumCpm_cr)
names(sumCpm_CRmn)
CR_Rep_Mn <- 	left_join(sumCpm_cr, sumCpm_CRmn, 
                        by = c("event", "group_size"))
save(CR_Rep_Mn, file= "data7_24/Clearance Rates 2/CR_Rep_Mn.Rdata")
write_xlsx(CR_Rep_Mn, "data7_24/Clearance Rates 2/CR_Rep_Mn.xlsx")
load("data7_24/Clearance Rates 2/CR_Rep_Mn.Rdata")

###### ###### ###### ###### 
###### Clearance Rate with taxa groups ordered in large and small grouping
###### ###### ###### ###### 

load("data7_24/Clearance Rates 2/sumCpm_CRmn.Rdata")
CR_Size <- sumCpm_CRmn %>% 
  mutate(group_size = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(CR_Size, file = "data7_24/Clearance Rates 2/CR_Size.Rdata")
write_xlsx(CR_Size, "data7_24/Clearance Rates 2/CR_Size.xlsx")

### Need to take the mean of the CR per taxon across all events
## This is the same as sumCRmnAll
CR_allEvents <- CR_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalCRmn = mean(CrMNmlcd, na.rm = TRUE)) %>% 
  ungroup()
### See 04_plots_IR_Various.R for plotting it.


################################################################################
### _________INGESTION (FEEDING) RATE BY BIOMASS ______________________________
################################################################################

### Sum up the biomass, pgC mL^1 (Bpm) for the 15 group_size groups, adding all bpm for 
##  organisms that fall into those 15 categories, such as all the small centric diatoms
##  in a sampling event, experimental bottle. Need to use pgC mL^1 because CR are in units
## of mL copepod^1 d^1, so the biomass units need to correspond. If I want to plot in 
## terms of ugC L^1, I can do the conversion then.

## Create the base data frame that has only the biomass, sum the bio_pgC_ml
## for all same-sized group_size organsims, per event, per exp, per rep
load("data7_24/Clearance Rates 2/base.Rdata")
sumBpm <- base %>% 
  group_by(samp_ev, exp, rep, group_size) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sumBpm_I <- sumBpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, group_size, TotalBpm)

### Apply the mean function to the initial mean biomass pgC per ml across
## the three replicates, as done in the CR
sumBpm_Imn <- sumBpm_I %>% 
  group_by(samp_ev, exp, group_size) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_Imn)
sumBpm_Imn <- select(sumBpm_Imn, 
                     event = samp_ev, exp, 
                     group_size, ImnBpm)

### Join sumBpm_Imn with the df that has clearance rates, sumCpm_cr
load("data7_24/Clearance Rates 2/sumCpm_cr.Rdata")
sumBpm_cr_Imn <- 	left_join(sumCpm_cr, sumBpm_Imn, 
                            by = c("event", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_cr_Imn)
sumBpm_cr_Imn <- select(sumBpm_cr_Imn, 
                        event, rep, group_size, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumBpm_FR <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sumBpm_FR <- sumBpm_FR %>% 
  rename("FRpgCcd" = "FR")
save(sumBpm_FR, file = "data7_24/Clearance Rates 2/Feeding Rates/sumBpm_FR.Rdata")
write_xlsx(sumBpm_FR, "data7_24/Clearance Rates 2/Feeding Rates/sumBpm_FR.xlsx")
load("data7_24/Clearance Rates 2/Feeding Rates/sumBpm_FR.Rdata")

### Calculate the means across the groups and sizes
BpmFRMn_GrpSz <- sumBpm_FR %>%
  group_by(event, group_size) %>%
  summarise(FRmnpgCcd = mean(FRpgCcd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
save(BpmFRMn_GrpSz, file = "data7_24/Clearance Rates 2/Feeding Rates/BpmFRMn_GrpSz.Rdata")
write_xlsx(BpmFRMn_GrpSz, "data7_24/Clearance Rates 2/Feeding Rates/BpmFRMn_GrpSz.xlsx")

## Add a column with the conversion from pg to µg
BugFRMn_GrpSz <- BpmFRMn_GrpSz %>% 
  group_by(event, group_size, FRmnpgCcd) %>%
  summarise(FRmnUgCcd = FRmnpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 
save(BugFRMn_GrpSz, file = "data7_24/Clearance Rates 2/Feeding Rates/BugFRMn_GrpSz.Rdata")
write_xlsx(BugFRMn_GrpSz, "data7_24/Clearance Rates 2/Feeding Rates/BugFRMn_GrpSz.xlsx")
load("data7_24/Clearance Rates 2/Feeding Rates/BugFRMn_GrpSz.Rdata")

### ______ Make a df that combines rep FR (biomass) with mean FR (biomass) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
load("data7_24/Clearance Rates 2/Feeding Rates/BpmFRMn_GrpSz.Rdata")
load("data7_24/Clearance Rates 2/Feeding Rates/sumBpm_FR.Rdata")
names(sumBpm_FR)
names(BpmFRMn_GrpSz)
FRbpm_Rep_Mn <- 	left_join(sumBpm_FR, BpmFRMn_GrpSz, 
                           by = c("event", "group_size"))
save(FRbpm_Rep_Mn, file= "data7_24/Clearance Rates 2/Feeding Rates/FRbpm_Rep_Mn.Rdata")
load("data7_24/Clearance Rates 2/Feeding Rates/FRbpm_Rep_Mn.Rdata")

### ______________ADD A COLUMN OF INGESTION RATE IN µg C per copepod per day________________
### for the means and the reps
##  1µg = 1,000,000 pg
##  1 µgC per copepod per day  = 1 pgC per copepod per day/ 1,000,000  (1 pgC)/(1 ml) * (1000 ml)/(1 L) * (0.000001 µg)/(1 pgC) =  0.001 µg L-1

# for Ingestion Rate per rep
FRBio_Rep_Mn <- FRbpm_Rep_Mn %>% 
  group_by(event, rep, group_size, CRmlcd, FRpgCcd, FRmnpgCcd) %>%
  reframe(FRUgCcd = FRpgCcd/1000000) 
#For ingestion rate means
FRBio_Rep_Mn <- FRBio_Rep_Mn %>% 
  group_by(event, rep, group_size, CRmlcd, FRpgCcd, FRmnpgCcd, FRUgCcd) %>%
  reframe(FRmnUgCcd = FRmnpgCcd/1000000) 
# Rearrange columns to group together the means both units and the reps both units
FRBio_Rep_Mn <- FRBio_Rep_Mn %>% 
  select(event, rep, group_size, CRmlcd, FRpgCcd, FRUgCcd, FRmnpgCcd, FRmnUgCcd)

save(FRBio_Rep_Mn, file = "data7_24/Clearance Rates 2/Feeding Rates/FRBio_Rep_Mn.Rdata")
write_xlsx(FRBio_Rep_Mn, "data7_24/Clearance Rates 2/Feeding Rates/FRBio_Rep_Mn.xlsx")
load("data7_24/Clearance Rates 2/Feeding Rates/FRBio_Rep_Mn.Rdata")

### Calculate the CR mean using the above df so I can then join it all together on one df
### Add the CR and CR means to the above df
FRBio_Rep_Mn <- FRBio_Rep_Mn %>% 
  group_by(event, group_size) %>%
  mutate(CRmn = mean(CRmlcd)) %>% 
  ungroup() 
IrCrBio_Rep_Mn <- left_join(CR_Rep_Mn, FRBio_Rep_Mn,  
                            by = c("event", "rep", "group_size"))
IrCrBio_Rep_Mn <- IrCrBio_Rep_Mn %>% 
  select(event, rep, group_size, cpmE, Cmn, CRmlcd = CRmlcd.x,
         CrMNmlcd, FRpgCcd, FRUgCcd, FRmnpgCcd, 
         FRmnUgCcd)
save(IrCrBio_Rep_Mn, file = "data7_24/Clearance Rates 2/IrCrBio_Rep_Mn.Rdata")
write_xlsx(IrCrBio_Rep_Mn,"data7_24/Clearance Rates 2/IrCrBio_Rep_Mn.xlsx")
load("data7_24/Clearance Rates 2/IrCrBio_Rep_Mn.Rdata")
################################################################################
#_____________ Make a df of IR bio totals by sampling event ____________________
################################################################################
### take the mean FR of all taxa groups in each sampling event
FRBio_EventMn <- FRBio_Rep_Mn %>% 
  group_by(event) %>% 
  summarise(FRmnUgCcd = mean(FRmnUgCcd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
save(FRBio_EventMn, file = "data7_24/Clearance Rates 2/Feeding Rates/FRBio_EventMn.Rdata")
write_xlsx(FRBio_EventMn,"data7_24/Clearance Rates 2/Feeding Rates/FRBio_EventMn.xlsx")

### Take the total FR of all Taxa groups in each sampling event
## Re-do this, because it's summing the three entries of the means, so tripling
##  the actual number
#FRBio_Event <- FRBio_Rep_Mn %>% 
 # group_by(event) %>% 
  #summarise(FRmnUgCcd = sum(FRmnUgCcd, na.rm = TRUE),
   #         .groups = 'drop') %>% 
  #as.data.frame()
#save(FRBio_Event, file = "data/Clearance Rates 2/Feeding Rates/FRBio_Event.Rdata")
#write_xlsx(FRBio_Event,"data/Clearance Rates 2/Feeding Rates/FRBio_Event.xlsx")



### Make a df or IR bio totals of each taxa group across all events
FRBio_Taxa <- FRBio_Rep_Mn %>% 
  group_by(group_size) %>% 
  summarise(FRmnUgCcd = mean(FRmnUgCcd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
save(FRBio_Taxa, file = "data7_24/Clearance Rates 2/Feeding Rates/FRBio_Taxa.Rdata")
write_xlsx(FRBio_Taxa,"data7_24/Clearance Rates 2/Feeding Rates/FRBio_Taxa.xlsx")


################################################################################
### _________ CLEARANCE RATE WITH INGESTION RATE BY BIOMASS ____________________
################################################################################
CR_IRbio_mn <- 	left_join(sumCpm_CRmn, BugFRMn_GrpSz, 
                          by = c("event", "group_size"))
save(CR_IRbio_mn, file = "data7_24/Clearance Rates 2/CR_IRbio_mn.Rdata")
write_xlsx(CR_IRbio_mn, "data7_24/Clearance Rates 2/CR_IRbio_mn.xlsx")
load("data7_24/Clearance Rates/CR_IRbio_mn.Rdata")
### This returns zeros where the NAs and NaNs were. Try to use merge and see what happens
CR_IRbio_mn_test <- 	merge(sumCpm_CRmn, BugFRMn_GrpSz, 
                          by = c("event", "group_size"))
### Yes, "merge" keeps the NAs and NaNs.
CR_IRbio_mn <- 	merge(sumCpm_CRmn, BugFRMn_GrpSz, 
                           by = c("event", "group_size"))
save(CR_IRbio_mn, file = "data7_24/Clearance Rates 2/CR_IRbio_mn.Rdata")
write_xlsx(CR_IRbio_mn, "data7_24/Clearance Rates 2/CR_IRbio_mn.xlsx")
load("data7_24/Clearance Rates 2/CR_IRbio_mn.Rdata")

### Look at the CR and IR means together of just the CR NA taxa
CR_IRbio_mn_NAs <- CR_IRbio_mn %>%
  filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
                         "CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                         "CenDiaLg"))
save(CR_IRbio_mn_NAs, file = "data7_24/Clearance Rates 2/CR_IRbio_mn_NAs.Rdata")
write_xlsx(CR_IRbio_mn_NAs, "data7_24/Clearance Rates 2/CR_IRbio_mn_NAs.xlsx")

################################################################################
####_____________INGESTION (FEEDING) RATE BY CELLS (COUNTS, cpm) __________
################################################################################

### Run the base data frame code (created above, near the top) that has the initial samples
sumCpm_I <- sumCpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumCpm_Imn <- sumCpm_I %>% 
  group_by(samp_ev, group_size) %>%
  summarise(Imn = mean(cpmI),
            .groups = 'drop') %>% 
  rename(event = samp_ev) %>% 
  as.data.frame()

### Join sumCpm_Imn with the df that has clearance rates, sumCpm_cr,
##  reorder columns so CR is the end column
load("data7_24/Clearance Rates 2/sumCpm_cr.Rdata")
names(sumCpm_cr)
names(sumCpm_Imn)
sumCpm_cr_fr <- 	left_join(sumCpm_cr, sumCpm_Imn, 
                           by = c("event", "group_size"))# %>% 

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumCpm_FR <- rowwise(sumCpm_cr_fr) %>% 
  mutate(FRCellsCd = fr_func(CR=CRmlcd, initialMnCt = Imn))
save(sumCpm_FR, file = "data7_24/Clearance Rates 2/Feeding Rates/sumCpm_FR.Rdata")
write_xlsx(sumCpm_FR, "data7_24/Clearance Rates 2/Feeding Rates/sumCpm_FR.xlsx")

load("data7_24/Clearance Rates 2/Feeding Rates/sumCpm_FR.Rdata")

### Take the mean FR for group and size, across the 3 reps, i.e., CenDiaLg in LSZ2
CpmFRMn_GrpSz <- sumCpm_FR %>%
  group_by(event, group_size) %>%
  summarise(FRmnCellsCd = mean(FRCellsCd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
save(CpmFRMn_GrpSz, file = "data7_24/Clearance Rates 2/Feeding Rates/CpmFRMn_GrpSz.Rdata")
load("data7_24/Clearance Rates 2/Feeding Rates/CpmFRMn_GrpSz.Rdata")


### ______ Make a df that combines rep FR (cells) with mean FR (cells) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
names(sumCpm_FR)
names(CpmFRMn_GrpSz)
FRCells_Rep_Mn <- 	left_join(sumCpm_FR, CpmFRMn_GrpSz, 
                             by = c("event", "group_size"))
save(FRCells_Rep_Mn, file= "data7_24/Clearance Rates 2/Feeding Rates/FRCells_Rep_Mn.Rdata")

####################
########## Ingestion rate, cells, combined totals, events only #########
####################
IRcells_eventsOnly <- CpmFRMn_GrpSz %>% 
  group_by(event) %>% 
  summarise(IRcellsMn = mean(FRmnCellsCd, na.rm = TRUE,
            .groups = 'drop')) %>% 
  as.data.frame()
load(IRcells_eventsOnly, file= "data7_24/Clearance Rates 2/Feeding Rates/IRcells_eventsOnly.Rdata")

names(CpmFRMn_GrpSz)
IRcells_Taxa <- CpmFRMn_GrpSz %>% 
  group_by(group_size) %>% 
  summarise(FRmnCellsCd = mean(FRmnCellsCd, na.rm = TRUE),
            .groups = 'drop') %>% 
  as.data.frame()
save(IRcells_Taxa, file = "data7_24/Clearance Rates 2/Feeding Rates/IRcells_Taxa.Rdata")
write_xlsx(IRcells_Taxa,"data7_24/Clearance Rates 2/Feeding Rates/IRcells_Taxa.xlsx")

######### This is where I stopped 6/6/23 Below is pasted from 03_calcs_CR_FR.R





### Look at the chain diatom group_size to see what the numbers are like
ChnDiaFRBio <- FRBio_Rep_Mn %>% 
  filter(group_size =="ChnDiaLg" | group_size =="ChnDiaSm")
write_xlsx(ChnDiaFRBio, "data7_24/Clearance Rates/Feeding Rates/ChnDiaFRBio.xlsx")

a <- ggplot(data=ChnDiaFRBio, aes(event, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(data = ChnDiaFRBio,
  # aes(y=FRUgCcd),
  #color="orange", size=2)+
  #xlab("Taxon Group, -centric diatoms") +
  ggtitle("Chain Diatom Biomass Ingestion Rates")+
  scale_y_continuous()+#limits = c(-.05, .09), 
  # breaks = c(0.3, 0, .01, .06, .07, .08)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (12)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Look at pennate diatoms IR bio now that the chain diatoms were removed from 
##  their group and note the change in IR. Chain diatoms only appeared in
##  YBP2 (IRbio 0.07) and LSZ2 (IR bio 0.001)
PenDiaIRbio <- BugFRMn_GrpSz %>% 
  filter(group_size == "PenDiaSm" | group_size == "PenDiaLg")
write_xlsx(PenDiaIRbio, "data7_24/Clearance Rates/Feeding Rates/PenDiaIRbio.xlsx")

### And CR pennates and chains
PenDiaCR <- sumCpm_CRmn %>% 
  filter(group_size == "PenDiaSm" | group_size == "PenDiaLg")

#####################################################################
############### SORT BY SMALL AND LARGE GROUPS #####################
#####################################################################
## Use these dfs:
load("data7_24/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
load( "data7_24/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion

### Ingestion Rates, biomass, Means only
FR_bio_Size <- FRBio_Rep_Mn %>% 
  mutate(group_size = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(FR_bio_Size, file = "data7_24/Clearance Rates/Feeding Rates/FR_bio_Size.Rdata")
write_xlsx(FR_bio_Size, "data7_24/Clearance Rates/Feeding Rates/FR_bio_Size.xlsx")
### Need to take the mean of the FRbio per taxon across all events
FRbio_allEvents <- FR_bio_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalFRbioMn = mean(FRmnUgCcd)) %>% 
  ungroup()
save(FRbio_allEvents, file = "data7_24/Clearance Rates/Feeding Rates/FRbio_allEvents.Rdata")
write_xlsx(FRbio_allEvents, "data7_24/Clearance Rates/Feeding Rates/FRbio_allEvents.xlsx")
### See 04_plots_IR_Various.R for plotting it.

### Ingestion rates, cells, Means only
load ("data7_24/Clearance Rates/Feeding Rates/CpmFRMn_GrpSz.Rdata")
FR_cell_Size <- CpmFRMn_GrpSz %>% 
  mutate(group_size = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(FR_cell_Size, file = "data7_24/Clearance Rates/Feeding Rates/FR_cell_Size.Rdata")
write_xlsx(FR_cell_Size, "data7_24/Clearance Rates/Feeding Rates/FR_cell_Size.xlsx")
load("data7_24/Clearance Rates/Feeding Rates/FR_cell_Size.Rdata")

### Take the mean of the IR cells per taxon across all events
IRcells_allEvents <- FR_cell_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalIRcells = mean(FRmnCellsCd)) %>% 
  ungroup()
save(IRcells_allEvents, file = "data7_24/Clearance Rates/Feeding Rates/IRcells_allEvents.Rdata")
### See 04_plots_IR_Various.R for plotting it.

#####################################################################
############### TOP 5 TAXA GROUPS IR REPLICATES #####################
#####################################################################
### Using the code from above, but modify so it keeps the replicate numbers

base <- data %>% 
  select(samp_ev, exp, rep, group_size, cpm, bio_pgC_ml)
save(base, file = "data7_24/Clearance Rates/base.Rdata")

sumBpm <- base %>% 
  group_by(samp_ev, group_size, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sumBpm_I <- sumBpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, group_size, TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumBpm_Imn <- sumBpm_I %>% 
  group_by(samp_ev, exp, group_size) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_Imn)
sumBpm_Imn <- select(sumBpm_Imn, 
                     event = samp_ev, exp, group_size, ImnBpm)

### Join sumBpm_Imn with the df that has clearance rates, sumCpm_cr
load("data7_24/Clearance Rates/sumCpm_cr.Rdata")
sumBpm_cr_Imn <- 	left_join(sumCpm_cr, sumBpm_Imn, 
                            by = c("event", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_cr_Imn)
sumBpm_cr_Imn <- select(sumBpm_cr_Imn, 
                        event, group_size, rep, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumBpm_FR <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sumBpm_FR <- sumBpm_FR %>% 
  rename("FRpgCcd" = "FR")
### Filter out everything but the top 5 taxa groups:
##  CilLg, CenDiaLg, CilSm, FlagSm, CenDiaSm
IRbioTop5 <- sumBpm_FR %>% 
  filter(group_size %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 
### Add a column in µg C L^1
IRbioTop5 <- IRbioTop5 %>% 
  group_by(event, group_size, rep, CRmlcd,FRpgCcd) %>%
  summarise(FRUgCcd = FRpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 

### Add the mean CR and FR
### Filter to keep the top 5 in the CR IR mean df
load("data7_24/Clearance Rates/CR_IRbio_mn.Rdata")
Top5CR_IRbio_mn <-CR_IRbio_mn %>% 
  filter(group_size %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 

IRbioCRTop5 <- IRbioTop5 %>% 
  left_join(Top5CR_IRbio_mn, IRbioTop5,
            by = c("event", "group_size"))

save(IRbioCRTop5, file = "data7_24/Clearance Rates/Feeding Rates/IRbioCRTop5.Rdata")
write_xlsx(IRbioCRTop5, "data7_24/Clearance Rates/Feeding Rates/IRbioCRTop5.xlsx")
load("data7_24/Clearance Rates/Feeding Rates/IRbioCRTop5.Rdata")

### 6/8/23 Determining a cut-off total count number so I can eliminate that
## taxa group from the analysis, also referring to the NA CR
## Get the reps cpmE, Cmn, CR for all the 11 taxa grops that had an NA CR in at least
## one of their reps, The groups are: filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
#"CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
#"CenDiaLg")) %>% 
#  select(samp_ev, exp, rep, group_size, counts)
#load("data/Clearance Rates 2/CR_Rep_Mn.Rdata")

NA_taxa <- CR_Rep_Mn %>% 
  filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
                           "CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                           "CenDiaLg"))
write_xlsx(NA_taxa, "data7_24/Clearance Rates 2/NA_taxa.xlsx")
NA_taxa_withCounts <- left_join(NA_taxa, abun_NA_sums,
                                by = "group_size")
write_xlsx(NA_taxa_withCounts, "data7_24/Clearance Rates 2/NA_taxa_withCounts.xlsx")

### Means of cells per mL
#FrCpmOverall <- FrGrps %>%
#  group_by(group_size) %>%
#  summarise( FrCpmAllEvents= mean(FRmnCpm))
#save(FrCpmOverall, file = "data/Clearance Rates/Feeding Rates/FrCpmOverall.Rdata")
#write_xlsx(FrCpmOverall, "data/Clearance Rates/Feeding Rates/FrCpmOverall.xlsx")

### Make a df that has event, rep, group_size, CRmlcd, CRmn, FRUgCcd, FrmnUgCcd, 
##  cpm, total counts
## 1) Redo the IR dfs so that I can get biomass IR in µgC with the rep numbers 1,2,3
##  and the IR reps and means calcs; use sumBpm_FR
## 2) Join that to CR_Rep_Mn
## 3) Find or make a df that has the total counts and counts per ml for each taxa entry
## 4) Join the IR CR df with the abun df

### 1) Add column with µg C conversion
IRbio_ugC <- sumBpm_FR %>% 
  group_by(event, rep, group_size, CRmlcd, FRpgCcd, ImnBpm) %>%
  reframe(FRUgCcd = FRpgCcd/1000000) 
### 1a) Add column with FRUgCcd means
IRbio_ugCMn <- IRbio_ugC %>% 
  group_by(event, group_size) %>%
  summarise(FRUgMn = mean(FRUgCcd),
            .groups = 'drop') %>% 
  as.data.frame()
### 1b) Join the means to the reps
IRbio_ugCRepMn <- left_join(IRbio_ugC, IRbio_ugCMn)
### 2) Join the above to CR_Rep_Mn
IRCRbioUg_Rep_Mn <- left_join(CR_Rep_Mn, IRbio_ugCRepMn)

### 3) Use abundanceI.Rdata to calculate the cpm and the total counts
## Total cpm
load("data7_24/Abundance/abundanceI.Rdata")
abunCpm_taxaGroup <- abundanceI %>% 
  group_by(samp_ev, rep, group_size) %>% 
  summarise(TotCpm = sum(cpm))
##  Total counts
abunCounts_taxa <- abundanceI %>% 
  group_by(samp_ev,rep, group_size) %>% 
  summarise(TotCt = sum(counts),
            .groups = 'drop') %>% 
  as.data.frame()
## Join the two above, and rename samp_ev to event
abunCt_Cpm <- left_join(abunCpm_taxaGroup, abunCounts_taxa)
abunCt_Cpm <- abunCt_Cpm %>% 
  rename(event = samp_ev)
save(abunCt_Cpm, file = "data7_24/Abundance/abunCt_Cpm.Rdata")
### 4)
CrIrCntRepMn <- left_join(IRCRbioUg_Rep_Mn,abunCt_Cpm)
CrIrCntRepMn <- CrIrCntRepMn %>% 
  rename(TotICpm = TotCpm, TotICt = TotCt) 

save(CrIrCntRepMn, file = "data7_24/Clearance Rates 2/CrIrCntRepMn.Rdata")
write_xlsx(CrIrCntRepMn, "data7_24/Clearance Rates 2/CrIrCntRepMn2.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntRepMn.Rdata")



### Add columns: one for total counts across all sampling events and
CrIrCntRepMnTots <- CrIrCntRepMn %>% 
  group_by(group_size) %>% 
  mutate(TotCtXallEvents = sum(TotCt)) %>% 
  ungroup()
##  And one for Mean CR across all sampling events; this one takes the means
##  of the individual reps clearance rates.
CrIrCntRepMnTots <- CrIrCntRepMnTots %>% 
  group_by(group_size) %>% 
  mutate(MnCrXbyReps = mean(CRmlcd, na.rm = TRUE)) %>% 
  ungroup()
### Add another column of mean CR calculated from the mean CRs per event
CrIrCntRepMnTots <- CrIrCntRepMnTots %>% 
  group_by(group_size) %>% 
  mutate(MnCrXbyMns = mean(CrMNmlcd, na.rm = TRUE)) %>% 
  ungroup()
### Add a column of the total biomass IR for all events and taxa groups
### DO NOT DO IT THIS WAY!!! This was adding up the Mn FR of all the rows, which
##  includes one entry for each replicate, so it was tripling the Total
#CrIrCntRepMnTots <- CrIrCntRepMnTots %>%
#  mutate(TotIRbioUgCcd = sum(FRUgCcd, na.rm = TRUE)) %>% 
 # select(-TotIRbio)

save(CrIrCntRepMnTots, file = "data7_24/Clearance Rates 2/CrIrCntRepMnTots.Rdata")
write_xlsx(CrIrCntRepMnTots, "data7_24/Clearance Rates 2/CrIrCntRepMnTots.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntRepMnTots.Rdata")

### Compare the mean CR calculated above, for each taxa group across all sampling events,
## calculated by taking the mean CR of all the mean CR

### Make a version of CrIrCntRepMn with just the CR NA taxa groups
CrIrCntRepMn_NAs <- CrIrCntRepMn %>% 
  filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
                           "CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                           "CenDiaLg"))
save(CrIrCntRepMn_NAs, file = "data7_24/Clearance Rates 2/CrIrCntRepMn_NAs.Rdata")
write_xlsx(CrIrCntRepMn_NAs, "data7_24/Clearance Rates 2/CrIrCntRepMn_NAs.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntRepMn_NAs.Rdata")

### Make a version of CrIrCntRepMnTots with just the CR NA taxa groups
CrIrCntRepMnTots_NAs <- CrIrCntRepMnTots %>% 
  filter(group_size %in% c("ChlLg", "ChlSm", "ChnDiaLg", "ChnDiaSm", "CyanoLg",
                           "CyanoSm", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                           "CenDiaLg"))
save(CrIrCntRepMnTots_NAs, file = "data7_24/Clearance Rates 2/CrIrCntRepMnTots_NAs.Rdata")
write_xlsx(CrIrCntRepMnTots_NAs, "data7_24/Clearance Rates 2/CrIrCntRepMnTots_NAs.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntRepMnTots_NAs.Rdata")

### Make a version of CrIrCntRepMnTots with only the positive CR groups, and
##  without the CR NA taxa groups
CrIrCntRepMnTots_noNAs <- CrIrCntRepMnTots %>% 
  filter(group_size %in% c("CenDiaSm", "CilLg", "CilSm", "FlagSm", 
                           "PenDiaSm", "UnidSm"))
save(CrIrCntRepMnTots_noNAs, file = "data7_24/Clearance Rates 2/CrIrCntRepMnTots_noAs.Rdata")
write_xlsx(CrIrCntRepMnTots_noNAs, "data7_24/Clearance Rates 2/CrIrCntRepMnTots_noNAs.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntRepMnTots_noNAs.Rdata")

### Create a version of CrIrCntRepMnTots without the reps, i.e., with the mean cpmE,
## total abundance, mean CR and mean IRbio. Need to take the mean of cpmE
CrIrCntMnTots <- CrIrCntRepMnTots %>% 
  group_by(event, group_size) %>% 
  mutate(cpmEmn = mean(cpmE))

### sum the total counts of the three reps per taxa group per event
CrIrCntMnTots <- subset(CrIrCntMnTots, 
                        select = c(event, group_size, cpmEmn, Cmn, CrMNmlcd, 
                                   FRUgMn, ImnBpm, TotCpm, TotCt,  TotCtXallEvents, 
                                   MnCrXbyReps))

CrIrCntMnTots <- CrIrCntMnTots %>% 
  group_by(event, group_size) %>% 
  mutate(TotalCells = sum(TotCt)) %>% 
  mutate(CpmMn = mean(TotCpm)) 

### Take the means of all the means, they will return the same number
CrIrCntMnTots2 <- CrIrCntMnTots %>% 
  group_by(event, group_size) %>% 
  mutate(cpmEmn = mean(cpmEmn)) %>%
  mutate(Cmn = mean(Cmn)) %>%
  mutate(CrMNmlcd = mean(CrMNmlcd, na.rm = TRUE)) %>%
  mutate(FRUgMn = mean(FRUgMn, na.rm = TRUE)) %>%
  mutate(ImnBpm = mean(ImnBpm)) %>% 
  mutate(TotalCells = mean(TotalCells)) %>% 
  mutate(TotCtXallEvents = mean(TotCtXallEvents)) %>% 
  mutate(CpmMn = mean(CpmMn)) %>%
  mutate(MnCrXbyReps = mean(MnCrXbyReps)) %>% 
  ungroup
### Get rid of the columns that have numbers corresponding to reps
CrIrCntMnTots2 <- subset(CrIrCntMnTots2, select = -c(TotCt, TotCpm))
### Remove the duplicated rows so I"m left with just one entry per taxa per event
##  that has all the means
duplicated(CrIrCntMnTots2)
CrIrCntMnTots2 <- CrIrCntMnTots2 %>% distinct()
save(CrIrCntMnTots2, file = "data7_24/Clearance Rates 2/CrIrCntMnTots2.Rdata")
write_xlsx(CrIrCntMnTots2, "data7_24/Clearance Rates 2/CrIrCntMnTots2.xlsx")
load("data7_24/Clearance Rates 2/CrIrCntMnTots2.Rdata")


###########################################################################
### Total Biomass ingestion rate for all taxa groups together, and also for only 
### the taxa groups I kept after excluding some for low total counts or low or 
### neg CR IR
###########################################################################

### Create the base data frame
load("data7_24/Clearance Rates 2/CR_IRbio_mn.Rdata")
IrTotalsByTaxa <- CR_IRbio_mn %>% 
  group_by(group_size) %>% 
  summarise(IrAllTotalugCcd = sum(FRmnUgCcd)) %>% 
  ungroup()  
### Not sure why I need to do a separate one for the group that had the excluded
## taxa removed, but here it is anyway, removed ChnDiaSm, Dino, CyanoSm, CyanoLg
IrTotalsByTaxaKept <- IrTotalsByTaxa %>% 
  filter(group_size %in% c("CenDiaLg","CenDiaSm", "CilLg", "CilSm", "FlagSm", "ChnDiaLg",
                           "FlagLg", "PenDiaLg", "PenDiaSm", "UnidSm"))
load("data7_24/Clearance Rates 2/IrCrBio_Rep_Mn.Rdata")

IRbioRepMnTotByTaxa <- left_join(IrCrBio_Rep_Mn, IrTotalsByTaxa)
save(IRbioRepMnTotByTaxa, file = "data7_24/Clearance Rates 2/IRbioRepMnTotByTaxa.Rdata")
write_xlsx(IRbioRepMnTotByTaxa, "data7_24/Clearance Rates 2/IRbioRepMnTotByTaxa.xlsx")

### Calculate the proportion that each taxa contributes to the whole by dividing
##  it by the total biomass IR

IrBio_TaxaProp <- IRbioRepMnTotByTaxa %>% 
  mutate(BugCprop = FRmnUgCcd/IrAllTotalugCcd)
save(IrBio_TaxaProp, file = "data7_24/Clearance Rates 2/IrBio_TaxaProp.Rdata")
write_xlsx(IrBio_TaxaProp, "data7_24/Clearance Rates 2/IrBio_TaxaProp.xlsx")

### Do the same as above but remove the taxa I'm exlcuding
##  1) Use IRbioRepMnTotByTaxa to make a df with only the kept taxa
##  2) Divide the taxa ingestion rate mean by the total 

IrBioRepMnKept <- IRbioRepMnTotByTaxa %>% 
  filter(group_size %in% c("CenDiaLg","CenDiaSm", "CilLg", "CilSm", "FlagSm", "ChnDiaLg",
                           "FlagLg", "PenDiaLg", "PenDiaSm", "UnidSm"))

IrBioRepMnKept_Prop <- IrBioRepMnKept %>% 
  mutate(BugCprop = FRmnUgCcd/IrAllTotalugCcd)


###########################################################################
### Total Biomass calcs for each taxa group, and also for only the taxa
### groups I kept after excluding some for low total counts or low or neg CR IR
###########################################################################
### Note: I did this below before doing the above, and then realized I was
##  supposed to calculate the total biomass ingestion rate for all taxa groups
##  together, not the biomass totals of each taxa group
### Create the base data frame
baseTotBioAllTaxa <- volbio_all_cr %>% 
  select(samp_ev, group_size, counts, cpm, tot_biomass_pgC, 
        bio_pgC_ml, bio_ugC_l)
baseTotBioKeptTaxa <- baseTotBioAllTaxa %>% 
  filter(group_size %in% c("CenDiaLg","CenDiaSm", "CilLg", "CilSm", "FlagSm", "ChnDiaLg",
                           "FlagLg", "PenDiaLg", "PenDiaSm", "UnidSm"))

### Sum the total biomass for each taxa group with each data frame
TotBioAllTaxa <- baseTotBioAllTaxa %>% 
  group_by(group_size) %>% 
  summarise(TotBiomassPgCmL = sum(tot_biomass_pgC)) %>% 
  ungroup()
TotCountAllTaxa <- baseTotBioAllTaxa %>% 
  group_by(group_size) %>% 
  summarise(TotCounts = sum(counts)) %>% 
  ungroup()
TotBioCtAllTaxa <- left_join(TotBioAllTaxa, TotCountAllTaxa)
save(TotBioCtAllTaxa, file = "data7_24/Biomass Analysis 06_13/TotBioCtAllTaxa.Rdata")
write_xlsx(TotBioCtAllTaxa, "data7_24/Biomass Analysis 06_13/TotBioCtAllTaxa.xlsx")

TotBioKeptTaxa <- baseTotBioKeptTaxa %>% 
  group_by(group_size) %>% 
  summarise(TotBiomassPgCmL = sum(tot_biomass_pgC)) %>% 
  ungroup()
TotCountKeptTaxa <- baseTotBioKeptTaxa %>% 
  group_by(group_size) %>% 
  summarise(TotCounts = sum(counts)) %>% 
  ungroup()
TotBioCtKeptTaxa <- left_join(TotBioKeptTaxa, TotCountKeptTaxa)
save(TotBioCtKeptTaxa, file = "data7_24/Biomass Analysis 06_13/TotBioCtKeptTaxa.Rdata")
write_xlsx(TotBioCtKeptTaxa, "data7_24/Biomass Analysis 06_13/TotBioCtKeptTaxa.xlsx")

