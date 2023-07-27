############################################################################
## PUT ORGANISMS INTO SMALL AND LARGE CATEGORIES AND CALCULATE CR AND FR  ##
############################################################################

### 2/15/23
### Notes:
##  CR = V/T * (ln(mean control samples) - ln(experimental samples))/n
##  Clearance rate = (Volume of experimental container--595 ml)/ Time of experiment *
##  (ln of the mean of the control samples) - (ln of the experimental samples)/ the number
##  of copepods in the experimental bottle. Units of the result are mL per copepod per day

### Then calculate the initial samples cpm or biomass to be used to calculate ingestion (feeding)
##  rate, I = F x mean IC, Ingestion rate = Clearance rate  * 
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
size_test <- volbio_all_cr
size_allGroups <- size_test

### Can include or exclude "type" from the aggregate argument. If excluded,
##  only the Group will be used for calculations

### _____NOTE!!! on 2/28/23, I removed the "type" from most of the Cpm code

sumCpmAll <- size_test %>% 
  aggregate(cpm ~ samp_ev+exp+rep+Group+size, FUN = sum)
sumBpmAll <- size_test %>% 
  aggregate(bio_ugC_l ~ samp_ev+exp+rep+Group+size, FUN = sum)
### 3/28/23 changed the above sumBpmAll to use ugC per L instead of pgC per ml
sumAll <- left_join(sumCpmAll,sumBpmAll)#%>% 
  #rename("bpm" = "bio_ugC_ml")
save(sumAll, file = "data/Clearance Rates/sumAll.Rdata") 
load("data/Clearance Rates/sumAll.Rdata")


### _________________CLEARANCE RATE ____________________________

## Create the base data frame that has only the controls and experimentals,
##  using either counts per ml or biomass per ml
sumAllCpm_CE <- sumAll %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, Group, size, cpm)

### Create another df from the above, with only the control samples
sumAllCpm_C <- sumAllCpm_CE %>% 
  filter(exp == "C") 

### Create a df with only experimental samples
sumAllCpm_E <- sumAllCpm_CE %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, rep, Group, cpm, size) %>% 
  ungroup
### add E to the cpm column name so it's clear that those are
##  the counts per mL of the experimental samples
names(sumAllCpm_E)[names(sumAllCpm_E) == 'cpm'] <- 'cpmE'

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sumAllCpm_Cmn <- sumAllCpm_C %>% 
  group_by(samp_ev, exp, Group, size) %>%
  summarize(Cmn = mean(cpm)) %>% 
  ungroup

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sumAllCpmE_Cmn <- 	left_join(sumAllCpm_E, sumAllCpm_Cmn, 
                          by = c("samp_ev", "Group", "size"))

### Remove unneeded columns, and rename and re-order remaining columns
##  Do I need the exp column?--Can't keep it since E is one column and C
## is another column
names(sumAllCpmE_Cmn)
sumAllCpmE_Cmn <- select(sumAllCpmE_Cmn, 
                      event = samp_ev,
                      rep,
                      group = Group,
                      size,
                      cpmE,
                      Cmn)


### Calculate clearance rates. The resulting data frame includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sumAllCpm_cr <- rowwise(sumAllCpmE_Cmn) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpmE))
### Replace the NAs with 0 because if not, when R calculates the mean, if one of the reps has NA, but
##  the other one or two have a number, R will not calculate it and will return NA
sumAllCpm_cr <- sumAllCpm_cr %>% 
  mutate(CR = ifelse(is.na(CR), 0, CR))
save(sumAllCpm_cr, file = "data/Clearance Rates/sumAllCpm_cr.Rdata")
write_xlsx(sumAllCpm_cr, "data/Clearance Rates/sumAllCpm_cr.xlsx")
load("data/Clearance Rates/sumAllCpm_cr.Rdata")

###  Take the mean of the CR
crmnsize_sumAllCpm_cr <- sumAllCpm_cr %>% 
  group_by(event, group, size) %>% 
  summarize(crMnCpm = mean(CR))
write_xlsx(crmnsize_sumAllCpm_cr, "data/Clearance Rates/crmnsize_sumAllCpm_cr.xlsx")
save(crmnsize_sumAllCpm_cr,file =  "data/Clearance Rates/crmnsize_sumAllCpm_cr.Rdata")
load("data/Clearance Rates/crmnsize_sumAllCpm_cr.Rdata")

### _________ Make a df that combines rep CR with mean CR_________________
###_______________________________________________________________________
### Use this to plot the CR means with the reps so I can see if the outliers
##  have one crazy CR that's pulling up or down the means
names(sumAllCpm_cr)
names(crmnsize_sumAllCpm_cr)
CR_Rep_Mn <- 	left_join(sumAllCpm_cr, crmnsize_sumAllCpm_cr, 
                        by = c("event", "group", "size"))
save(CR_Rep_Mn, file= "data/Clearance Rates/CR_Rep_Mn.Rdata")
load("data/Clearance Rates/CR_Rep_Mn.Rdata")

### ____________ ADD THE GROUP_SIZE ABBREVIATIONS COLUMN _________________
###_______________________________________________________________________

CrGrps <-  crmnsize_sumAllCpm_cr
## Make a new column named "group_size" that combines the names of the group column
##   and the size column
CrGrps$group_size <- (paste(CrGrps$group, CrGrps$size))

CrGrps["group_size"][CrGrps["group_size"] == "centricDiatom large"] <- "CenDiaLg"
CrGrps["group_size"][CrGrps["group_size"] == "centricDiatom small"] <- "CenDiaSm"
CrGrps["group_size"][CrGrps["group_size"] == "chlorophyte large"] <- "ChlLg"
CrGrps["group_size"][CrGrps["group_size"] == "chlorophyte small"] <- "ChlSm"
CrGrps["group_size"][CrGrps["group_size"] == "ciliate large"] <- "CilLg"
CrGrps["group_size"][CrGrps["group_size"] == "ciliate small"] <- "CilSm"
CrGrps["group_size"][CrGrps["group_size"] == "cyanobacteria large"] <- "CyanoLg"
CrGrps["group_size"][CrGrps["group_size"] == "cyanobacteria small"] <- "CyanoSm"
CrGrps["group_size"][CrGrps["group_size"] == "dinoflagellate large"] <- "DinoLg"
CrGrps["group_size"][CrGrps["group_size"] == "dinoflagellate small"] <- "DinoSm"
CrGrps["group_size"][CrGrps["group_size"] == "flagellate large"] <- "FlagLg"
CrGrps["group_size"][CrGrps["group_size"] == "flagellate small"] <- "FlagSm"
CrGrps["group_size"][CrGrps["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
CrGrps["group_size"][CrGrps["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
CrGrps["group_size"][CrGrps["group_size"] == "unidentified large"] <- "UnidLg"
CrGrps["group_size"][CrGrps["group_size"] == "unidentified small"] <- "UnidSm"

save(CrGrps, file =  "data/Clearance Rates/CrGrps.Rdata")
save(CrGrps, file = "Notes/CR_Groups/CrGrps.Rdata")
write_xlsx(CrGrps, "data/Clearance Rates/CrGrps.xlsx")

### Do the same for the Clearance Rates df that includes the reps
CrGrpsReps <- CR_Rep_Mn 
## Make a new column named "group_size" that combines the names of the group column
##   and the size column
CrGrpsReps$group_size <- (paste(CrGrpsReps$group, CrGrpsReps$size))

CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "centricDiatom large"] <- "CenDiaLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "centricDiatom small"] <- "CenDiaSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "chlorophyte large"] <- "ChlLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "chlorophyte small"] <- "ChlSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "ciliate large"] <- "CilLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "ciliate small"] <- "CilSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "cyanobacteria large"] <- "CyanoLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "cyanobacteria small"] <- "CyanoSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "dinoflagellate large"] <- "DinoLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "dinoflagellate small"] <- "DinoSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "flagellate large"] <- "FlagLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "flagellate small"] <- "FlagSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "unidentified large"] <- "UnidLg"
CrGrpsReps["group_size"][CrGrpsReps["group_size"] == "unidentified small"] <- "UnidSm"

save(CrGrpsReps, file =  "data/Clearance Rates/CrGrpsReps.Rdata")
save(CrGrpsReps, file = "Notes/CR_Groups/CrGrpsReps.Rdata")
write_xlsx(CrGrpsReps, "data/Clearance Rates/CrGrpsReps.xlsx")
load("data/Clearance Rates/CrGrpsReps.Rdata")

### _________INGESTION (FEEDING) RATE CELLS (COUNTS) ______________________________

load("data/Clearance Rates/sumAll.Rdata")
## Create the base data frame that has only the initial samples
sumAllCpm_I <- sumAll %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, size, cpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumAllCpm_Imn <- sumAllCpm_I %>% 
  group_by(samp_ev, Group, size) %>%
  summarize(Imn = mean(cpm)) %>% 
            #n=length(cpm)) %>% 
  ungroup

### Remove unneeded columns, and rename and re-order remaining columns
names(sumAllCpm_Imn)
sumAllCpm_Imn <- select(sumAllCpm_Imn, 
                     event = samp_ev,
                     #sample = exp,
                     group = Group,
                     size, Imn)

### Join sumAllCpm_Imn with the df that has clearance rates, sumAllCpm_cr
load("data/Clearance Rates/sumAllCpm_cr.Rdata")
### Note, 2/19/23 I had a problem with the code below where it added too many rows, and
##  on 2/20/23 I figured out it was because I was including "sample" in the join by 
##  argument. That doesn't work since the CR df has the E samples, and the Imn df has
##  the initial samples, so R can't add the Imn column to the end of the columns since
##  the samples are different. I removed "sample" from the join by argument and it worked.
names(sumAllCpm_cr)
names(sumAllCpm_Imn)
sumAllCpm_cr_fr <- 	left_join(sumAllCpm_cr, sumAllCpm_Imn, 
                        by = c("event", "group", 
                                "size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumAllCpm_cr_fr)
sumAllCpm_cr_fr <- select(sumAllCpm_cr_fr, 
                    event, group,
                    size, Imn, CR)


### Do a little test to see how it calculates FR when I use a df that has just the
##  mean CR
TestCpm_cr_fr <- 	left_join(crmnsize_sumAllCpm_cr, sumAllCpm_Imn, 
                              by = c("event", "group", 
                                     "size"))
TestCpmFr <- rowwise(TestCpm_cr_fr) %>% 
  mutate(FR = fr_func(CR=crMn, initialMnCt = Imn))
### Answer to above test: it's the same either way

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumAllCpm_fr <- rowwise(sumAllCpm_cr_fr) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imn))
save(sumAllCpm_fr, file = "data/Clearance Rates/Feeding Rates/sumAllCpm_fr.Rdata")
write_xlsx(sumAllCpm_fr, "data/Clearance Rates/Feeding Rates/sumAllCpm_fr.xlsx")

load("data/Clearance Rates/Feeding Rates/sumAllCpm_fr.Rdata")

### Take the mean FR for group and size, i.e., centricDiatoms large in LSZ2
CpmFrMn <- sumAllCpm_fr %>%
  group_by(event, group, size) %>%
  summarise(FRmn = mean(FR))
### Rename the FRmn column to say FRmnCpm so that when I combine this df
##  with the Bpm FR, I know which column is which
CpmFrMn <- CpmFrMn %>% 
  rename("FRmnCpm" = "FRmn")
save(CpmFrMn, file = "data/Clearance Rates/Feeding Rates/CpmFrMn.Rdata")
write_xlsx(CpmFrMn, "data/Clearance Rates/Feeding Rates/CpmFrMn.xlsx")
load("data/Clearance Rates/Feeding Rates/CpmFrMn.Rdata")

### Remove the rows where FR is 0.
sumAllCpm_fr_no0 <- sumAllCpm_fr[sumAllCpm_fr$FR != 0,]
save(sumAllCpm_fr_no0, file = "data/Clearance Rates/Feeding Rates/sumAllCpm_fr_no0.Rdata")
write_xlsx(sumAllCpm_fr_no0, "data/Clearance Rates/Feeding Rates/sumAllCpm_fr_no0.xlsx")

### A test to see what unique group/types this df has
sumAllGrpNames <- select(sumAllCpm_fr_noNA, group, type)
duplicated(sumAllGrpNames)
sumAllGrpNames[!duplicated(sumAllGrpNames)]
sumAllGrpNames <- unique(sumAllGrpNames)
write_xlsx(sumAllGrpNames, "data/Clearance Rates/Feeding Rates/sumAllGrpNames.xlsx")

### ______ Make a df that combines rep FR (cells) with mean FR (cells) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
### This still needs work as of 4/4/23, because I think I need the rep numbers
names(sumAllCpm_fr)
names(CpmFrMn)
FRcpm_Rep_Mn <- 	left_join(sumAllCpm_fr, CpmFrMn, 
                        by = c("event", "group", "size"))
save(FRcpm_Rep_Mn, file= "data/Clearance Rates/FRcpm_Rep_Mn.Rdata")

### ____________ ADD THE GROUP_SIZE ABBREVIATIONS COLUMN _________________
###_______________________________________________________________________
FrGrpsRepsCells <- FRcpm_Rep_Mn 

## Make a new column named "group_size" that combines the names of the group column
##   and the size column
FrGrpsRepsCells$group_size <- (paste(FrGrpsRepsCells$group, FrGrpsRepsCells$size))

FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "centricDiatom large"] <- "CenDiaLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "centricDiatom small"] <- "CenDiaSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "chlorophyte large"] <- "ChlLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "chlorophyte small"] <- "ChlSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "ciliate large"] <- "CilLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "ciliate small"] <- "CilSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "cyanobacteria large"] <- "CyanoLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "cyanobacteria small"] <- "CyanoSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "dinoflagellate large"] <- "DinoLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "dinoflagellate small"] <- "DinoSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "flagellate large"] <- "FlagLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "flagellate small"] <- "FlagSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "unidentified large"] <- "UnidLg"
FrGrpsRepsCells["group_size"][FrGrpsRepsCells["group_size"] == "unidentified small"] <- "UnidSm"

save(FrGrpsRepsCells, file =  "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCells.Rdata")
save(FrGrpsRepsCells, file = "Notes/FR_Groups/FrGrpsRepsCells.Rdata")
write_xlsx(FrGrpsRepsCells, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCells.xlsx")

### _________INGESTION (FEEDING) RATE BY BIOMASS ______________________________

## Create the base data frame that has only the initial samples
load("data/Clearance Rates/sumAll.Rdata")
sumAllBpm_I <- sumAll %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, Group, size, bio_ugC_l)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumAllBpm_Imn <- sumAllBpm_I %>% 
  group_by(samp_ev, exp, Group, size) %>%
  summarize(ImnBpL = mean(bio_ugC_l)) %>% 
  #n=length(bpm)) %>% 
  ungroup

### Remove unneeded columns, and rename and re-order remaining columns
names(sumAllBpm_Imn)
sumAllBpm_Imn <- select(sumAllBpm_Imn, 
                        event = samp_ev,
                        sample = exp,
                        group = Group,
                        size, ImnBpL)

### Join sumAllBpm_Imn with the df that has clearance rates, sumAllBpm_cr
### Join sumAllCpm_Imn with the df that has clearance rates, sumAllCpm_cr
load("data/Clearance Rates/sumAllCpm_cr.Rdata")
### Note, 2/19/23 I had a problem with the code below where it added too many rows, and
##  on 2/20/23 I figured out it was because I was including "sample" in the join by 
##  argument. That doesn't work since the CR df has the E samples, and the Imn df has
##  the initial samples, so R can't add the Imn column to the end of the columns since
##  the samples are different. I removed "sample" from the join by argument and it worked.
sumAllBpm_cr_fr <- 	left_join(sumAllCpm_cr, sumAllBpm_Imn, 
                              by = c("event", "group", 
                                      "size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumAllBpm_cr_fr)
sumAllBpm_cr_fr <- select(sumAllBpm_cr_fr, 
                          event, group,
                          size, ImnBpL, CR)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumAllBpm_fr <- rowwise(sumAllBpm_cr_fr) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = ImnBpL))
save(sumAllBpm_fr, file = "data/Clearance Rates/Feeding Rates/sumAllBpm_fr.Rdata")
write_xlsx(sumAllBpm_fr, "data/Clearance Rates/Feeding Rates/sumAllBpm_fr.xlsx")
load("data/Clearance Rates/Feeding Rates/sumAllBpm_fr.Rdata")

### Calculate the means across the groups and sizes
BpmFrMn <- sumAllBpm_fr %>%
  group_by(event, group, size) %>%
  summarise(FRmn = mean(FR))
### Rename the FRmn column to say FRmnBpm so that when I combine this df
##  with the Cpm FR, I know which column is which
BpmFrMn <- BpmFrMn %>% 
  rename("FRmnBpm" = "FRmn")
save(BpmFrMn, file = "data/Clearance Rates/Feeding Rates/BpmFrMn.Rdata")
write_xlsx(BpmFrMn, "data/Clearance Rates/Feeding Rates/BpmFrMn.xlsx")

### Remove the rows where FR is 0.
sumAllBpm_fr_no0 <- sumAllBpm_fr[sumAllBpm_fr$FR != 0,]
save(sumAllBpm_fr_no0, file = "data/Clearance Rates/Feeding Rates/sumAllBpm_fr_no0.Rdata")
write_xlsx(sumAllBpm_fr_no0, "data/Clearance Rates/Feeding Rates/sumAllBpm_fr_no0.xlsx")

### ______ Make a df that combines rep FR (biomass) with mean FR (biomass) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
load("data/Clearance Rates/Feeding Rates/BpmFrMn.Rdata")
load("data/Clearance Rates/Feeding Rates/sumAllBpm_fr.Rdata")
names(sumAllCpm_fr)
names(BpmFrMn)
FRbpm_Rep_Mn <- 	left_join(sumAllBpm_fr, BpmFrMn, 
                           by = c("event", "group", "size"))
save(FRbpm_Rep_Mn, file= "data/Clearance Rates/FRbpm_Rep_Mn.Rdata")

### ____________ ADD THE GROUP_SIZE ABBREVIATIONS COLUMN _________________
###_______________________________________________________________________
FrGrpsRepsBio <- FRbpm_Rep_Mn 

## Make a new column named "group_size" that combines the names of the group column
##   and the size column
FrGrpsRepsBio$group_size <- (paste(FrGrpsRepsBio$group, FrGrpsRepsBio$size))

FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "centricDiatom large"] <- "CenDiaLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "centricDiatom small"] <- "CenDiaSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "chlorophyte large"] <- "ChlLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "chlorophyte small"] <- "ChlSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "ciliate large"] <- "CilLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "ciliate small"] <- "CilSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "cyanobacteria large"] <- "CyanoLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "cyanobacteria small"] <- "CyanoSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "dinoflagellate large"] <- "DinoLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "dinoflagellate small"] <- "DinoSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "flagellate large"] <- "FlagLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "flagellate small"] <- "FlagSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "unidentified large"] <- "UnidLg"
FrGrpsRepsBio["group_size"][FrGrpsRepsBio["group_size"] == "unidentified small"] <- "UnidSm"

save(FrGrpsRepsBio, file =  "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBio.Rdata")
save(FrGrpsRepsBio, file = "Notes/FR_Groups/FrGrpsRepsBio.Rdata")
write_xlsx(FrGrpsRepsBio, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBio.xlsx")


### __________ CLEARANCE RATE CPM AND BPL IN THE SAME DF _________________
###  This is no longer needed, since I wasn't supposed to calculate clearance 
##  rates using biomass, only using cells per mL. But I want to keep the code
## for now in case I need the CrGrps code.

##  BPL is biomass in µgC per Liter
#load("data/Clearance Rates/crmnsize_sumAllCpm_cr.Rdata")
#load("data/Clearance Rates/crmnsize_sumAllBpm_cr.Rdata")
#CRcpmBpl <- left_join(crmnsize_sumAllCpm_cr, crmnsize_sumAllBpm_cr,
 #                     by = c("event", "group", "size"))




###______________ INGESTION RATE CPM and BPM IN THE SAME DF __________________________
##________________________________________________________________________
load("data/Clearance Rates/Feeding Rates/CpmFrMn.Rdata")
load("data/Clearance Rates/Feeding Rates/BpmFrMn.Rdata")
FRcpmBpm <-left_join(CpmFrMn, BpmFrMn,
                     by = c("event", "group", "size"))

### ______________ADD A COLUMN OF BIOMASS IN µg C per liter________________
##  1 pg mL-1     (1 pgC)/(1 ml) * (1000 ml)/(1 L) * (0.000001 µg)/(1 pgC) =  0.001 µg L-1
FRcpmBpm <- FRcpmBpm %>% 
  group_by(event, group, size, FRmnCpm, FRmnBpm) %>%
  summarize(FRmnBul = FRmnBpm*.001) # FrBul = feeding rate, in µg C per Liter
save(FRcpmBpm, file = "data/Clearance Rates/Feeding Rates/FRcpmBpm.Rdata")
write_xlsx(FRcpmBpm, "data/Clearance Rates/Feeding Rates/FRcpmBpm.xlsx")


#### _____________MAIN GROUPS/SIZES BY EVENT ____________________
load("data/Clearance Rates/Feeding Rates/FRcpmBpm.Rdata")
### Make a table of feeding rate for the main groups across all events
### see 04_plots_FRgroups.R and 
load("data/Clearance Rates/Feeding Rates/FrGrps.Rdata")

FrGrps <- FRcpmBpm 

## Make a new column named "group_size" that combines the names of the group column
##   and the size column
FrGrps$group_size <- (paste(FrGrps$group, FrGrps$size))

FrGrps["group_size"][FrGrps["group_size"] == "centricDiatom large"] <- "CenDiaLg"
FrGrps["group_size"][FrGrps["group_size"] == "centricDiatom small"] <- "CenDiaSm"
FrGrps["group_size"][FrGrps["group_size"] == "chlorophyte large"] <- "ChlLg"
FrGrps["group_size"][FrGrps["group_size"] == "chlorophyte small"] <- "ChlSm"
FrGrps["group_size"][FrGrps["group_size"] == "ciliate large"] <- "CilLg"
FrGrps["group_size"][FrGrps["group_size"] == "ciliate small"] <- "CilSm"
FrGrps["group_size"][FrGrps["group_size"] == "cyanobacteria large"] <- "CyanoLg"
FrGrps["group_size"][FrGrps["group_size"] == "cyanobacteria small"] <- "CyanoSm"
FrGrps["group_size"][FrGrps["group_size"] == "dinoflagellate large"] <- "DinoLg"
FrGrps["group_size"][FrGrps["group_size"] == "dinoflagellate small"] <- "DinoSm"
FrGrps["group_size"][FrGrps["group_size"] == "flagellate large"] <- "FlagLg"
FrGrps["group_size"][FrGrps["group_size"] == "flagellate small"] <- "FlagSm"
FrGrps["group_size"][FrGrps["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
FrGrps["group_size"][FrGrps["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
FrGrps["group_size"][FrGrps["group_size"] == "unidentified large"] <- "UnidLg"
FrGrps["group_size"][FrGrps["group_size"] == "unidentified small"] <- "UnidSm"

save(FrGrps, file =  "data/Clearance Rates/Feeding Rates/FrGrps.Rdata")
save(FrGrps, file = "Notes/FR_Groups/FrGrps.Rdata")
write_xlsx(FrGrps, "data/Clearance Rates/Feeding Rates/FrGrps.xlsx")

### Means of cells per mL
FrCpmOverall <- FrGrps %>%
  group_by(group_size) %>%
  summarise( FrCpmAllEvents= mean(FRmnCpm))
save(FrCpmOverall, file = "data/Clearance Rates/Feeding Rates/FrCpmOverall.Rdata")
write_xlsx(FrCpmOverall, "data/Clearance Rates/Feeding Rates/FrCpmOverall.xlsx")

### Means of biomass in pgC per mL
FrBpmAllMn <- FrGrps%>%
  group_by(group_size) %>%
  summarise(FrBpmMnAllEvents= mean(FRmnBpm))
save(FrBpmAllMn, file = "data/Clearance Rates/Feeding Rates/FrBpmAllMn.Rdata")
write_xlsx(FrBpmAllMn, "data/Clearance Rates/Feeding Rates/FrBpmAllMn.xlsx")

### Means of biomass in ug per L
FrBugAllMn <-FrGrps%>%
  group_by(group_size) %>%
  summarise(FrBugAllMn= mean(FrBpm_ug))
save(FrBugAllMn, file = "data/Clearance Rates/Feeding Rates/FrBugAllMn.Rdata")
write_xlsx(FrBugAllMn, "data/Clearance Rates/Feeding Rates/FrBugAllMn.xlsx")

FrAllOverall <- left_join(FrBpmOverall, FrCpmOverall)
save(FrAllOverall, file = "data/Clearance Rates/Feeding Rates/FrAllOverall.Rdata")
write_xlsx(FrAllOverall, "data/Clearance Rates/Feeding Rates/FrAllOverall.xlsx")

### Add a column that has biomass in µg C per liter
load("data/Clearance Rates/Feeding Rates/FrAllOverall.Rdata")
FrAllOverall_ug <- FrAllOverall %>% 
  group_by(group_size, FrCpmAllEvents, FrBpmAllEvents) %>%
  summarize(FrBpm_ug = FrBpmAllEvents*.001)
save(FrAllOverall_ug, file = "data/Clearance Rates/Feeding Rates/FrAllOverall_ug.Rdata")
load("data/Clearance Rates/Feeding Rates/FrAllOverall_ug.Rdata")
### ______________________ OTHER CODE TRIALS____________________

### Note: use cumsum()? example:
##  df %>%  group_by(the columns that identify the organisms) %>% 
##  mutate (new column name = cumsum(cpm or bpm))

### Test below with just centric diatoms, since they are numerous enough that
##  I would have sufficient counts to make a comparison

size_test <- volbio_all_cr
size_test_tinLSZ2<- size_test %>% 
  filter(samp_ev == "LSZ2", Group == "tintinnid", size == "large") %>% 
  select(samp_ev, exp, rep, Group, type,  
         counts, cpm, size)# %>% 
#subset(cpm !=0)
### add the cpm of all pendia
size_test_pendia2 <- size_test_pendia %>% 
  #group_by(size) %>% 
  aggregate(cpm ~ samp_ev+exp+rep+size, FUN = sum)
#mutate(cpm_by_size = cumsum(cpm)) 

### Note about above: I learned that when using aggregate,
##  group_by is not needed

### Now use a base data frame with all the Groups and add Group
##  to the argument of what columns to match when adding



