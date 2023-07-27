############################################################################
##### CLEARANCE RATES TEST WITH CENTRIC DIATOMS, SJR1 SINGLE EXP ONLY #####
###########################################################################
### 12/21/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
load("data/Clearance Rates/taxaCen.Rdata") # created in 03_calcs_CR_test_cendia.R
source("scripts/01_function_clearanceRates.R")
### source("scripts/01_function_wimGraph and Palettes.R")

### Re-ran on 1/9/23 Got error codes: Error in `mutate()`:
# ! Problem while computing `CR = cr_func(controlMnCt = Cmn, expCt = cpm)`.
# Caused by error:
#  ! `CR` must return compatible vectors across groups.

### AND

#Error in `mutate()`:
#  ! Problem while computing `CR = cr_func(controlMnCt = Cmn, expCt = cpm)`.
# Caused by error:
#  ! `CR` must return compatible vectors across groups.
#ℹ Result type for group 1 (group = "diatom centric 14 14", rep = "1"): <formattable>.
#ℹ Result type for group 7 (group = "diatom centric 32 32", rep = "1"): <double>.

### Fixed it by re-doing taxaCen.Rdata without using "formattable" to reduce
## the decimal places to two. That seemed to change the type of vector or number
## it was, causing the error in using the CR function.


### Cleaned up 1/8/23

### Using the base file taxaCen, filter out everything but SJR1
taxaCenSJR1 <- filter(taxaCen, grepl('SJR1', samp_ev))

## Remove the esd column 
taxaCenSJR1 <-select(taxaCenSJR1, samp_ev, exp, rep, grp_sz,  
                                 counts_per_ml)

### Create a df with only the controls and a df with only the
##  experimentals. Calculate the mean counts of the controls, then 
## add that df to the experimentals df using left join or merge. 
## This new df can then be used with the clearance rate function

### Controls
taxaCenSRJ1C <- 	taxaCenSJR1[(taxaCenSJR1$exp== "FC"),]

### Experimentals
taxaCenSRJ1E <- 	taxaCenSJR1[(taxaCenSJR1$exp== "T24"),]

### Calculate the  mean counts of the controls  
##  "Cmn" in the mutate function indicates the Control (formerly FC) mean

taxaCenSRJ1Cmn <- 	group_by(taxaCenSRJ1C, grp_sz) %>% 
  mutate(Cmn = mean(counts_per_ml)) %>% 
  subset(select = -counts_per_ml) %>%
  ungroup

### Remove the rep column and then remove duplicate rows
taxaCenSRJ1Cmn2 <- 	select(taxaCenSRJ1Cmn, -rep)

## looks at what rows are duplicated
duplicated(taxaCenSRJ1Cmn2)  

## removes duplicated rows
taxaCenSRJ1Cmn2 <- taxaCenSRJ1Cmn2 [!duplicated(taxaCenSRJ1Cmn2), ]

### Join taxaCenSRJ1C and taxaCenSRJ1Cmn2

taxaCenSJR1CE <- 	left_join(taxaCenSRJ1E, taxaCenSRJ1Cmn2, by = "grp_sz")

### Remove unneeded columns
taxaCenSJR1CE <- 	subset(taxaCenSJR1CE, select = c(-samp_ev.y, -exp.y))

### Rename and re-order columns
taxaCenSJR1CE <- select(taxaCenSJR1CE, 
                        event = samp_ev.x,
                        sample = exp.x,
                        rep,
                        group = grp_sz,
                        cpm = counts_per_ml,
                        Cmn)

### Rename T24 to exp
taxaCenSJR1CE$sample <-  replace(taxaCenSJR1CE$sample,taxaCenSJR1CE$sample=="T24","exp")

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

sjr1Cr <- rowwise(taxaCenSJR1CE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))

#__________ Below is the orginally written code, above is updated as pf 1/8/23 _______
load("data/Clearance Rates/taxaCen.Rdata")

### Using the base file taxaCen, filter out everything but SJR1
taxaCenSJR1 <- filter(taxaCen, grepl('SJR1', samp_ev))

## Remove the esd column 
taxaCenSJR1 <-select(taxaCenSJR1, samp_ev, exp, rep, grp_sz,  
                     counts_per_ml)
### Keep only FC and T24, which means remove site and IC
##  taxaCenSJR1exp <- taxaCenSJR1[!(taxaCenSJR1$exp=="site" | taxaCenSJR1$exp=="IC"),]

### Update 1/4/23: Create a df with only the controls and a df with only the
##  experimentals. Calculate the mean counts of the controls, then add
##  that df to the experimentals df using left join or merge. This new df
##  can then be used with the clearance rate function

### Controls
taxaCenSRJ1C <- taxaCenSJR1[(taxaCenSJR1$exp== "FC"),]

### Experimentals
taxaCenSRJ1E <- taxaCenSJR1[(taxaCenSJR1$exp== "T24"),]

### Calculate the  mean counts of the controls  
##  "Cmn" in the mutate function indicates the Control (formerly FC) mean
taxaCenSRJ1Cmn <- group_by(taxaCenSRJ1C, grp_sz) %>% 
  mutate(Cmn = mean(counts_per_ml)) %>% 
  subset(select = -counts_per_ml) %>%
  ungroup

### Get rid of the second and third reps
taxaCenSRJ1Cmn1 <- subset(taxaCenSRJ1Cmn, rep!= "2" & rep!= "3")

### Or instead of above, remove the rep column and then remove duplicate rows
taxaCenSRJ1Cmn2 <- subset(taxaCenSRJ1Cmn, select = -rep)
duplicated(taxaCenSRJ1Cmn2)  ## looks at what rows are duplicated
taxaCenSRJ1Cmn2 <- taxaCenSRJ1Cmn2[!duplicated(taxaCenSRJ1Cmn2), ] ## removes duplicated rows

### Join taxaCenSRJ1C and taxaCenSRJ1Cmn2
taxaCenSJR1CE <- left_join(taxaCenSRJ1E, taxaCenSRJ1Cmn2, by = "grp_sz")
taxaCenSJR1CE <- subset(taxaCenSJR1CE, select = c(-samp_ev.y, -exp.y))
taxaCenSJR1CE <- taxaCenSJR1CE %>% 
  rename("event" = "samp_ev.x",
         "sample" = "exp.x",
         "group" = "grp_sz",
         "cpm" = "counts_per_ml")
taxaCenSJR1CE$sample <-  replace(taxaCenSJR1CE$sample,taxaCenSJR1CE$sample=="T24","exp")

### Reorder the columns
taxaCenSJR1CE <- taxaCenSJR1CE[, c(1,4,2,3,5,6)]
 

### Calculate the  mean FC (Controls) while keeping all the T24s (experimentals). 
##  The letter "C" in the mutate function indicates the Control (formerly FC) mean
#taxaCenSJR1_Mn <- group_by(taxaCenSJR1exp, samp_ev, exp, grp_sz) %>%  # for each combination of 
                                                                      # these variables
  #mutate(C = ifelse(exp=="FC", ## if the experiment column's row is FC
  #                  mean(counts_per_ml), NA)) %>% ## then calculate the mean, if not, return NA
  #ungroup

write_xlsx(taxaCenSJR1_Mn, "data/Mean Counts/taxaCenSJR1_Mn.xlsx")
save(taxaCenSJR1_Mn, file = "data/Clearance Rates/taxaCenSJR1_Mn.Rdata")

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

#function(controlMnCt, expCt, numBugs=24, V=595) {
#  cr = V * ((log(controlMnCt) - log(expCt))/numBugs)
#  if(cr == 0){
#    return(NA)
#  } else{
#    return(cr)
#  }
#}


### 1/4/23 Figure out how to apply clearance rate calculation
##  Clearance rate equation is: 595 ml * ((ln(controlMnCt) - ln(expCt))/24), where
##  595 ml is the volume of the experimental container, they were 500ml bottle, filled
##  all the way to the top, which made them 595ml.
##  ControlMnCt is the mean count of the organism across all three replicates of the 
##    controls sample, referred to previously as FC, now called C
##  expCt is the raw counts of the organism in the experimentl sample, called T24
##  24 is the number of copepods that were grazing in the samples, 24 for all samples

sjr1Cr <- rowwise(taxaCenSJR1CE) %>% 
mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))

### Replace the NAs in Cmn with zeros
sjr1Cr <- sjr1Cr %>% 
  mutate_at(7, ~replace_na(.,0))

write_xlsx(sjr1Cr, "data/Clearance Rates/sjr1Cr.xlsx")

sjr1Cr1 <- group_by(taxaCenSJR1CE, group, rep) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))


### Test the clearance rate function

c_mean <- c(10,20,50)
ct_exp <- c(5, 12, 15)
df <- data.frame(c_mean, ct_exp)
dfcrtest <- rowwise(df) %>% 
  mutate(CR = cr_func(controlMnCt = c_mean, expCt = ct_exp))
