####################################################################
########### CLEARANCE RATES TEST WITH CENTRIC DIATOMS ##############
####################################################################
### 12/21/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

### Using the base file volbio_all, select the columns needed
### IMPORTANT NOTE!!!:  Use the grp_sz column for selecting the organisms to calculate the
##  mean counts for, or some other column, but DO NOT use szesd. esd is calculated based on 
##  volume per organism, which is based on the counts, so if there were 0 counts, there would 
##  then be no esd. Although their dimensions and esd would still be equivalent, the szesd name 
##  would have NaN in place of the esd size, so the names for identical organisms would not be 
##  the same, and R would not include all three reps in the means calculations.So the solution 
##  is to use the grp_sz column for calculating the means, rather than the szesd column.

### Also, be sure to include the zero counts, since those are needed to calculate
##  the true means of the three replicates.

taxaCen <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, Group,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml))

### Filter out everything but centric diatoms
taxaCen <- filter(taxaCen, grepl('centric', grp_sz))
#taxaCen <- subset(taxaCen,counts_per_ml !=0)

### Remove the columns that are now not needed
taxaCen <- subset(taxaCen, 
                  select = c(samp_ev, exp, rep, grp_sz, esd,  
                             counts_per_ml))

### Reduce counts per milliliter to 2 digits
### Don't do this, to see if it corrects the error code when running the
##  clearance rate function
#taxaCen$counts_per_ml<-formattable(taxaCen$counts_per_ml,
 #                                  format="f",digits=2)
#taxaCen$bio_per_vol_pgc_ml<-formattable(taxaCen$bio_per_vol_pgc_ml,
                                       # format="f",digits=2)
save(taxaCen, file = "data/Clearance Rates/taxaCen.Rdata")

### Filter out experimentals to only FC and T24 since I don't need site or IC for
##  the clearance rate calculations
taxaCen2 <- filter(taxaCen, exp=='FC' | exp == 'T24')
### Remove the esd column
taxaCen2 <- subset(taxaCen2, select = c(samp_ev, exp, rep, grp_sz, counts_per_ml))

### Calculate the means of the FCs only because I don't need the means of the
## experimentals when calculating the clearance rates

## Here's where I got the code idea: https://stackoverflow.com/questions/48075533/
##  r-add-another-column-with-the-sum-of-a-column-if-all-other-are-the-same

## Add the argument to calculate the mean counts per ml for each of the FC 
## samples across the 3 replicates
 

### Fixed the above, and used just SJR1
### Using the base file taxaCen, filter out everything but SJR1

##_____ 1/12/23 Don't do this here anymore because the better
##  version is in 03_calcs_CR_test_cendia_SJR1.R

#taxaCenSJR1 <- filter(taxaCen, grepl('SJR1', samp_ev))
## Remove the esd column 
#taxaCenSJR1 <- subset(taxaCenSJR1, 
#                      select = c(samp_ev, exp, rep, grp_sz,  
#                                 counts_per_ml))
### Filter all but FC
#taxaCenSJR1FC <- filter(taxaCenSJR1, grepl('FC', exp))

### Calculate the means of the FCs only.
## Here's where I got the code idea: https://stackoverflow.com/questions/48075533/
##  r-add-another-column-with-the-sum-of-a-column-if-all-other-are-the-same
## In 03_calcs_CR_test_cendia_SJR1.R, I remved the n() >1 line, since it seemed 
##  unneccesary, and it seeemd to run fine.

## Use  03_calcs_CR_test_cendia_SJR1.R for the best example of getting the means of FCs
## while retaining the T24s.
taxaCenSJR1FCMn <- group_by(taxaCenSJR1FC, samp_ev, exp, grp_sz) %>%  # for each combination of these variables
  mutate(C = ifelse(n() >1 & # if there are multiple rows
                      exp=="FC", ## and the experiment is FC
                    mean(counts_per_ml), NA)) %>% 
  ungroup

### Create a df for T24 but don't calcualte means since the clearance rate calculations
##  use the mean FC, but the individual T24 experimentals
taxaCenSJR1T24 <- filter(taxaCenSJR1, grepl('T24', exp))

### Join FC and T24 together
taxaCenMnSRJ1exp <-merge(taxaCenSJR1FCMn, taxaCenSJR1T24, by= "grp_sz")
write_xlsx(taxaCenMnSRJ1exp, "data/Mean Counts/taxaCenMnSRJ1exp.xlsx")

