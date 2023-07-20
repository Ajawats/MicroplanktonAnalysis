############################################################################
########## SJR1 CR FUNCTION TESTING USING SPLIT APPLY COMBINE ##############
############################################################################
### 1/17/23
### This is for finding out how to tell R to calculate the means of the controls
## counts per ml, and add them to a column of the experimentals, so that 
## the Cmn (control mean counts per ml) appears next to the experimental
## counts per ml. Wim suggested using this: 
## https://pages.stat.wisc.edu/~yandell/R_for_data_sciences/curate/tidyverse.html

### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
load("data/Clearance Rates/taxaCen.Rdata")
load("data/Clearance Rates/taxaCenSJR1E.Rdata")
source("scripts/01_function_clearanceRates.R")
### source("scripts/01_function_wimGraph and Palettes.R")

## From the website above:
#surveys %>%
#  filter(taxa == "Rodent",
#         !is.na(weight)) %>%
#  group_by(sex,genus) %>%
#  summarize(mean_weight = mean(weight))

### Remove the esd column from taxaCen because I don't need it right now
taxaCen <-select(taxaCen, samp_ev, exp, rep, grp_sz,  
                     counts_per_ml)

### Check the column names
names(taxaCen)

### Get only SJR1 FC, then calculate the mean counts per ml of FC across
## the three replicates
taxaCenCmn <- taxaCen %>%
  filter(exp == "FC",
         samp_ev =="SJR1") %>%
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

write_xlsx(taxaCenCmn, "data/Clearance Rates/taxaCenCmn.xlsx")

### Join the experimental cpm df with the control cpm mean df
taxaCenSJR1CmnE <- 	left_join(taxaCenSJR1E, taxaCenCmn, by = "grp_sz")

names(taxaCenSJR1CmnE)
### Rename and re-order columns
taxaCenSJR1CmnE <- select(taxaCenSJR1CmnE, 
                        event = samp_ev.x,
                        sample = exp.x,
                        rep,
                        group = grp_sz,
                        cpm = counts_per_ml,
                        Cmn)

### Rename T24 to exp
taxaCenSJR1CmnE$sample <-  replace(taxaCenSJR1CmnE$sample,taxaCenSJR1CmnE$sample=="T24","exp")
write_xlsx(taxaCenSJR1CmnE, "data/Clearance Rates/ taxaCenSJR1CmnE.xlsx")

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

sjr1Cr <- rowwise(taxaCenSJR1CmnE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
write_xlsx(sjr1Cr, "data/Clearance Rates/sjr1Cr.xlsx")
