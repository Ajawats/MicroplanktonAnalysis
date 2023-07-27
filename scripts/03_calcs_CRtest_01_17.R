############################################################################
########################## CR FUNCTION TESTING #############################
############################################################################
### 1/17/23
### This is for finding out how to run the code I just wrote in 
##  03_calcs_CtrlMn_Cr.R with any group of organisms and sampling events
### see 03_calcs_GroupsCollapse.R for references and code for all groups 
### 1/18/23
##  It seems to work fine with the diatom info. Now try with the ciliate cone info

library(tidyverse)
library(writexl)
#load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
### Use this file below instead of volbio_all becasuse volbio_all_cr has
##  the experimental name changes FC=C, T24=E, IC=I, site=S
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
### source("scripts/01_function_wimGraph and Palettes.R")

## From the website above:
#surveys %>%
#  filter(taxa == "Rodent",
#         !is.na(weight)) %>%
#  group_by(sex,genus) %>%
#  summarize(mean_weight = mean(weight))


### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates
names(volbio_all_cr)
unique(volbio_all_cr$Group)
unique(volbio_all_cr$grp_typ)

diacen <- volbio_all_cr %>%
  filter(Group == "diatom",
         type == "centric",
         str_detect(exp, "C|E"),
         samp_ev =="SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

diacenC <- diacen %>% 
  filter(exp == "C")

diacenCmn <- diacenC %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml),
            n=length(counts_per_ml))

#table(diacenCmn$n)

### Make a df with only centric diatoms experimental samples
diacenE <- diacen %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz)

### Join the above df with the df that has the T24 (experimental) counts
## per ml
diacenCE <- 	left_join(diacenE, diacenCmn, by = "grp_sz")

names(diacenCE)
### Remove unneeded columns
diacenCE <- 	subset(diacenCE, select = c(-samp_ev.y, -exp.y))


### Rename and re-order columns
diacenCE <- select(diacenCE, 
                          event = samp_ev.x,
                          sample = exp.x,
                          rep,
                          group = grp_sz,
                          cpm = counts_per_ml,
                          Cmn)

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

diacenCR <- rowwise(diacenCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))



###__________________USING CILIATE CONE DATA_______________________________
### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates
names(volbio_all_cr)
unique(volbio_all_cr$Group)
unique(volbio_all_cr$grp_typ)

cilcon <- volbio_all_cr %>%
  filter(Group == "ciliate",
         type == "cone",
         str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

cilconC <- cilcon %>% 
  filter(exp == "C") 

cilconCmn <- cilconC %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Make a df with only centric diatoms experimental samples
cilconE <- cilcon %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz)
 
### Join the above df with the df that has the T24 (experimental) counts
## per ml
cilconCE <- 	left_join(cilconE, cilconCmn, by = c("samp_ev", "grp_sz"))

 names(cilconCE)

### Rename and re-order columns
cilconCE <- select(cilconCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

cilconCR <- rowwise(cilconCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))



###__________________USING OTHER DATA_______________________________
### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates

### Start with new base df, volbio_all_cr, that already has the experiment
## sample names changed:  T24 to E (for experimental), FC to C (for control),
##  IC to I, (for initials), site to S (for site)

load("data/Clearance Rates/volbio_all_cr.Rdata")

### Try with pennate diatoms
diapen <- volbio_all_cr %>%
  filter(Group == "diatom",
         type == "pennate",
         str_detect(exp, "C|E"),
         samp_ev =="SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

### Change the pennate diatom names
## diapen$grp_sz <-  replace(diapen$grp_sz,diapen$grp_sz=="diatom pennate 2 20","diapen_2_20")

diapenC <- diapen %>% 
  filter(exp == "C")
  
diapenCmn <- diapenC %>% 
  group_by(samp_ev, exp, grp_sz)%>%
  summarize(Cmn = mean(counts_per_ml))
#write_xlsx(diapenC, "data/Clearance Rates/diapenC.xlsx")

### Make a df with only centric diatoms experimental samples
diapenE <- diapen %>% 
  filter(exp == "E")
#write_xlsx(diapenE, "data/Clearance Rates/diapenE.xlsx")

### Join the above df with the df that has the T24 (experimental) counts
## per ml
diapenCE <- 	left_join(diapenE, diapenCmn, by = "grp_sz")

### Rename and re-order columns
names(diapenCR)
diapenCR <- select(diapenCE, 
                  event = samp_ev.x,
                  sample = exp.x,
                  rep,
                  group = grp_sz,
                  cpm = counts_per_ml,
                  Cmn)


### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

diapenSJR1cr <- rowwise(diapenCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))

unique(diapenSJR1cr$group)

##________________ TRY WITH MULTIPLE SAMPLING EVENTS ____________________##

unique(volbio_all_cr$grp_typ)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
## Flagellates
### Filter out the organism group and sampling events desired, and keep only
##  controls and experimentals

flag <- volbio_all_cr %>%
  filter(Group == "flagellate",
         type == "cryptomonas",
         str_detect(exp, "C|E"))%>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

### Make a df with only experimental samples
flag_E <- flag %>% 
  filter(exp == "E")

### Make a df for just the controls
flag_C <- flag %>% 
  filter(exp == "C")

### Make a df with that calculates the means of the control counts
## per ml across the three replicates
flag_Cmn <-flag_C %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Join the df of control means with the df of the experimental counts
## per ml
flagCE <- 	left_join(flag_E, flag_Cmn, by = c("samp_ev", "grp_sz"))

names(flagCE)

### Rename and re-order columns
flagCE <- select(flagCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)


### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

flagCR <- rowwise(flagCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
unique(flagCR$group)
write_xlsx(flagCR, "data/Clearance Rates/flagCR.xlsx")
