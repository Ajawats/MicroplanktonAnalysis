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
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
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
names(volbio_all)
unique(volbio_all$Group)
unique(volbio_all$grp_typ)

cr_sjr1_data <- volbio_all %>%
  filter(Group == "diatom",
         type == "centric",
         str_detect(exp, "FC|T24"),
         samp_ev =="SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

cr_sjr1_fc <- cr_sjr1_data %>% 
  filter(exp == "FC") %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Make a df with only centric diatoms experimental samples
diacen <- volbio_all %>% 
  filter(Group == "diatom",
          type == "centric",
          exp == "T24",
          samp_ev == "SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

### Join the above df with the df that has the T24 (experimental) counts
## per ml
diacenCR <- 	left_join(diacen, cr_sjr1_fc, by = "grp_sz")

names(diacenCR)
### Remove unneeded columns
diacenCR <- 	subset(diacenCR, select = c(-samp_ev.y, -exp.y))


### Rename and re-order columns
diacenCR <- select(diacenCR, 
                          event = samp_ev.x,
                          sample = exp.x,
                          rep,
                          group = grp_sz,
                          cpm = counts_per_ml,
                          Cmn)

### Rename T24 to exp
diacenCR$sample <-  replace(diacenCR$sample,diacenCR$sample=="T24","exp")

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

diacenSJR1cr <- rowwise(diacenCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))



###__________________USING CILIATE CONE DATA_______________________________
### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates
names(volbio_all)
unique(volbio_all$Group)
unique(volbio_all$grp_typ)

cr_sjr1_cilcn <- volbio_all %>%
  filter(Group == "ciliate",
         type == "cone",
         str_detect(exp, "FC|T24"),
         samp_ev =="SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

cr_sjr1_cilcon_fc <- cr_sjr1_cilcn %>% 
  filter(exp == "FC") %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Make a df with only centric diatoms experimental samples
cilcnE <- volbio_all %>% 
  filter(Group == "ciliate",
         type == "cone",
         exp == "T24",
         samp_ev == "SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

### Join the above df with the df that has the T24 (experimental) counts
## per ml
cilcnCR <- 	left_join(cilcnE, cr_sjr1_cilcon_fc, by = "grp_sz")

names(cilcnCR)
### Remove unneeded columns
cilcnCR <- 	subset(cilcnCR, select = c(-samp_ev.y, -exp.y))


### Rename and re-order columns
cilcnCR <- select(cilcnCR, 
                   event = samp_ev.x,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)

### Rename T24 to exp
cilcnCR$sample <-  replace(cilcnCR$sample,cilcnCR$sample=="T24","exp")

### Calculate clearance rates for SJR1
source("scripts/01_function_clearanceRates.R")

cilcnSJR1cr <- rowwise(diacenCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))



###__________________USING OTHER DATA_______________________________
### Create a new df with only the FC (Controls) counts per ml, and
##  calculate the FC counts per ml means across the three replicates

### Start with new base df, volbio_all_cr, that already has the experiment
## sample names changed:  T24 to E (for experimental), FC to C (for control),
##  IC to I, (for initials), site to S (for site)

load("data/Clearance Rates/volbio_all_cr.Rdata")

### Try with pennate diatoms
cr_sjr1_diapen <- volbio_all_cr %>%
  filter(Group == "diatom",
         type == "pennate",
         str_detect(exp, "C|E"),
         samp_ev =="SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

cr_sjr1_diapen_C <- cr_sjr1_diapen %>% 
  filter(exp == "C") %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Make a df with only centric diatoms experimental samples
diapenE <- volbio_all_cr %>% 
  filter(Group == "diatom",
         type == "pennate",
         exp == "E",
         samp_ev == "SJR1") %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)

### Join the above df with the df that has the T24 (experimental) counts
## per ml
diapenCR <- 	left_join(diapenE, cr_sjr1_diapen_C, by = "grp_sz")

names(diapenCR)
### Remove unneeded columns
diapenCR <- 	subset(diapenCR, select = c(-samp_ev.y, -exp.y))


### Rename and re-order columns
diapenCR <- select(diapenCR, 
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
