###  7/25/22 
### This file is for figuring out how to do this that Wim said to do:
### Basically, what you should be doing now is getting the controls and 
##    experimentals in shape so you can look at them and see what types 
##    of particles differ between them consistently among the replicates 
##    for a given experiment.

library(tidyverse)
library(readxl)
library(segmented)

load("BioPerVolCtrlExp.Rdata")


### Try to use select or filter to pull out only one sampling event,
###  only controls and experimentals, FC and T24
##    See DiaCen_WLD2.R where I did this for centric diatoms in WLD2
##  diacen_wld2 <- bio_per_vol_all_ord[grepl("^diatom", bio_per_vol_all_ord$group), ] %>% 
##    filter("centric" == type) %>% 
##    filter(str_detect(sample, "WLD2")) %>% 
##    filter(!str_detect(sample, "site"))

wld2_ct_ex <-  bio_per_vol_all_ord[grepl("^WLD2", bio_per_vol_all_ord$sample), ] %>% 
  dplyr::select(-cnt_date, -tot_vol_um3, -tot_biomass_pgC, -pres_fact, -vol_set_ml,
         -n_bugs, -time_d, -bio_per_vol_pgc_ml)
names(wld2_ct_ex)


