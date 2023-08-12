##############################################################################
############ SITE WATER SAMPLES ABUNDANCE (Counts and Biomass) ###############
##############################################################################


### 3/8/23

library(tidyverse)
library(writexl)
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")
### When returning to this code to run it again, load:
load("data/SiteWater/siteCntBio.Rdata")

siteCntBio <- volbio_all_cr
siteCntBio <- siteCntBio %>%
  filter(exp == "S") %>% 
  select(samp_ev, exp, group = Group, size, counts, cpm,  biomass_cell_pgC, bio_pgC_ml, bio_ugC_l)

## Make a new column named "group_size" that combines the names of the group column
##   and the size column.  See 04_plots_FRgroups.R
siteCntBio$group_size <- (paste(siteCntBio$group, siteCntBio$size))

### Make the short names
siteCntBio["group_size"][siteCntBio["group_size"] == "centricDiatom large"] <- "CenDiaLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "centricDiatom small"] <- "CenDiaSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "chlorophyte large"] <- "ChlLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "chlorophyte small"] <- "ChlSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "ciliate large"] <- "CilLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "ciliate small"] <- "CilSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "cyanobacteria large"] <- "CyanoLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "cyanobacteria small"] <- "CyanoSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "dinoflagellate large"] <- "DinoLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "dinoflagellate small"] <- "DinoSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "flagellate large"] <- "FlagLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "flagellate small"] <- "FlagSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
siteCntBio["group_size"][siteCntBio["group_size"] == "unidentified large"] <- "UnidLg"
siteCntBio["group_size"][siteCntBio["group_size"] == "unidentified small"] <- "UnidSm"

### Order the columns how I want them
siteCntBio <- siteCntBio %>%
  select(samp_ev, exp, group_size, group, size, counts, cpm,  biomass_cell_pgC, bio_pgC_ml, bio_ugC_l)

save(siteCntBio, file =  "data/SiteWater/siteCntBio.Rdata")
write_xlsx(siteCntBio, "data/SiteWater/siteCntBio.xlsx")
load("data/SiteWater/siteCntBio.Rdata")

### Sum the group_size organisms for each sampling event
## Counts per mL
siteCntTot <- siteCntBio %>% 
  group_by(samp_ev, group_size) %>% 
  summarise(tot_ct = sum(cpm))
min(siteCntTot$tot_ct)
max(siteCntTot$tot_ct)
save(siteCntTot, file = "data/SiteWater/siteCntTot.Rdata")

### Biomass, ugC per L
siteBioUgLTot  <- siteCntBio %>% 
  group_by(samp_ev, group_size) %>% 
  summarise(tot_bio_ug = sum(bio_ugC_l))
min(siteBioUgLTot$tot_bio_ug)
max(siteBioUgLTot$tot_bio_ug)
save(siteBioUgLTot, file = "data7_24/SiteWater/siteBioUgLTot.Rdata")

### Make separate plots for each of the sampling events
## Cells
siteCntTotLSZ2 <- siteCntTot %>% 
  filter(samp_ev =="LSZ2")

siteCntTotSJR1 <- siteCntTot %>% 
  filter(samp_ev =="SJR1")

siteCntTotSJR2 <- siteCntTot %>% 
  filter(samp_ev =="SJR2")

siteCntTotWLD2 <- siteCntTot %>% 
  filter(samp_ev =="WLD2")

siteCntTotYBP1 <- siteCntTot %>% 
  filter(samp_ev =="YBP1")

siteCntTotYBP2 <- siteCntTot %>% 
  filter(samp_ev =="YBP2")

## Biomass
siteBioUgLTotLSZ2 <- siteBioUgLTot %>% 
  filter(samp_ev =="LSZ2")

siteBioUgLTotSJR1 <- siteBioUgLTot %>% 
  filter(samp_ev =="SJR1")

siteBioUgLTotSJR2 <- siteBioUgLTot %>% 
  filter(samp_ev =="SJR2")

siteBioUgLTotWLD2 <- siteBioUgLTot %>% 
  filter(samp_ev =="WLD2")

siteBioUgLTotYBP1 <- siteBioUgLTot %>% 
  filter(samp_ev =="YBP1")

siteBioUgLTotYBP2 <- siteBioUgLTot %>% 
  filter(samp_ev =="YBP2")
