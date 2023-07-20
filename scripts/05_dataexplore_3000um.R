############################################################################
################# DIATOM AND DINOFLAGELLATE SIZE CHECK #####################
############################################################################
### 1/30/23
### I was reviewing the biomass function calculations and saw that I needed
## to revise some of the equations according to what MD&L 2000 says about
##  a different equation for diatoms and dinoflagellates > 3,000 µm^3.


### Check to see if any diatoms are > 3,000 µm^3
library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
diabig <- volbio_all_cr %>% 
  filter(Group == "diatom", vol_per_org_um3 > 3000) %>% 
  select(samp_ev, Group, type, sa, la, wi, counts, vol_per_org_um3)
write_xlsx(diaSize, "data/Main Groups/diaSize.xlsx")

diasmall <- volbio_all_cr %>% 
  filter(Group == "diatom", vol_per_org_um3 <= 3000) %>% 
  select(samp_ev, Group, type, sa, la, wi, counts, vol_per_org_um3)

diacil <- volbio_all_cr %>% 
  filter(Group %in% c("diatom", "ciliate")) %>% 
  select(samp_ev, Group, type, sa, la, wi, counts, vol_per_org_um3) %>% 
  drop_na(vol_per_org_um3)

#<- filter(taxaGrp, Group %in% c("diatom", "cyanobacteria", "ciliate", 
#                                "flagellate", "unidentified")) 

### Yes, there are 208 observations
## centric, 60; chain, 45;  egg shaped, 16; other, 4;
##  pennate, 65; pennate fragillaria, 6; pennate pleurosigma, 12.

unique(diaSize$type)
### Same with dinoflagellates to see their size, since the MD&L paper
## said dinoflagellates  are  significantly  denser  than  diatoms, but only
## for cells larger  than  3,000 µm3;  smaller  dinoflagellates  and  diatoms
## did not differ significantly in their carbon content

dinoSize <- volbio_all_cr %>% 
  filter(Group == "dinoflagellate") %>% 
  select(samp_ev, Group, type, shp, sa, la, wi, esd, counts, vol_per_org_um3) %>% 
  drop_na()
dinoSize <- dinoSize %>% 
  mutate(size = case_when(vol_per_org_um3 <= 3000 ~ "small",
                          vol_per_org_um3 > 3000 ~ "large")) %>% 
  drop_na()
write_xlsx(dinoSize, "data/Main Groups/dinoSize.xlsx")

### 20 total obs of dinoflagellates; 5 small; 15 large

### Result is that I will need to change the biomass function to include
## the different equations for the different sized diatoms and dinoflagellates

### Use the diatom data frame to test the edits to the biomass function

load("data/vol100.Rdata")
biodia_test <- rowwise(diacil) %>% 
  mutate(biomass = biotest_func(vol_org = vol_per_org_um3, Group = Group))

diacil_bio <- rowwise(diacil) %>% 
  mutate(biomass_pgC_cell = biotest_func( vol_org = vol_per_org_um3, Group = Group))

volbio_noNAesd <- volbio_all_cr %>% 
  drop_na(esd) %>% 
  select(samp_ev, exp, rep, Group, type, shp,
         sa, la, wi, counts, counts_per_ml, vol_per_org_um3, esd)

cil <- volbio_noNAesd %>% 
  filter(Group == "ciliate") %>% 
  select(samp_ev, Group, type, shp, sa, la, wi, esd, counts, vol_per_org_um3)

cilSize <- cil%>% 
  mutate(size = case_when(vol_per_org_um3 <= 3000 ~ "small",
                          vol_per_org_um3 > 3000 ~ "large"))
