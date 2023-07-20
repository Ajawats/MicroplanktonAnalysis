############################################################################
######## TEST OF DIATOM BIOMASS WITH DIFFERENT CONVERSION EQUATIONS ########
############################################################################

### 2/13/23
### Compare the biomass of diatoms using two different conversions

### Current biomass conversion function: (volume > 3000)) biomass = .117 * volume^.881
##  (volume <= 3000) biomass = .287 * volume^.811

### Biomass conversion function #2 for both sizes, biomass = .216 * volume^.939
library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")

### Get only diatom biomass and relevent columns from volbio_all_cr that has the
##  original biomass conversion equations
biomass1 <- volbio_all_cr %>% 
  filter(Group == "diatom") %>% 
  subset(select = c(Group, type, shp, sa, la, wi, counts, vol_per_cell_um3, biomass_cell_pgC)) %>% 
  rename(vpc = vol_per_cell_um3, bio1 =biomass_cell_pgC)

### Check one line to make sure that it's correct
.117 * 25735.93^.881
### Yes, it is


### Make a df using the protist plankton biomass conversion for both small and large diatoms
load("data/Calculations/vol100.Rdata")
diatom100_forBio <- vol100 %>% 
  filter(Group == "diatom")
load("data/Calculations/vol400.Rdata")
diatom400_forBio <- vol400 %>% 
  filter(Group == "diatom")
### Join the two datasets
diatomForBio <- bind_rows(diatom100_forBio, diatom400_forBio)

### Calculate biomass with conversion function #2 for both sizes, biomass = .216 * volume^.939
source("scripts/Clearance Rates/01_function_biomass_allProtistPlankton.R")
biomass2 <- rowwise(diatomForBio) %>%
  mutate(bio2 = biomass2_func(volume = vol_per_cell_um3, Group = Group))
biomass2 <- subset(biomass2, select = c(Group, type, shp, sa, la, wi, counts, vol_per_cell_um3, bio2)) %>% 
  rename(vpc = vol_per_cell_um3)

### Join the two datasets
biomass_1_2 <- left_join(biomass1, biomass2)

### Filter out all the duplicates so I'm left with only the distince
##  diatom type, shp, sa, la, wi, and the corresponding volume per cell and
##  biomass per cell with the two different biomass calculations to compare
biomass_1_2 <-distinct(biomass_1_2)
biomass_1_2 <- biomass_1_2 %>% 
  group_by(Group, type, shp, sa, la, vpc, bio1, bio2)%>% 
  summarize(tot_cts = sum(counts)) %>% 
  select(Group, type, shp, sa, la, tot, vpc, bio1, bio2)

write_xlsx(biomass_1_2, "data/biomass_1_2.xlsx")


