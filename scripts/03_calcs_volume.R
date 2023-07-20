####################################################################
################## 100x AND 400X VOLUME AND BIOMASS#################
####################################################################

### Uses data files raw100_final and raw400_final 
##  data/MasterRawData/MasterRFiles/raw100_final.Rdata
##  data/MasterRawData/MasterRFiles/raw400_final.Rdata
## from 02_dataclean_100_400.R
##  separately to calculate volume and biomass, 
## but biomass has its own R script also

### Also includes a separate data frame that excludes zero count organisms

library(tidyverse)
library(writexl)

### Load the volume and biomass functions
source("scripts/01_function_volume.R")
source("scripts/01_function_biomass.R")

################ FOR 100X ################
### Load the raw counts data, file created in 02_dataclean_100_400.R
load("data/MasterFiles/MasterRFiles/raw100_final.Rdata")

### Run the volume function
##  Volume function returns volume per cell in cubic micrometers (um^3), vol_per_cell_um3
##  tot_volume_um3 is the volume per cell times the counts (number of organisms counted)

vol100 <- rowwise(raw100_final) %>% 
  mutate(vol_per_cell_um3 = vol_func(diameter = sa, height = la, width = wi, 
                           shape = shp)) %>% 
  mutate(tot_vol_um3 = vol_per_cell_um3*counts)

### Save the file as an R data file
save(vol100, file = "data/Calculations/vol100.Rdata")
write_xlsx(vol100, "data/Calculations/vol100.xlsx")

### Create a data frame that eliminates the zero count organisms
vol100_no0 <- vol100 %>% 
  rowwise() %>% 
  filter(counts != 0)

save(vol100_no0, file = "data/Calculations/vol100_no0.Rdata")
write_xlsx(vol100_no0, "data/Calculations/vol100_no0.xlsx")

### 10/17/22 Test to see if total obs. will change if I filter out the
##   rows where the volume is 0
#vol100_test_no0_vol <- vol100 %>% 
 # rowwise() %>% 
  #filter(volume != 0) 

  
################ FOR 400X ################

### Load the raw counts data, file created in 02_dataclean_100_400.R

load("data/MasterFiles/MasterRFiles/raw400_final.Rdata")
## Run the volume function
# Also, add a column that calculates the volume per organism,
# and rename the volume column to include units of micrograms cubed.
vol400 <- rowwise(raw400_final) %>% 
  mutate(vol_per_cell_um3 = vol_func(diameter = sa, height = la, width = wi, 
                                     shape = shp)) %>% 
  mutate(tot_vol_um3 = vol_per_cell_um3*counts)

### Save the file as an R data file
save(vol400, file = "data/Calculations/vol400.Rdata")
write_xlsx(vol400, "data/Calculations/vol400.xlsx")

### Create a data frame that eliminates the zero count organisms
vol400_no0 <- vol400 %>% 
  rowwise() %>% 
  filter(counts != 0)
save(vol400_no0, file = "data/Calculations/vol400_no0.Rdata")
write_xlsx(vol400_no0, "data/Calculations/vol400_no0.xlsx")

### 10/17/22 Test to see if total obs. will change if I filter out the
##   rows where the volume is 0
# vol400_test_no0_vol <- vol400 %>% 
  #rowwise() %>% 
  #filter(volume != 0)

### 2/6/23 Testing to see if I can solve an error code in volbio_all
vol100400test <- rbind(vol100, vol400)
vol100400test <- vol100400test %>% 
  mutate(cpm = tot_vol_um3/(propCntd*pres_fact*vol_set_ml))
