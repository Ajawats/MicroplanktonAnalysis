####################################################################
##################### 100x AND 400X BIOMASS ########################
####################################################################

library(tidyverse)
library(writexl)

### Load the biomass function
source("scripts/01_function_biomass.R")

################ FOR 100X ################
### Load the vol100, which is the data file with the volume calculations,
##  created in 03_calcs_volume.R
load("data/Calculations/vol100.Rdata")

### Run the biomass function:  units of the biomass equation results 
##  are pgC per cell
##  biomass_cell_pgC is the biomass per individual cell
##  tot_biomass_pgC is the total biomass of all the cells counted
volbio100 <- rowwise(vol100) %>% 
  mutate(biomass_cell_pgC = biomass_func(volume = vol_per_cell_um3, Group = Group)) %>% 
  mutate(tot_biomass_pgC = biomass_cell_pgC * counts)

### Save the file as an R data file
save(volbio100, file = "data/Calculations/volbio100.Rdata")
write_xlsx(volbio100, "data/Calculations/volbio100.xlsx")

### NO ZEROS 100x

### Load the vol100_no0, which is the data file with the volume calculations
##  vol100_no0 was created in the script 03_calcs_volume.R
load("data/Calculations/vol100_no0.Rdata")

### Run the biomass function:  units of the biomass equation results 
##  are pgC per cell
##  biomass_pgC_cell is the biomass per individual cell
##  tot_biomass is the total biomass of all the cells counted
volbio100_no0 <- rowwise(vol100_no0) %>% 
  mutate(biomass_cell_pgC = biomass_func(volume = vol_per_cell_um3, Group = Group)) %>% 
  mutate(tot_biomass_pgC = biomass_cell_pgC * counts)
  
### Save the file as an R data file
save(volbio100_no0, file = "data/Calculations/volbio100_no0.Rdata")
write_xlsx(volbio100_no0, "data/Calculations/volbio100_no0.xlsx")


################ FOR 400X ################
### Load the vol400, which is the data file with the volume calculations,
##  created in 03_calcs_volume.R
load("data/Calculations/vol400.Rdata")
### Run the biomass function 
volbio400 <- rowwise(vol400) %>% 
  mutate(biomass_cell_pgC = biomass_func(volume = vol_per_cell_um3, Group = Group)) %>% 
  mutate(tot_biomass_pgC = biomass_cell_pgC * counts)

### Save the file as an R data file
save(volbio400, file = "data/Calculations/volbio400.Rdata")
write_xlsx(volbio400, "data/Calculations/volbio400.xlsx")

### NO ZEROS 400x 

### Load the vol400_no0, which is the data file with the volume calculations
load("data/Calculations/vol400_no0.Rdata")
volbio400_no0 <- rowwise(vol400_no0) %>% 
  mutate(biomass_cell_pgC = biomass_func(volume = vol_per_cell_um3, Group = Group)) %>% 
  mutate(tot_biomass_pgC = biomass_cell_pgC * counts)

### Save the file as an R data file
save(volbio400_no0, file = "data/Calculations/volbio400_no0.Rdata")
write_xlsx(volbio400_no0, "data/Calculations/volbio400_no0.xlsx")

