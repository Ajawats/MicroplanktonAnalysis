####################################################################
############# JOIN 100x AND 400X VOLUME AND BIOMASS#################
####################################################################

### 7/24/23 updated files to include the YBP1 centric diatom large exp rep 2 count to 1
##  and created a new data folder data7_24 because I lost the data folder when I made a
##  Git Hub repository.

### This file creates volbio_all and volbio_all_no0;has the volume and biomass 
##   calculations for all the  sampling events and includes all the organisms, 
##   including the ones that have zero counts in sampling events other than the ones I
##  counted them in.

### To be used as a base data file for other calculations
### Calculations include: volume per organism, um^3; total volume, um^3; 
##  Biomass per cell pgC; Total biomass, pgC

### Updated 4/23/23: removed chain diatoms from the pennate diatom group_size
## and made their own group_size, ChnDiaSm and ChnDiaLg

library(tidyverse)
library(writexl)
#load("data/Clearance Rates/volbio_all_cr.Rdata")
### Load the 100x and 400x volume/biomass data files. They were created in
##   03_calcs_biomass.R
load("data7_24/Calculations/volbio100.Rdata")
load("data7_24/Calculations/volbio400.Rdata")

### Join the two datasets
volbio100400 <- bind_rows(volbio100, volbio400)

names(volbio100400)
### Add a column that calculates biomass per milliliter, pgC per ml
# total biomass in pgC divided by volume, which is the proportion of sample
##  counted times preservative factor, times volume of sample settled: 
##  tot_biomass_pgC/(propCntd*pres_fact*vol_set_ml) and then name it bio_pgC_ml
### Convert pgC per mL to ugC per L
### Add a column that calculates counts per milliliter, rowwise, so the result
##  is cpm per that experimental sample

### double check YBP2 diatom centric counts--correct
sum(volbio100400[which(volbio100400$samp_ev == "YBP2" & volbio100400$Group == "diatom"
                       & volbio100400$type == "centric"), 11])
    
volbio_all <- volbio100400 %>%
  mutate(bio_pgC_ml = tot_biomass_pgC/(propCntd*pres_fact*vol_set_ml)) %>% 
  mutate(bio_ugC_l = bio_pgC_ml*.001) %>% 
  mutate(cpm = counts/(propCntd*pres_fact*vol_set_ml))

### 2/5/23 error message <error/vctrs_error_incompatible_size>
#Error in `dplyr_col_modify()`:
#  ! Can't recycle `bio_pgC_ml` (size 15060) to size 19320.

### Test by calculating bio_pgC_ml with just volbio100 and volbio400

volbio100_test <- volbio100 %>% 
  mutate(bio_pgC_ml = tot_biomass_pgC/(propCntd*pres_fact*vol_set_ml))
volbio400_test <- volbio400 %>% 
  mutate(bio_pgC_ml = tot_biomass_pgC/(propCntd*pres_fact*vol_set_ml))
volbio_all_test <- rbind(volbio100_test, volbio400_test)
volbio_all_test <- volbio_all_test %>% 
  #mutate(bio_ugC_l = bio_pgC_ml*1000) %>% 
  mutate(counts_per_ml = counts/(propCntd*pres_fact*vol_set_ml))


#volbio_all <- volbio_all %>%
#  mutate(bio_pgC_ml = tot_biomass_pgC/(propCntd*pres_fact*vol_set_ml)) %>% 
##  #mutate(bio_ugC_l = bio_pgC_ml*1000) %>% 
#  mutate(cpm = counts/(propCntd*pres_fact*vol_set_ml))

### Calculate equivalent spherical diameter. Use the equation for the
## volume of a sphere, V= 4/3*pi*rcubed, and solve for r, and then double
## it to get the diameter. 
## r = the cube root of 0.75V/pi, so the function will be that the 
## column esd = 2* the cube root of (0.75V/pi) Where V= volume
#volbio_all <- mutate(esd = vol_per_org_um3*)

source("scripts/01_function_esd.R")
volbio_all <- rowwise(volbio_all) %>% 
  mutate(esd = esd_func(vol_per_cell_um3))


### Add several columns to volbio_all, that put group categories together
## sa and la, so that the dimensions are together
## grp-typ so that the organism group and type are together
## grp_sz, so that the organism group and type and size are together
## grp_esd, so tht the organsim group and type and esd are together

## Don't use formattable anymore. It causes other issues.
#volbio_all$esd<-formattable(volbio_all$esd,format="f",digits=2)

volbio_all$size <- (paste(volbio_all$sa, volbio_all$la)) 
volbio_all$grp_typ <- (paste(volbio_all$Group, volbio_all$type))
volbio_all$grp_sz <- (paste(volbio_all$grp_typ, volbio_all$size))
volbio_all$grp_esd <- (paste(volbio_all$grp_typ, volbio_all$esd))
volbio_all$szesd <- paste(volbio_all$grp_sz, volbio_all$esd)
#volbio_all$szGroup <- with(volbio_all, ifelse(esd < 15, 'small',
#                                              ifelse(esd >= 15, 'large',
#                                                     ifelse("WHAT?"))))
### Above is the way I first added the size groups, but later found this way
##  below and thought it better, since I don't have to deal with the third
## if_else requirement that I assigned "WHAT?" to, since I knew there was no
##  other if_else argument and didn't know what to put there.
volbio_all <- volbio_all %>% 
  mutate(size = case_when(esd < 15 ~ "small",
                          esd >= 15 ~ "large"))

save(volbio_all, file = "data7_24/MasterFiles/MasterRFiles/volbio_all.Rdata")
write_xlsx(volbio_all, "data7_24/MasterFiles/volbio_all.xlsx")
write_csv(volbio_all, "data7_24/MasterFiles/volbio_all.csv")

na_rows <- volbio_all[!complete.cases(volbio_all$Group), ]

################ NO ZEROS ###################
### 7/24/23 Did not run the no-zero data, it's not needed
#volbio_all_no0 <- volbio_all %>% 
#  rowwise() %>% 
#  filter(counts != 0)
#save(volbio_all_no0, file = "data7_24/MasterFiles/MasterRFiles/volbio_all_no0.Rdata")
#write_xlsx(volbio_all_no0, "data7_24/MasterFiles/volbio_all_no0.xlsx")

################################################################################# 
#################### VOLBIO_ALL WITH EXPERIMENT NAME CHANGES ####################
#################################################################################

### Change T24 to E, FC to C, IC to I, site to S
load("data7_24/MasterFiles/MasterRFiles/volbio_all.Rdata")
volbio_all_cr <- volbio_all

volbio_all_cr$exp <- replace(volbio_all_cr$exp,volbio_all_cr$exp=="T24","E")
volbio_all_cr$exp <- replace(volbio_all_cr$exp,volbio_all_cr$exp=="FC","C")
volbio_all_cr$exp <- replace(volbio_all_cr$exp,volbio_all_cr$exp=="IC","I") 
volbio_all_cr$exp <- replace(volbio_all_cr$exp,volbio_all_cr$exp=="site","S")

### Change organism group names as follows, so that I can consolidate them for calculating
##  main group CR and FR
##  Note that chlorophytes, cyanobacteria and unidentified don't need consolidating, as
##  those groups will be calclulated all together as is.

### Add tintinnids to the ciliate Group
volbio_all_cr$Group <- replace(volbio_all_cr$Group,volbio_all_cr$Group=="tintinnid","ciliate")
volbio_all_cr$type <- replace(volbio_all_cr$type,volbio_all_cr$type=="agglutinated","tintinnidAgg") 
volbio_all_cr$type <- replace(volbio_all_cr$type,volbio_all_cr$type=="hyaline","tintinnidHya")

### Make centric diatoms their own category
volbio_all_cr$Group[volbio_all_cr$Group == "diatom" & volbio_all_cr$type == "centric"] <- "centricDiatom"

### All other diatoms are changed to Group, "pennateDiatom", and keep their type category, except
##  for chain diatoms, which have their own category as of 4/23/23, since Wim told me they are
##  very different from pennates and shouldn't be lumped with pennates
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "chain"] <- "chainDiatom"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "egg shaped"] <- "pennateDiatom"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "other"] <- "pennateDiatom"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "pennate"] <- "pennateDiatom"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "pennate fragillaria"] <- "pennateDiatom"
volbio_all_cr$type[volbio_all_cr$type == 
                     "pennate fragillaria"] <- "fragillaria"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom" & volbio_all_cr$type == "pennate pleurosigma"] <- "pennateDiatom"
volbio_all_cr$type[volbio_all_cr$type == 
                     "pennate pleurosigma"] <- "pleurosigma"
volbio_all_cr$type[volbio_all_cr$Group == 
                     "diatom cylindrotheca"] <- "cylindrotheca"
volbio_all_cr$Group[volbio_all_cr$Group == 
                      "diatom cylindrotheca"] <- "pennateDiatom"

### Change Group "Ochrophyte" to flagellate
volbio_all_cr$Group <- replace(volbio_all_cr$Group,volbio_all_cr$Group=="ochrophyte","flagellate")

### Reorder columns
volbio_all_cr <- volbio_all_cr[, c(1,2,3,4,5,6,7,8,9,10,16,11,26,12,13,18,14,15,17,19,20,21,22,23,24,25,27,28,29,30)]
volbio_all_cr <- volbio_all_cr[, c(1,2,3,4,5,6,7,8,9,10,13,11,12,14,15,19,16,17,18,20,21,22,23,24,25,26,27,28,29,30)]

### 4/11/23 Add the 16 group_size name abbreviations that combines the names of the 
##  group column and the size column
volbio_all_cr$group_size <- (paste(volbio_all_cr$Group, volbio_all_cr$size))

volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "centricDiatom large"] <- "CenDiaLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "centricDiatom small"] <- "CenDiaSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "chlorophyte large"] <- "ChlLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "chlorophyte small"] <- "ChlSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "ciliate large"] <- "CilLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "ciliate small"] <- "CilSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "cyanobacteria large"] <- "CyanoLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "cyanobacteria small"] <- "CyanoSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "dinoflagellate large"] <- "DinoLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "dinoflagellate small"] <- "DinoSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "flagellate large"] <- "FlagLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "flagellate small"] <- "FlagSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "chainDiatom large"] <- "ChnDiaLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "chainDiatom small"] <- "ChnDiaSm"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "unidentified large"] <- "UnidLg"
volbio_all_cr["group_size"][volbio_all_cr["group_size"] == "unidentified small"] <- "UnidSm"

save(volbio_all_cr, file = "data7_24/Clearance Rates/volbio_all_cr.Rdata")
