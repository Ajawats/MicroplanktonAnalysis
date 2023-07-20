## 7/12/22 This file is for combining the 100x and 400x biomass per volume 
## data frames
#  FYI, I also combined both data sets in the 100x_BioPerVol.R file, 

library(tidyverse)

load("bio400_hd.Rdata")
load("bio100_hd.Rdata")

#getwd()

bio_per_vol_all <- rbind(bio100_hd, bio400_hd)

## But do still need to do this below
## Order the data frame first by sample date, then alphabetically by sample
bio_per_vol_all_ord <- bio_per_vol_all [with(bio_per_vol_all, order(samp_date, sample, group, type, sa)), ]
save(bio_per_vol_all_ord, file = "BioPerVolAllOrd.Rdata")
load("BioPerVolAllOrd.Rdata")

##  Now make one that removes the site water samples
bio_vol_ctrl_exp <- filter(bio_per_vol_all_ord, !str_detect(sample, "site"))
save(bio_vol_ctrl_exp, file = "BioPerVolCtrlExp.Rdata")
write_xlsx(bio_vol_ctrl_exp, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/BioPerVolCtEp.xlsx")
## 7/22/22 Calculate the mean  biomass per volume for each different group, per experimental 
###  sample (IC, FC, T24), per sampling event
##  I probably just need to write a function for this.

#ic_mean <- allbiovol_exp 

##Create the file as an excel document
##write_xlsx(outAll, "add filepath and name here.xlsx")

write_xlsx(bio_per_vol_all_ord, "/Users/allisonadams/Documents/Thesis/Microplankton/R Work/100x June 2022/BioPerVolAll.xlsx")
