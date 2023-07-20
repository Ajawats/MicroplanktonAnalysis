####################################################################
############# GROUPS COLLAPSE Flagellate Cryptomonas ###############
####################################################################
### 12/16/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

taxaFlagC <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaFlagC$Group <- paste(taxaFlagC$grp_sz, taxaFlagC$esd)

taxaFlagC <- filter(taxaFlagC, grepl('cryptomonas', Group))

taxaFlagC <- subset(taxaFlagC,totalMnCPM !=0)

taxaFlagC <- subset(taxaFlagC, 
                  select = c(samp_ev, exp, rep, mag, Group, grp_sz, esd,  
                             totalMnCPM, totalMnBPM))

taxaFlagC$szesd <- paste(taxaFlagC$grp_sz, taxaFlagC$esd)

taxaFlagC$totalMnCPM<-formattable(taxaFlagC$totalMnCPM,
                                format="f",digits=2)

taxaFlagC$totalMnBPM<-formattable(taxaFlagC$totalMnBPM,
                                format="f",digits=2)


### Add up the counts per ml for each distinct pennate 
##  size/esd name but keep the esd  and biomass columns
taxaFlagClumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                          data = taxaFlagC, FUN = sum, na.rm =TRUE)

taxaFlagClumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                          data = taxaFlagC, FUN = sum, na.rm =TRUE)

taxaFlagClump <- merge(taxaFlagClumpC, taxaFlagClumpB, by="szesd")

taxaFlagClump<- subset(taxaFlagClump, 
                     select = c(szesd, esd.x, totalMnCPM, totalMnBPM))

colnames(taxaFlagClump)[1] = "Group"
colnames(taxaFlagClump)[2] = "esd"

### Put the pennates in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaFlagClump$szGroup <- with(taxaFlagClump, ifelse(esd < 7.25, 'small',
                                                ifelse(esd > 12, 'large',
                                                       ifelse(esd >= 7.25 & esd <= 12, "medium", "WHAT?"))))
taxaFlagClumpAgg <- aggregate(totalMnCPM ~ szGroup, 
                            data = taxaFlagClump, FUN = sum, na.rm =TRUE)

# Compare to the size groups I made just for the centrics by looking at the plot
taxaFlagClump2<- taxaFlagClump

taxaFlagClump2$szGroup <- with(taxaFlagClump, ifelse(esd < 16, 'small',
                                                 ifelse(esd >= 16, 'large', "nada")))

#ifelse(esd >= 16 & esd <= 22, "medium", "WHAT?"))))
taxaFlagClump2 <- aggregate(totalMnCPM ~ szGroup, 
                          data = taxaFlagClump2, FUN = sum, na.rm =TRUE)

#write_xlsx(taxaFlagCLump, "data/TopTen/taxaFlagCLump.xlsx")
#save(taxaFlagCLump, file = "data/TopTen/taxaFlagCLump.Rdata")

