####################################################################
################ GROUPS COLLAPSE UNIDENTIFIED ROUND ################
####################################################################
### 12/19/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

taxaUnRn <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, Group, type, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaUnRn$grpSzEsd <- paste(taxaUnRn$grp_sz, taxaUnRn$esd)

taxaUnRn <- filter(taxaUnRn, type =='round', Group=='unidentified')

taxaUnRn <- subset(taxaUnRn,totalMnCPM !=0)

taxaUnRn <- subset(taxaUnRn, 
                  select = c(samp_ev, exp, rep, type, mag, grpSzEsd, grp_sz, esd,  
                             totalMnCPM, totalMnBPM))

taxaUnRn$totalMnCPM<-formattable(taxaUnRn$totalMnCPM,
                                format="f",digits=2)

taxaUnRn$totalMnBPM<-formattable(taxaUnRn$totalMnBPM,
                                format="f",digits=2)


### Add up the counts per ml for each distinct pennate 
##  size/esd name but keep the esd  and biomass columns
taxaUnRnlumpC <- aggregate(totalMnCPM ~ grpSzEsd +esd, 
                          data = taxaUnRn, FUN = sum, na.rm =TRUE)

taxaUnRnlumpB <- aggregate(totalMnBPM ~ grpSzEsd + esd, 
                          data = taxaUnRn, FUN = sum, na.rm =TRUE)

taxaUnRnlump <- merge(taxaUnRnlumpC, taxaUnRnlumpB, by="grpSzEsd")

taxaUnRnlump<- subset(taxaUnRnlump, 
                     select = c(grpSzEsd, esd.x, totalMnCPM, totalMnBPM))

colnames(taxaUnRnlump)[2] = "esd"

### Put the unid round in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaUnRnlump$grpSzEsd <- with(taxaUnRnlump, ifelse(esd < 14, 'small',
                                                ifelse(esd > 28, 'large',
                                                       ifelse(esd >= 14 & esd <= 28, "medium", "WHAT?"))))
taxaUnRnlumpAgg <- aggregate(totalMnCPM ~ grpSzEsd, 
                            data = taxaUnRnlump, FUN = sum, na.rm =TRUE)

# Compare to the size groups I made just for the centrics by looking at the plot
taxaUnRnlump2<- taxaUnRnlump
taxaUnRnlump2$grpSzEsd <- with(taxaUnRnlump, ifelse(esd < 16, 'small',
                                                 ifelse(esd >= 16, 'large', "nada")))
#ifelse(esd >= 16 & esd <= 22, "medium", "WHAT?"))))
taxaUnRnlump2 <- aggregate(totalMnCPM ~ grpSzEsd, 
                          data = taxaUnRnlump2, FUN = sum, na.rm =TRUE)

#write_xlsx(taxaUnRnLump, "data/TopTen/taxaUnRnLump.xlsx")
#save(taxaUnRnLump, file = "data/TopTen/taxaUnRnLump.Rdata")

