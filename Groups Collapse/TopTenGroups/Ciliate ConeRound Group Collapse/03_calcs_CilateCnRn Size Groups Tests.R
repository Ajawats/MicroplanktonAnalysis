####################################################################
############ GROUPS COLLAPSE CILIATES ROUND AND CONE ###############
####################################################################
### 12/19/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

taxaCnRn <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, Group, type, mag,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaCnRn$grpSzEsd <- paste(taxaCnRn$grp_sz, taxaCnRn$esd)

taxaCnRn <- filter(taxaCnRn, type=='cone'| type =='round', Group=='ciliate')

taxaCnRn <- subset(taxaCnRn,totalMnCPM !=0)

taxaCnRn <- subset(taxaCnRn, 
                  select = c(samp_ev, exp, rep, type, mag, grpSzEsd, grp_sz, esd,  
                             totalMnCPM, totalMnBPM))

taxaCnRn$totalMnCPM<-formattable(taxaCnRn$totalMnCPM,
                                format="f",digits=2)

taxaCnRn$totalMnBPM<-formattable(taxaCnRn$totalMnBPM,
                                format="f",digits=2)


### Add up the counts per ml for each distinct pennate 
##  size/esd name but keep the esd  and biomass columns
taxaCnRnlumpC <- aggregate(totalMnCPM ~ grpSzEsd +esd, 
                          data = taxaCnRn, FUN = sum, na.rm =TRUE)

taxaCnRnlumpB <- aggregate(totalMnBPM ~ grpSzEsd + esd, 
                          data = taxaCnRn, FUN = sum, na.rm =TRUE)

taxaCnRnlump <- merge(taxaCnRnlumpC, taxaCnRnlumpB, by="grpSzEsd")

taxaCnRnlump<- subset(taxaCnRnlump, 
                     select = c(grpSzEsd, esd.x, totalMnCPM, totalMnBPM))

colnames(taxaCnRnlump)[2] = "esd"

### Put the ciliates in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaCnRnlump$grpSzEsd <- with(taxaCnRnlump, ifelse(esd < 15, 'small',
                                                ifelse(esd > 25, 'large',
                                                       ifelse(esd >= 15 & esd <= 25, "medium", "WHAT?"))))
taxaCnRnlumpAgg <- aggregate(totalMnCPM ~ grpSzEsd, 
                            data = taxaCnRnlump, FUN = sum, na.rm =TRUE)

# Compare to the size groups I made just for the centrics by looking at the plot
taxaCnRnlump2<- taxaCnRnlump
taxaCnRnlump2$grpSzEsd <- with(taxaCnRnlump, ifelse(esd < 16, 'small',
                                                 ifelse(esd >= 16, 'large', "nada")))
#ifelse(esd >= 16 & esd <= 22, "medium", "WHAT?"))))
taxaCnRnlump2 <- aggregate(totalMnCPM ~ grpSzEsd, 
                          data = taxaCnRnlump2, FUN = sum, na.rm =TRUE)

#write_xlsx(taxaCnRnLump, "data/TopTen/taxaCnRnLump.xlsx")
#save(taxaCnRnLump, file = "data/TopTen/taxaCnRnLump.Rdata")

