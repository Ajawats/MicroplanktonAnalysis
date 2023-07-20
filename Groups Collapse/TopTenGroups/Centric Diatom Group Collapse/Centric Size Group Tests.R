####################################################################
################ GROUPS COLLAPSE CENTRIC DIATOMS ###################
####################################################################
### 12/16/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

taxaCen <- subset(volbio_all, 
                  select = c(samp_ev, exp, rep, Group,
                             grp_sz, esd, counts_per_ml, 
                             bio_per_vol_pgc_ml)) #%>% 
  #mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)
taxaCen$szesd <- paste(taxaCen$grp_sz, taxaCen$esd)
taxaCen <- filter(taxaCen, grepl('centric', szesd))
taxaCen <- subset(taxaCen,counts_per_ml !=0)
taxaCen <- subset(taxaCen, 
                  select = c(samp_ev, exp, rep, szesd, esd,  
                             counts_per_ml, bio_per_vol_pgc_ml))
taxaCen$counts_per_ml<-formattable(taxaCen$counts_per_ml,
                                format="f",digits=2)
taxaCen$bio_per_vol_pgc_ml<-formattable(taxaCen$bio_per_vol_pgc_ml,
                                     format="f",digits=2)

### testing something
taxaCen2 <- filter(taxaCen, exp=='FC' | exp == 'T24')
taxaCen2 <- subset(taxaCen2, select = c(samp_ev, exp, rep, szesd, counts_per_ml))


### Add up the counts per ml for each distinct pennate 
##  size/esd name but keep the esd  and biomass columns
taxaCenlumpC <- aggregate(counts_per_ml ~ szesd +esd, 
                          data = taxaCen, FUN = sum, na.rm =TRUE)
taxaCenlumpB <- aggregate(bio_per_vol_pgc_ml ~ szesd + esd, 
                          data = taxaCen, FUN = sum, na.rm =TRUE)
taxaCenlump <- merge(taxaCenlumpC, taxaCenlumpB, by="szesd")

taxaCenlump<- subset(taxaCenlump, 
                     select = c(szesd, esd.x, counts_per_ml, bio_per_vol_pgc_ml))
colnames(taxaCenlump)[1] = "Group"
colnames(taxaCenlump)[2] = "esd"

### Put the pennates in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaCenlump$szGroup <- with(taxaCenlump, ifelse(esd < 15, 'small',
                                                ifelse(esd > 25, 'large',
                                                       ifelse(esd >= 15 & esd <= 25, "medium", "WHAT?"))))
taxaCenlumpAgg <- aggregate(counts_per_ml ~ szGroup, 
                            data = taxaCenlump, FUN = sum, na.rm =TRUE)

# Compare to the size groups I made just for the centrics by looking at the plot
taxaCenlump2<- taxaCenlump
taxaCenlump2$szGroup <- with(taxaCenlump, ifelse(esd < 16, 'small',
                                                 ifelse(esd >= 16, 'large', "nada")))
                                                        #ifelse(esd >= 16 & esd <= 22, "medium", "WHAT?"))))
taxaCenlump2 <- aggregate(counts_per_ml ~ szGroup, 
                          data = taxaCenlump2, FUN = sum, na.rm =TRUE)

#write_xlsx(taxaCenLump, "data/TopTen/taxaCenLump.xlsx")
#save(taxaCenLump, file = "data/TopTen/taxaCenLump.Rdata")

