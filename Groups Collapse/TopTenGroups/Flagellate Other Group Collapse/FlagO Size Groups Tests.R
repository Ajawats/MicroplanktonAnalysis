####################################################################
############# GROUPS COLLAPSE Flagellate Other ###############
####################################################################
### 12/16/22
### see 03_calcs_GroupsCollapse.R for references and code for all groups 

library(tidyverse)
library(writexl)
library(formattable)
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
source("scripts/01_function_wimGraph and Palettes.R")

taxaFlagO <- subset(volbio_all, 
                    select = c(samp_ev, exp, rep, mag,
                               grp_sz, esd, counts_per_ml, 
                               bio_per_vol_pgc_ml)) %>% 
  mutate(totalMnCPM=counts_per_ml,totalMnBPM= bio_per_vol_pgc_ml)

taxaFlagO$Group <- paste(taxaFlagO$grp_sz, taxaFlagO$esd)

taxaFlagO <- filter(taxaFlagO, grepl('flagellate', Group))

taxaFlagO <-filter(taxaFlagO, !str_detect(Group, "cryptomonas"))
taxaFlagO <-filter(taxaFlagO, !str_detect(Group, "dinoflagellate"))
taxaFlagO <- subset(taxaFlagO,totalMnCPM !=0)

taxaFlagO <- subset(taxaFlagO, 
                    select = c(samp_ev, exp, rep, mag, Group, grp_sz, esd,  
                               totalMnCPM, totalMnBPM))

### ??? Trying to figure out why I did this, when it's identical to what I just created,
##  "Group" which should be changed, since "Group" is already a name for the organism group
##  I started to change this in the ciliate cone/round code 12/19
taxaFlagO$szesd <- paste(taxaFlagO$grp_sz, taxaFlagO$esd)

taxaFlagO$totalMnCPM<-formattable(taxaFlagO$totalMnCPM,
                                  format="f",digits=2)

taxaFlagO$totalMnBPM<-formattable(taxaFlagO$totalMnBPM,
                                  format="f",digits=2)


### Add up the counts per ml for each distinct pennate 
##  size/esd name but keep the esd  and biomass columns
taxaFlagOlumpC <- aggregate(totalMnCPM ~ szesd +esd, 
                            data = taxaFlagO, FUN = sum, na.rm =TRUE)

taxaFlagOlumpB <- aggregate(totalMnBPM ~ szesd + esd, 
                            data = taxaFlagO, FUN = sum, na.rm =TRUE)

taxaFlagOlump <- merge(taxaFlagOlumpC, taxaFlagOlumpB, by="szesd")

taxaFlagOlump<- subset(taxaFlagOlump, 
                       select = c(szesd, esd.x, totalMnCPM, totalMnBPM))

colnames(taxaFlagOlump)[1] = "Group"
colnames(taxaFlagOlump)[2] = "esd"

### Put the flagellates in small, medium and large, alternately with 12 and 24, then
##  15 and 25 as the break points, and with the individually determined break points
## in 03_calcs_GroupCollapse.R
taxaFlagOlump$szGroup <- with(taxaFlagOlump, ifelse(esd < 15, 'small',
                                                    ifelse(esd > 25, 'large',
                                                           ifelse(esd >= 15 & esd <= 25, "medium", "WHAT?"))))
taxaFlagOlumpAgg <- aggregate(totalMnCPM ~ szGroup, 
                              data = taxaFlagOlump, FUN = sum, na.rm =TRUE)

# Compare to the size groups I made just for the centrics by looking at the plot
taxaFlagOlump2<- taxaFlagOlump

taxaFlagOlump2$szGroup <- with(taxaFlagOlump, ifelse(esd < 16, 'small',
                                                     ifelse(esd >= 16, 'large', "nada")))

#ifelse(esd >= 16 & esd <= 22, "medium", "WHAT?"))))
taxaFlagOlump2 <- aggregate(totalMnCPM ~ szGroup, 
                            data = taxaFlagOlump2, FUN = sum, na.rm =TRUE)

#write_xlsx(taxaFlagOLump, "data/TopTen/taxaFlagOLump.xlsx")
#save(taxaFlagOLump, file = "data/TopTen/taxaFlagOLump.Rdata")

