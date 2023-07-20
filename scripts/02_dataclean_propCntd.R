####################################################################
### 100x AND 400X PROPORTION OF CHAMBER (SAMPLE SETTLED) COUNTED ###
####################################################################

### This script starts with the raw proportion of chamber counted data, 
##  and prepares it to be able to use add to the master data sheet so 
## that the counts per ml will be accurate

## _____ Not really necessary since I added the prop_cntd column to the
##  volbio_all df, and changed the counts per ml calculation to include it.
library(tidyverse)
library(writexl)

### Load the excel csv data files, two separate ones for 100x and 400x. 
## because some of the samples had different amounts of sample settled, 
##  which will affect the counts per mL.

propCntd100 <- read_csv("data/MasterFiles/PropCntd_100_R.csv")

#Convert to long format
propCntd100_long <- pivot_longer(data = propCntd100,
                                cols = c(9:68),
                                names_to = "sample",
                                values_to = "propCntd")

### Rearrange the column orders to have the station/sample column first, 
##  using the select function
propCntd100_fin <- select(propCntd100_long, sample, Organism, Group, type, name, 
                         shp, sa, la, wi, propCntd)

# Separate the sampling event names into three columns: sampling event, 
# experimental and replicate
### First, make a separate column for the sampling event 
propCntd100_sampev <- mutate(propCntd100_fin, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                             ifelse(grepl("WLD2", sample), "WLD2", 
                                                                    ifelse(grepl("YBP1", sample), "YBP1",
                                                                           ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                                  ifelse(grepl("SJR2", sample), "SJR2",
                                                                                         ifelse(grepl("YBP2", sample), "YBP2", "??")))))))
# then, using the above file, make a separate column for the expriement type
propCntd100_exp <- mutate(propCntd100_sampev, exp = ifelse(grepl("site", sample), "site",
                                                         ifelse(grepl("IC-1", sample), "IC-1",
                                                                ifelse(grepl("IC-2", sample), "IC-2",
                                                                       ifelse(grepl("IC-3", sample), "IC-3",
                                                                              ifelse(grepl("FC-1", sample), "FC-1",
                                                                                     ifelse(grepl("FC-2", sample), "FC-2",
                                                                                            ifelse(grepl("FC-3", sample), "FC-3",
                                                                                                   ifelse(grepl("T24-1", sample), "T24-1",
                                                                                                          ifelse(grepl("T24-2", sample), "T24-2",
                                                                                                                 ifelse(grepl("T24-3", sample), "T24-3","??")))))))))))
# Then using the above file, make a separate column for the replicate
# the N/A will be filled in where the experiment was "site" and there were
#  no replicates
propCntd100_rep <- mutate(propCntd100_exp, rep = ifelse(grepl("-1", exp), "1",
                                                      ifelse(grepl("-2", exp), "2", 
                                                             ifelse(grepl("-3", exp), "3","N/A"))))
# Then, separate the experiment type from the replicate number
propCntd100_rep["exp"][propCntd100_rep["exp"] == "IC-1"] <- "IC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "IC-2"] <- "IC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "IC-3"] <- "IC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "FC-1"] <- "FC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "FC-2"] <- "FC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "FC-3"] <- "FC"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "T24-1"] <- "T24"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "T24-2"] <- "T24"
propCntd100_rep["exp"][propCntd100_rep["exp"] == "T24-3"] <- "T24"

## Reorder the columns and remove sample, Organism, name, count dates
names(propCntd100_rep)
propCntd100_final <- select(propCntd100_rep, samp_ev, exp, rep, Group, type,
                           shp, sa, la, wi, propCntd, -sample,-Organism,-name)
save(propCntd100_final, file = "data/MasterFiles/propCntd100_final.Rdata")

#______________Repeat for 400x___________________________________
propCntd400 <- read_csv("data/MasterFiles/PropCntd_400_R.csv")

#Convert to long format
propCntd400_long <- pivot_longer(data = propCntd400,
                                cols = c(9:68),
                                names_to = "sample",
                                values_to = "propCntd")

### Rearrange the column orders to have the station/sample column first, 
##  using the select function
propCntd400_fin <- select(propCntd400_long, sample, Organism, Group, type, name, 
                         shp, sa, la, wi, propCntd)

# Separate the sampling event names into three columns: sampling event, 
# experimental and replicate
### First, make a separate column for the sampling event 
propCntd400_sampev <- mutate(propCntd400_fin, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                             ifelse(grepl("WLD2", sample), "WLD2", 
                                                                    ifelse(grepl("YBP1", sample), "YBP1",
                                                                           ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                                  ifelse(grepl("SJR2", sample), "SJR2",
                                                                                         ifelse(grepl("YBP2", sample), "YBP2", "??")))))))
# then, using the above file, make a separate column for the expriement type
propCntd400_exp <- mutate(propCntd400_sampev, exp = ifelse(grepl("site", sample), "site",
                                                         ifelse(grepl("IC-1", sample), "IC-1",
                                                                ifelse(grepl("IC-2", sample), "IC-2",
                                                                       ifelse(grepl("IC-3", sample), "IC-3",
                                                                              ifelse(grepl("FC-1", sample), "FC-1",
                                                                                     ifelse(grepl("FC-2", sample), "FC-2",
                                                                                            ifelse(grepl("FC-3", sample), "FC-3",
                                                                                                   ifelse(grepl("T24-1", sample), "T24-1",
                                                                                                          ifelse(grepl("T24-2", sample), "T24-2",
                                                                                                                 ifelse(grepl("T24-3", sample), "T24-3","??")))))))))))
# Then using the above file, make a separate column for the replicate
# the N/A will be filled in where the experiment was "site" and there were
#  no replicates
propCntd400_rep <- mutate(propCntd400_exp, rep = ifelse(grepl("-1", exp), "1",
                                                      ifelse(grepl("-2", exp), "2", 
                                                             ifelse(grepl("-3", exp), "3","N/A"))))
# Then, separate the experiment type from the replicate number
propCntd400_rep["exp"][propCntd400_rep["exp"] == "IC-1"] <- "IC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "IC-2"] <- "IC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "IC-3"] <- "IC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "FC-1"] <- "FC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "FC-2"] <- "FC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "FC-3"] <- "FC"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "T24-1"] <- "T24"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "T24-2"] <- "T24"
propCntd400_rep["exp"][propCntd400_rep["exp"] == "T24-3"] <- "T24"

## Reorder the columns and remove sample, Organism, name, count dates
names(propCntd400_rep)
propCntd400_final <- select(propCntd400_rep, samp_ev, exp, rep, Group, type,
                           shp, sa, la, wi, propCntd, -sample,-Organism,-name)

save(propCntd400_final, file = "data/MasterFiles/propCntd400_final.Rdata")

### Check against counts in volbioall.R
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")

