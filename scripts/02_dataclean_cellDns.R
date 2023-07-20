####################################################################
########### 100x AND 400X CELL DENSITY CLEANED FOR ANALYSIS ########
####################################################################

### This script starts with the raw cell density data, and prepares it to be able
#  to use add to the master data sheet so that the counts per ml will be accurate

library(tidyverse)
library(writexl)

### Load the excel csv data files, two separate ones for 100x and 400x. 
## because some of the samples had different amounts of sample settled, 
##  which will affect the counts per mL.

cellDns100 <- read_csv("data/MasterFiles/CellDns100_R.csv")
cellDns400 <- read_csv("data/MasterFiles/CellDns400_R.csv")

#Convert to long format
cellDns100_long <- pivot_longer(data = cellDns100,
                            cols = c(9:68),
                            names_to = "sample",
                            values_to = "cellDns_ct_L")

### Rearrange the column orders to have the station/sample column first, 
##  using the select function
cellDns100_fin <- select(cellDns100_long, sample, Organism, Group, type, name, 
                         shp, sa, la, wi, cellDns_ct_L)

# Separate the sampling event names into three columns: sampling event, 
# experimental and replicate
### First, make a separate column for the sampling event 
cellDns100_sampev <- mutate(cellDns100_fin, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                        ifelse(grepl("WLD2", sample), "WLD2", 
                                                               ifelse(grepl("YBP1", sample), "YBP1",
                                                                      ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                             ifelse(grepl("SJR2", sample), "SJR2",
                                                                                    ifelse(grepl("YBP2", sample), "YBP2", "??")))))))
# then, using the above file, make a separate column for the expriement type
cellDns100_exp <- mutate(cellDns100_sampev, exp = ifelse(grepl("site", sample), "site",
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
cellDns100_rep <- mutate(cellDns100_exp, rep = ifelse(grepl("-1", exp), "1",
                                              ifelse(grepl("-2", exp), "2", 
                                                     ifelse(grepl("-3", exp), "3","N/A"))))
# Then, separate the experiment type from the replicate number
cellDns100_rep["exp"][cellDns100_rep["exp"] == "IC-1"] <- "IC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "IC-2"] <- "IC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "IC-3"] <- "IC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "FC-1"] <- "FC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "FC-2"] <- "FC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "FC-3"] <- "FC"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "T24-1"] <- "T24"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "T24-2"] <- "T24"
cellDns100_rep["exp"][cellDns100_rep["exp"] == "T24-3"] <- "T24"

## Reorder the columns and remove sample, Organism, name, count dates
names(cellDns100_rep)
cellDns100_final <- select(cellDns100_rep, samp_ev, exp, rep, Group, type,
                           shp, sa, la, wi, cellDns_ct_L, -sample,-Organism,-name)

### Add a column that calculates the cell density in counts per milliliter
cellDns100_final <- mutate(cellDns100_final, cpm = cellDns_ct_L/1000)
save(cellDns100_final, file = "data/MasterFiles/cellDns100_final.Rdata")


#______________Repeat for 400x___________________________________
#Convert to long format
cellDns400_long <- pivot_longer(data = cellDns400,
                                cols = c(9:68),
                                names_to = "sample",
                                values_to = "cellDns_ct_L")

### Rearrange the column orders to have the station/sample column first, 
##  using the select function
cellDns400_fin <- select(cellDns400_long, sample, Organism, Group, Type, name, 
                         shp, sa, la, wi, cellDns_ct_L)

# Separate the sampling event names into three columns: sampling event, 
# experimental and replicate
### First, make a separate column for the sampling event 
cellDns400_sampev <- mutate(cellDns400_fin, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                             ifelse(grepl("WLD2", sample), "WLD2", 
                                                                    ifelse(grepl("YBP1", sample), "YBP1",
                                                                           ifelse(grepl("LSZ2", sample), "LSZ2",
                                                                                  ifelse(grepl("SJR2", sample), "SJR2",
                                                                                         ifelse(grepl("YBP2", sample), "YBP2", "??")))))))
# then, using the above file, make a separate column for the expriement type
cellDns400_exp <- mutate(cellDns400_sampev, exp = ifelse(grepl("site", sample), "site",
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
cellDns400_rep <- mutate(cellDns400_exp, rep = ifelse(grepl("-1", exp), "1",
                                                      ifelse(grepl("-2", exp), "2", 
                                                             ifelse(grepl("-3", exp), "3","N/A"))))
# Then, separate the experiment type from the replicate number
cellDns400_rep["exp"][cellDns400_rep["exp"] == "IC-1"] <- "IC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "IC-2"] <- "IC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "IC-3"] <- "IC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "FC-1"] <- "FC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "FC-2"] <- "FC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "FC-3"] <- "FC"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "T24-1"] <- "T24"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "T24-2"] <- "T24"
cellDns400_rep["exp"][cellDns400_rep["exp"] == "T24-3"] <- "T24"

## Reorder the columns and remove sample, Organism, name, count dates
names(cellDns400_rep)
cellDns400_final <- select(cellDns400_rep, samp_ev, exp, rep, Group, type=Type,
                           shp, sa, la, wi, cellDns_ct_L, -sample,-Organism,-name)

### Add a column that calculates the cell density in counts per milliliter
cellDns400_final <- mutate(cellDns400_final, cpm = cellDns_ct_L/1000)
save(cellDns400_final, file = "data/MasterFiles/cellDns400_final.Rdata")
### Check against counts in volbioall.R
load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
