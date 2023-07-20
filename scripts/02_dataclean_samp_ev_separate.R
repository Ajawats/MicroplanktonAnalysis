####################################################################
################### SEPARATE SAMPLING EVENT NAMES #################
###################################################################

### This data cleaning script shows how to separate the sampling event names
#  from, for example, SJR1 FC-1, to three separate columns: SJR1, FC, 1,
#  which is necessary when analyzing the counts and mean biomass of specific
# organisms and their specific size groups.


### First, make a separate column for the sampling event 
raw100_sampev <- mutate(raw100_fin_hd, samp_ev = ifelse(grepl("SJR1", sample), "SJR1",
                                                 ifelse(grepl("WLD2", sample), "WLD2", 
                                                 ifelse(grepl("YBP1", sample), "YBP1",
                                                 ifelse(grepl("LSZ2", sample), "LSZ2",
                                                 ifelse(grepl("SJR2", sample), "SJR2",
                                                 ifelse(grepl("YBP2", sample), "YBP2", "??")))))))

# Then, using the above file, make a separate column for the experiment type
raw100_exp <- mutate(raw100_sampev, exp = ifelse(grepl("site", sample), "site",
                                          ifelse(grepl("IC-1", sample), "IC-1",
                                          ifelse(grepl("IC-2", sample), "IC-2",
                                          ifelse(grepl("IC-3", sample), "IC-3",
                                          ifelse(grepl("FC-1", sample), "FC-1",
                                          ifelse(grepl("FC-2", sample), "FC-2",
                                          ifelse(grepl("FC-3", sample), "FC-3",
                                          ifelse(grepl("T24-1", sample), "T24-1",
                                          ifelse(grepl("T24-2", sample), "T24-2",
                                          ifelse(grepl("T24-3", sample), "T24-3","??")))))))))))

# Then using the above file, make a separate column for the replicate.
# The N/A will be filled in where the experiment was "site" and there were
#  no replicates.

raw100_rep <- mutate(raw100_exp, rep = ifelse(grepl("1", exp), "1",
                                       ifelse(grepl("2", exp), "2", 
                                        ifelse(grepl("3", exp), "3","N/A"))))

# Then, separate the experiment type from the replicate number
raw100_rep["exp"][raw100_rep["exp"] == "IC-1"] <- "IC"
raw100_rep["exp"][raw100_rep["exp"] == "IC-2"] <- "IC"
raw100_rep["exp"][raw100_rep["exp"] == "IC-3"] <- "IC"
raw100_rep["exp"][raw100_rep["exp"] == "FC-1"] <- "FC"
raw100_rep["exp"][raw100_rep["exp"] == "FC-2"] <- "FC"
raw100_rep["exp"][raw100_rep["exp"] == "FC-3"] <- "FC"
raw100_rep["exp"][raw100_rep["exp"] == "T24-1"] <- "T24"
raw100_rep["exp"][raw100_rep["exp"] == "T24-2"] <- "T24"
raw100_rep["exp"][raw100_rep["exp"] == "T24-3"] <- "T24"

## When applying this code to a data file, you'll need to then remove the column
# that has the sample names in the original format, as SJR1 FC-1, for example. The
# code below shows how I did that, but also removed several other columns that I 
# didn't need for the later analysis.

## Reorder the columns and remove sample, count dates, and a few others
names(raw100_rep)
raw100_final <- subset(raw100_rep, select=c(11,17, 18, 19, 3, 4, 6, 7, 8, 9, 10, 13, 14))
