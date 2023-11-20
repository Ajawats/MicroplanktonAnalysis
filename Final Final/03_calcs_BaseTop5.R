#############################################################################
########### BASE TOP 5 DF CREATION FOR CALCULATING ALL CR AND IR  ###########
###############################################################################

### 11/17/23 I needed to re-run the CR and IR because I didn't use the corrected
##  df where I had added a count of 1 to YBP1 rep 2 cpmE CenDiaLg. See notes in "Thesis
##  Project and Analysis Notes.docx" for the reasons. 

library(tidyverse)
library(writexl)
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")

baseTop5 <- volbio_all_cr %>% 
  mutate(taxaGroup = group_size)

baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "ChlLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "ChlSm"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "CyanoLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "CyanoSm"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "DinoLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "DinoSm"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "FlagLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "PenDiaLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "PenDiaSm"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "ChnDiaLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "ChnDiaSm"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "UnidLg"] <- "Other"
baseTop5["taxaGroup"][baseTop5["taxaGroup"] == "UnidSm"] <- "Other"

save(baseTop5, file = "data7_24/MasterFiles/MasterRFiles/baseTop5.Rdata")
save(baseTop5, file = "Final Final/baseTop5.Rdata")
write_xlsx(baseTop5, "Final Final/baseTop5.xlsx")
