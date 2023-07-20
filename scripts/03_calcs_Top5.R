####################################################################
########### NEW TAXA GROUPS WITH LOW IR LUMPED AS "OTHER"###########
####################################################################

### 4/27/23
##  Use volbio_all_cr.Rdata to make a new taxa group df that lumps
##  together all the low IR bio taxa groups into "other"
## The main groups will be ("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm"

### Steps: make a copy of the column, "taxaGroup" so I can keep it
##  Rename the new one to taxaGroup
##  put PenDiaLg, CyanoLg, UnidSm, FlgLg, DinoLg, ChnDiaLg, UnidLg, 
##  PenDiaSm, ChlSm, ChlLg, ShnDiaSm, CyanoSm in one taxa group called "Other"

### DFs created in this script:
load("data/FinalAnalysis/baseTop5.Rdata")

### Updated 7/19 with the new volbio that has the YBP1 CenDiaLg E count rep 2 corrected
data <- read.csv("data/Clearance Rates/volbio_all_cr_new_07_18.csv")

baseTop5 <- data %>% 
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

save(baseTop5, file = "data/FinalAnalysis/baseTop5.Rdata")

