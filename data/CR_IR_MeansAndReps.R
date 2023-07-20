##############################################################################
############# INGESTION RATE AND CLEARANCE RATES REPS AND MEANS ##############
##############################################################################
### 4/6/23

### Individual data frames of the clearance rate and ingestion rates by
##  sampling event, with the means and the reps. For easily looking at
## to compare to the means plots to see if there are big numbers in the
##  reps pulling the outliers way high or way low.

library(tidyverse)
library(writexl)

### CLEARANCE RATES

load("data/Clearance Rates/CrGrpsReps.Rdata")
write_xlsx(CrGrpsReps, "data/Clearance Rates/Reps/CrGrpsReps.xlsx")

CrGrpsRepsYBP2 <- CrGrpsReps %>% 
  filter(event =="YBP2") %>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsYBP2, "data/Clearance Rates/Reps/CrGrpsRepsYBP2.xlsx")

CrGrpsRepsYBP1 <- CrGrpsReps %>% 
  filter(event =="YBP1")%>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsYBP1, "data/Clearance Rates/Reps/CrGrpsRepsYBP1.xlsx")

CrGrpsRepsSJR2 <- CrGrpsReps %>% 
  filter(event =="SJR2")%>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsSJR2, "data/Clearance Rates/Reps/CrGrpsRepsSJR2.xlsx")

CrGrpsRepsWLD2 <- CrGrpsReps %>% 
  filter(event =="WLD2")%>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsWLD2, "data/Clearance Rates/Reps/CrGrpsRepsWLD2.xlsx")

CrGrpsRepsSJR1 <- CrGrpsReps %>% 
  filter(event =="SJR1")%>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsSJR1, "data/Clearance Rates/Reps/CrGrpsRepsSJR1.xlsx")

CrGrpsRepsLSZ2 <- CrGrpsReps %>% 
  filter(event =="LSZ2")%>% 
  select(event, rep, group_size, CR, crMnCpm)
write_xlsx(CrGrpsRepsLSZ2, "data/Clearance Rates/Reps/CrGrpsRepsLSZ2.xlsx")

### INGESTION RATES, Cells
load("data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCells.Rdata")

FrGrpsRepsCellsYBP2 <- FrGrpsRepsCells %>% 
  filter(event =="YBP2") %>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsYBP2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsYBP2.xlsx")

FrGrpsRepsCellsYBP1 <- FrGrpsRepsCells %>% 
  filter(event =="YBP1")%>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsYBP1, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsYBP1.xlsx")

FrGrpsRepsCellsSJR2 <- FrGrpsRepsCells %>% 
  filter(event =="SJR2")%>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsSJR2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsSJR2.xlsx")

FrGrpsRepsCellsWLD2 <- FrGrpsRepsCells %>% 
  filter(event =="WLD2")%>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsWLD2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsWLD2.xlsx")

FrGrpsRepsCellsSJR1 <- FrGrpsRepsCells %>% 
  filter(event =="SJR1")%>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsSJR1, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsSJR1.xlsx")

FrGrpsRepsCellsLSZ2 <- FrGrpsRepsCells %>% 
  filter(event =="LSZ2")%>% 
  select(event, group_size, FR, FRmnCpm)
write_xlsx(FrGrpsRepsCellsLSZ2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsCellsLSZ2.xlsx")


### INGESTION RATES, BIOMASS
load("data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBio.Rdata")

FrGrpsRepsBioYBP2 <- FrGrpsRepsBio %>% 
  filter(event =="YBP2") %>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioYBP2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioYBP2.xlsx")

FrGrpsRepsBioYBP1 <- FrGrpsRepsBio %>% 
  filter(event =="YBP1")%>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioYBP1, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioYBP1.xlsx")

FrGrpsRepsBioSJR2 <- FrGrpsRepsBio %>% 
  filter(event =="SJR2")%>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioSJR2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioSJR2.xlsx")

FrGrpsRepsBioWLD2 <- FrGrpsRepsBio %>% 
  filter(event =="WLD2")%>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioWLD2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioWLD2.xlsx")

FrGrpsRepsBioSJR1 <- FrGrpsRepsBio %>% 
  filter(event =="SJR1")%>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioSJR1, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioSJR1.xlsx")

FrGrpsRepsBioLSZ2 <- FrGrpsRepsBio %>% 
  filter(event =="LSZ2")%>% 
  select(event, group_size, FR, FRmnBpm)
write_xlsx(FrGrpsRepsBioLSZ2, "data/Clearance Rates/Feeding Rates/Reps/FrGrpsRepsBioLSZ2.xlsx")
