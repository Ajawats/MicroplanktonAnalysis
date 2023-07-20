############################################################################
################################ CR PLOTS ##################################
############################################################################
### 1/24/23

library(tidyverse)
library(writexl)
load("data/Clearance Rates/crAllCR.Rdata")

### Plot which taxa have CR > 0

###_________ Prepare the data for preliminary look__________________________
## Per notes from discussion with Wim 1/20/23:
## Take median CR of the 3 E reps
## Have one panel per sampling event
## Get rid of CR near 0, or NA, but keep the negative values

### Apply the mean function to the control mean counts per ml across
## the three replicates
crAllplot <- crAllCR %>% 
  group_by(event, sample, group, Cmn, CR) %>%
  summarize(eMed = median(cpm)) %>% 
ungroup

ggplot(data = crAllCR, )