######################################################################
############# CLEARANCE RATE CALCULATIONS ############################
####################  FOR TOP 5 + OTHER  #############################
######################################################################

### 8/21/23
library(tidyverse)
library(writexl)
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")
load("data7_24/FinalAnalysis/baseTop5.Rdata")
source("scripts/01_function_clearanceRates.R")
### DF created here: ("Final Final/Clearance/CrMnTop5.Rdata")

### Use baseTop5.Rdata as the base file, because it has the taxaGroup column with just top 5 + other
### Select only the columns needed for CR
baseTop5CR <- baseTop5 %>% 
  select(samp_ev, exp, rep,  taxaGroup, cpm)

### Sum up the cpm for the top 5 + other taxa groups, adding all cpm for organisms
##  that fall into those top 5 + other taxa groups, such as, all the small centric diatoms
##  in a sampling event, experimental bottle
sumCpm <- baseTop5CR %>% 
  group_by(samp_ev, taxaGroup, exp, rep) %>% 
  summarise(TotalCpm=sum(cpm),
            .groups = 'drop') %>% 
  as.data.frame()

## Create the base data frame that has only the control and experimental cpm
sumCpm_CE <- sumCpm %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
sumCpm_C <- sumCpm_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = TotalCpm)

### Create a df with only experimental samples
sumCpm_E <- sumCpm_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = TotalCpm) 

### Create a df with only initial samples (for the ingestion rates by counts, for later)
sumCpm_I <- sumCpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sumCpm_Cmn <- sumCpm_C %>% 
  group_by(samp_ev, taxaGroup, exp) %>% 
  summarise(CmnCpm=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sumCpmE_Cmn <- 	left_join(sumCpm_E, sumCpm_Cmn, 
                          by = c("samp_ev", "taxaGroup"))

### Remove unneeded columns, and rename and re-order remaining columns
##  Do I need the exp column?--Can't keep it since E is one column and C
## is another column
names(sumCpmE_Cmn)
sumCpmE_Cmn <- select(sumCpmE_Cmn, 
                      event = samp_ev,
                      taxaGroup, rep, cpmE,
                      CmnCpm)

### Calculate clearance rates. The resulting data frame includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
CR_Rep_Mn_Top5 <- rowwise(sumCpmE_Cmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = CmnCpm, expCt = cpmE))
write_xlsx(CR_Rep_Mn_Top5, "Final Final/Clearance/CR_Rep_Mn_Top5.xlsx")
save(CR_Rep_Mn_Top5,file =  "Final Final/Clearance/CR_Rep_Mn_Top5.Rdata")

###  Take the mean of the CR per taxaGroup per event
CrMnTop5 <- CR_Rep_Mn_Top5 %>% 
  group_by(event, taxaGroup) %>% 
  summarize(CrMNmlcd = mean(CRmlcd, na.rm = TRUE))
write_xlsx(CrMnTop5, "Final Final/Clearance/CrMnTop5.xlsx")
save(CrMnTop5,file =  "Final Final/Clearance/CrMnTop5.Rdata")



