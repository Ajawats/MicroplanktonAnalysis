######################################################################
############# CLEARANCE RATE CALCULATIONS ############################
####################  FOR TOP 5 + OTHER  #############################
######################################################################

### 11/17/23 Edited from the original df, 03_calcs_CrTop5_0821.R in Final Final/
##  Clearance/R and Rdata files: 
##  I needed to use the df that has a "1" added to the cpmE of 
##  YBP1 CenDiaLg because it was a zero, which results in an NA for CR, and that 
##  results in IR non-existent. I re-ran the code in a new R script, Final Final/
##  03_calcs_BaseTop5.R.

### Now will re-run CR and IR

### DFs created here:
load("Final Final/Clearance/R and Rdata files/CR_Reps_Top5.Rdata")
load("Final Final/Clearance/R and Rdata files/CrMnTop5.Rdata")
load("Final Final/Clearance/R and Rdata files/CR_Rep_Mn_Top5.Rdata")

### 8/21/23
library(tidyverse)
library(writexl)

load("Final Final/baseTop5.Rdata")
source("scripts/01_function_clearanceRates.R")
### DF created here: ("Final Final/Clearance/CrMnTop5.Rdata")

### Use baseTop5.Rdata as the base file, because it has the taxaGroup column with 
##  just top 5 + other
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
CR_Reps_Top5 <- rowwise(sumCpmE_Cmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = CmnCpm, expCt = cpmE))
write_xlsx(CR_Rep_Mn_Top5, "Final Final/Clearance/Excel docs/CR_Reps_Top5.xlsx")
save(CR_Rep_Mn_Top5,file =  "Final Final/Clearance/R and Rdata files/CR_Reps_Top5.Rdata")

###  Take the mean of the CR per taxaGroup per event
CrMnTop5 <- CR_Reps_Top5 %>% 
  group_by(event, taxaGroup) %>% 
  summarize(CrMNmlcd = mean(CRmlcd, na.rm = TRUE))
write_xlsx(CrMnTop5, "Final Final/Clearance/Excel docs/CrMnTop5.xlsx")
save(CrMnTop5,file =  "Final Final/Clearance/R and Rdata files/CrMnTop5.Rdata")

### Join the above two dfs so the CR reps and means are in the same df
CR_Rep_Mn_Top5 <- left_join(CR_Reps_Top5, CrMnTop5, 
                            by = c("event", "taxaGroup"))
write_xlsx(CR_Rep_Mn_Top5, "Final Final/Clearance/Excel docs/CR_Rep_Mn_Top5.xlsx")
save(CR_Rep_Mn_Top5,file =  "Final Final/Clearance/R and Rdata files/CR_Rep_Mn_Top5.Rdata")

