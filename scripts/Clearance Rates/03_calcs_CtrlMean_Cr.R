############################################################################
################ CONTROL MEANS CPM AND CR FUNCTION TESTING #################
############################################################################
### 1/22/23

### This script is for calcuating the means of the control samples 
##  counts per ml (cpm) and then joining that df with another df of
## the experimental samples counts per ml (individual per replicate)
## and then using that data to calculate the clearance rates.

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")

### This is an example with one taxa, cone ciliates, from all sampling events

### Create the base data frame with just cone ciliates from SJR1
cilcon <- volbio_all_cr %>%
  filter(Group == "ciliate",
         type == "cone",
         str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)
### If you want just one sampling event, add to the filter argument:
##  samp_ev =="SJR1", or any other sampa_ev

### Create another df from the above, with only the control samples
cilconC <- cilcon %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
cilconCmn <- cilconC %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Create a df with only cone ciliate experimental samples
cilconE <- cilcon %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz)

### Join the experimental sample df with the control means df
cilconCE <- 	left_join(cilconE, cilconCmn, by = c("samp_ev", "grp_sz"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_eve" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(cilconCE)
cilconCE <- select(cilconCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
cilconCR <- rowwise(cilconCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))



###_____________________1/22/23 Calculate everything together________________###

load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")

### Edited on 1/29/23 to inlcude size group column that I edited today 
##  (size) that has small < 15µm and large >= 15 µm esd,
## and that includes the biomass column.
##
## Create the base data frame 
crAll <- volbio_all_cr %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, cpm, size)

### Create another df from the above, with only the control samples
crAllC <- crAll %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
crAllCmn <- crAllC %>% 
  group_by(samp_ev, exp, grp_sz, size) %>%
  summarize(Cmn = mean(cpm),
            n=length(cpm)) %>% 
  ungroup

### Create a df with only experimental samples
crAllE <- crAll %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz, size) %>% 
  ungroup

### Join the experimental sample df with the control means df
crAllCE <- 	left_join(crAllE, crAllCmn, by = c("samp_ev", "grp_sz"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(crAllCE)
crAllCE <- select(crAllCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm,
                   Cmn,
                  size = size.x)
save(crAllCE, file = "data/Clearance Rates/crAllCE.Rdata")

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
crAllCR <- rowwise(crAllCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
save(crAllCR, file = "data/Clearance Rates/crAllCR.Rdata")
write_xlsx(crAllCR, "data/Clearance Rates/crAllCR.xlsx")
load("data/Clearance Rates/crAllCR.Rdata")

###__________Eveything together with the the sa la columns____________###

load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
## Create the base data frame 
crAll1 <- volbio_all_cr %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, Group, type, sa, la, wi, cpm, size)

### Create another df from the above, with only the control samples
crAll1C <- crAll1 %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
crAll1Cmn <- crAll1C %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, size) %>%
  summarize(Cmn = mean(cpm),
            n=length(cpm)) %>% 
  ungroup

### Create a df with only experimental samples
crAll1E <- crAll1 %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, size) %>% 
  ungroup

### Join the experimental sample df with the control means df
crAll1CE <- 	left_join(crAll1E, crAll1Cmn, by = c("samp_ev", "Group", "type", "sa", "la", "wi"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(crAll1CE)
crAll1CR <- select(crAll1CE, 
                  event = samp_ev,
                  sample = exp.x,
                  rep,
                  group = Group,
                  type, sa, la, wi,
                  cpm,
                  Cmn,
                  size = size.x)

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
crAll1CR <- rowwise(crAll1CR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
save(crAll1CR, file = "data/Clearance Rates/crAll1CR.Rdata")
write_xlsx(crAll1CR, "data/Clearance Rates/crAll1CR.xlsx")


### Calculate clearance rates for biomass
## First make a df that calculates the intitials mean CR for biomass
## Create the base data frame with just the initial samples
fr_allI <- volbio_all_cr %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, type, sa, la, wi, bio_pgC_ml, size)

### Apply the mean function to the initial mean counts per ml across
## the three replicates
fr_allImn <- fr_allI %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, size) %>%
  summarize(Imn = mean(bio_pgC_ml),
            n=length(bio_pgC_ml)) %>% 
  ungroup

### Remove unneeded columns, and rename and re-order remaining columns
names(fr_allImn)
fr_allImn <- select(fr_allImn, 
                    event = samp_ev,
                    sample = exp,
                    group = Group,
                    type, sa, la, wi,
                    Imn,
                    size = size)

### Joint fr_allImn with the df that has clearance rates
load("data/Clearance Rates/crAll1CR.Rdata")

### Join the experimental sample df with the control means df
fr_cr_all <- 	left_join(crAll1CR, fr_allImn, 
                        by = c("event", "group", "type", "sa", "la", "wi", "size"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.



### Take the median clearance rate so I can compare taxa and sampling events
crfrAll1FR <- rowwise(crAll1CR) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imn))

### Calculate the median clearance rates across the E samples
### Apply the median function to the CR across the three replicates
crAll1CR_test <- crAll1CR %>% 
  group_by(event, sample, group, type, sa, la, wi) %>%
  summarize(CRmed = median(CR)) 

### Remove all taxa with NA in the CRmed
crAll1CR_testE_noNA <- crAll1CR_test %>%
  drop_na(CRmed)

### Get rid of the CRmed column
crAll1cR_test_final <-crAll1CR_testE_noNA %>% 
  group_by(group, type, sa, la, wi) %>%
  summarize(Drop = median(CRmed)) 
crAll1cR_test_final <-  subset(crAll1cR_test_final,
                               select = c(group, type, sa, la, wi))
### Get rid of the duplicated rows
duplicated(crAll1cR_test_final) ## none are duplicated

### Save to excel and then make a sheet with the unique taxa by name and size
write_xlsx(crAll1cR_test_final, "data/Clearance Rates/crAll1cR_test_final.xlsx")


#2/2/23
###________________CR FOR ONE SAMPLING EVENT___________________________
## Use crAll1CR as base since it has the CR for all already calulated

ybp2CR <- crAll1CR %>% 
  filter(event == "YBP2") %>% 
  drop_na(CR)
write_xlsx(ybp2CR, "data/Clearance Rates/Feeding Rates/ybp2CR.xlsx")

### 2/2/23
###_____________Now calculate CR for biomass, not counts _____________###

load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
## Create the base data frame 
cr_biomass <- volbio_all_cr %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, Group, type, sa, la, wi, bio_per_vol_pgc_ml, counts_per_ml, size)

### Create another df from the above, with only the control samples
cr_biomassC <- cr_biomass %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
cr_biomassCmn <- cr_biomassC %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, counts_per_ml, size) %>%
  summarize(Cmn = mean(bio_per_vol_pgc_ml),
            n=length(bio_per_vol_pgc_ml)) %>% 
  ungroup

### Create a df with only experimental samples
cr_biomassE <- cr_biomass %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, counts_per_ml, size) %>% 
  ungroup

### Join the experimental sample df with the control means df
cr_biomassCE <- 	left_join(cr_biomassE, cr_biomassCmn, 
                           by = c("samp_ev", "Group", "type", "sa", "la", "wi"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(cr_biomassCE)
cr_biomassCR <- select(cr_biomassCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = Group,
                   type, sa, la, wi,
                   bpm = bio_per_vol_pgc_ml,
                   Cmn,
                   size = size.x)

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
cr_bio_all <- rowwise(cr_biomassCR) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = bpm))
save(crAll1CR, file = "data/Clearance Rates/crAll1CR.Rdata")
write_xlsx(crAll1CR, "data/Clearance Rates/crAll1CR.xlsx")



  


###________________EXTRA CODE FOR LITTLE EXPERIMENTS___________________________-

###Make a list of all the different taxa
##  taxa

###________ This below is extra code from when I was figuring things out
####______The hash-tagged out code below was for correcting an error, which is done.
### This returned 1932 obs when it should have been 1950, indicating that there
## are some duplicates somewhere, so I added the n=length so I can find the error.
#table(crAllCmn$n)
## There are 18 duplicated rows, as the table returned 3/1914 and 6/18.

### Look at the CR for the major groups
names(crAll1CR)
unique(crAll1CR$group)

### Find the median clearance rate by major groups; omitting NA CR rows
allGroupTestCr <- subset(crAll1CR, select = -wi)
allGroupTestCr <- drop_na(allGroupTestCr)
allGroupTestCr <- allGroupTestCr %>% 
  group_by(group) %>%
  summarize(mdCR = median(CR),
            n=length(CR)) %>% 
  ungroup
write_xlsx(allGroupTestCr, "data/Clearance Rates/allGroupTestCr.xlsx")

### Find the mean clearance rate by major groups; omitting NA CR rows
allGroupTestCrmn <- subset(crAll1CR, select = -wi)
allGroupTestCrmn <- drop_na(allGroupTestCrmn)
allGroupTestCrmn <- allGroupTestCrmn %>% 
  group_by(group) %>%
  summarize(mnCR = mean(CR),
            n=length(CR)) %>% 
  ungroup
write_xlsx(allGroupTestCrmn, "data/Clearance Rates/allGroupTestCrmn.xlsx")



#crAllCmntable <- crAllCmn %>% 
#  filter(n == 6)
#write_xlsx(crAllCmntable, "data/Clearance Rates/crAllCmntable.xlsx")
### Duplicates are found in the excel of the df:

### Curious about synura/ochrphyte, maybe get rid of it
unique(volbio_all_cr$Group)
ochro <- volbio_all_cr %>%
  filter(Group == "ochrophyte",
         str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)
### If you want just one sampling event, add to the filter argument:
##  samp_ev =="SJR1", or any other sampa_ev

### Create another df from the above, with only the control samples
ochroC <- ochro %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
ochroCmn <-ochroC %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Create a df with only cone ciliate experimental samples
ochroE <- ochro %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz)
### without 0 

### Join the experimental sample df with the control means df
ochroCE <- 	left_join(ochroE, ochroCmn, by = c("samp_ev", "grp_sz"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_eve" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(ochroCE)
ochroCE <- select(ochroCE, 
                   event = samp_ev,
                   sample = exp.x,
                   rep,
                   group = grp_sz,
                   cpm = counts_per_ml,
                   Cmn)

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
ochroCR <- rowwise(ochroCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
### Note: 0 cpm in E samples; 2 occurrences in C samples
## Maybe add to flagellates

### ___________________Repeat for tintinnids

unique(volbio_all_cr$Group)
tin <- volbio_all_cr %>%
  filter(Group == "tintinnid",
         str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep, grp_sz, counts_per_ml)
### If you want just one sampling event, add to the filter argument:
##  samp_ev =="SJR1", or any other sampa_ev

### Create another df from the above, with only the control samples
tinC <- tin %>% 
  filter(exp == "C") 

### Apply the mean function to the control mean counts per ml across
## the three replicates
tinCmn <-tinC %>% 
  group_by(samp_ev, exp, grp_sz) %>%
  summarize(Cmn = mean(counts_per_ml))

### Create a df with only cone ciliate experimental samples
tinE <- tin %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, grp_sz)
### without 0 
tinE[tinE$counts_per_ml != 0, ]
tinC[tinC$counts_per_ml != 0, ]
tinCmn_no0 <- tinCmn %>% 
  filter(Cmn != 0)
### Join the experimental sample df with the control means df
tinCE <- 	left_join(tinE, tinCmn, by = c("samp_ev", "grp_sz"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_eve" is not necessary.

### Remove unneeded columns, and rename and re-order remaining columns
names(tinCE)
tinCE <- select(tinCE, 
                  event = samp_ev,
                  sample = exp.x,
                  rep,
                  group = grp_sz,
                  cpm = counts_per_ml,
                  Cmn)

### Calculate clearance rates
source("scripts/01_function_clearanceRates.R")
tinCR <- rowwise(tinCE) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn, expCt = cpm))
### Note: 0 cpm in E samples; 2 occurrences in C samples
## Maybe add to flagellates

