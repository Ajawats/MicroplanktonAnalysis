############################################################################
################ INITIAL MEANS CPM AND FR FUNCTION TESTING #################
############################################################################
### 1/22/23

### This script is for calcuating the means of the initial samples 
##  counts per ml (cpm) and then joining that df with another df of
## the experimental samples counts per ml (individual per replicate)
## and then using that data along with clearance rates to calculate the feeding rates.

### Below is the clearance rate counts per ml, 
##  and below that is the clearance rates biomass

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_feedingRate.R")

### Clearance Rate based on counts per ml
## Create the base data frame with just the initial samples
fr_allI <- volbio_all_cr %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, type, sa, la, wi, counts_per_ml, size)

### Apply the mean function to the initial mean counts per ml across
## the three replicates
fr_allImn <- fr_allI %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, size) %>%
  summarize(Imn = mean(counts_per_ml),
            n=length(counts_per_ml)) %>% 
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

### Calculate feeding rate
source("scripts/01_function_feedingRate.R")

fr_all <- rowwise(fr_cr_all) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imn))
save(fr_all, file = "data/Clearance Rates/Feeding Rates/fr_all.Rdata")
write_xlsx(fr_all, "data/Clearance Rates/Feeding Rates/fr_all.xlsx")

### Remove the NAs in FR
fr_all_nona <- fr_all %>% 
  drop_na(FR)
write_xlsx(fr_all_nona, "data/Clearance Rates/Feeding Rates/fr_all_nona.xlsx")

### Take out only the YBP2 samples
fr_ybp2 <- fr_all %>% 
  filter(event == "YBP2")
fr_ybp2_nona <-fr_all_nona %>% 
  filter(event == "YBP2") %>% 
  subset(select = c(-sample.x, -sample.y)) %>% 
  group_by(event, rep, group, type, sa, la, wi, cpm, Cmn, size, CR, Imn, FR) %>%
  summarise(FRugC = (FR/1000))
write_xlsx(fr_ybp2_nona, "data/Clearance Rates/Feeding Rates/fr_ybp2_nona.xlsx")

###______________Clearance rate and feeding rate based on biomass________________###

load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_feedingRate.R")

### Clearance Rate based on counts per ml
## Create the base data frame with just the initial samples
fr_allI <- volbio_all_cr %>%
  filter(exp == "I")%>%
  select(samp_ev, exp, rep, Group, type, sa, la, wi, counts_per_ml, size)

### Apply the mean function to the initial mean counts per ml across
## the three replicates
fr_allImn <- fr_allI %>% 
  group_by(samp_ev, exp, Group, type, sa, la, wi, size) %>%
  summarize(Imn = mean(counts_per_ml),
            n=length(counts_per_ml)) %>% 
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

### Join fr_allImn with the df that has clearance rates
load("data/Clearance Rates/crAll1CR.Rdata")

### Join the experimental sample df with the control means df
fr_cr_all <- 	left_join(crAll1CR, fr_allImn, 
                        by = c("event", "group", "type", "sa", "la", "wi", "size"))
### If you're calculating for only one sampling even, the "by" should
##  be only "grp_sz". "samp_ev" is not necessary.

### Calculate feeding rate
source("scripts/01_function_feedingRate.R")

fr_all <- rowwise(fr_cr_all) %>% 
  mutate(FR = fr_func(CR=CR, initialMnCt = Imn))
save(fr_all, file = "data/Clearance Rates/Feeding Rates/fr_all.Rdata")
write_xlsx(fr_all, "data/Clearance Rates/Feeding Rates/fr_all.xlsx")

### Remove the NAs in FR
fr_all_nona <- fr_all %>% 
  drop_na(FR)
write_xlsx(fr_all_nona, "data/Clearance Rates/Feeding Rates/fr_all_nona.xlsx")

### Take out only the YBP2 samples
fr_ybp2 <- fr_all %>% 
  filter(event == "YBP2")
fr_ybp2_nona <-fr_all_nona %>% 
  filter(event == "YBP2") %>% 
  subset(select = c(-sample.x, -sample.y)) %>% 
  group_by(event, rep, group, type, sa, la, wi, cpm, Cmn, size, CR, Imn, FR) %>%
  summarise(FRugC = (FR/1000))
write_xlsx(fr_ybp2_nona, "data/Clearance Rates/Feeding Rates/fr_ybp2_nona.xlsx")

