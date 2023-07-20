################################################################################
######### COPY OF BIOMASS CLEARANCE RATE CODE ##################################
################################################################################

### Not needed because clearance rate does not need to be calculated using
##  biomass, since it is a calcultion of the concentration of cells and
## the "volume swept clear"
##  But I wanted to save this just in case I need it later, because when
##  I compared this CR with the cells CR, it was slightly different, and
##  I thought it should have been the same, if it's a rate.

### __________________ BY BIOMASS ______________________________
### _________________CLEARANCE RATE ____________________________

load("data/Clearance Rates/sumAll.Rdata") ### This df was made above and is the
##  base file for calculating the CR and FR for small and large groups

## Create the base data frame that has only the controls and experimentals,
##  using either counts per ml or biomass per ml
sumAllBpm_CE <- sumAll %>%
  filter( str_detect(exp, "C|E")) %>% 
  select(samp_ev, exp, rep,Group, size, bio_ugC_l)

### Create another df from the above, with only the control samples
sumAllBpm_C <- sumAllBpm_CE %>% 
  filter(exp == "C") 

### Create a df with only experimental samples
sumAllBpm_E <- sumAllBpm_CE %>% 
  filter(exp == "E") %>% 
  group_by(samp_ev, exp, rep, Group, bio_ugC_l, size) %>% 
  ungroup
names(sumAllBpm_E)[names(sumAllBpm_E) == 'bio_ugC_l'] <- 'bio_ugC_l_E'

### Apply the mean function to the controls df to get control mean counts or 
##  biomass per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sumAllBpm_Cmn <- sumAllBpm_C %>% 
  group_by(samp_ev, exp, Group, size) %>%
  summarize(Cmn_bio = mean(bio_ugC_l)) %>% 
  ungroup

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sumAllBpmE_Cmn <- 	left_join(sumAllBpm_E, sumAllBpm_Cmn, 
                             by = c("samp_ev", "Group", "size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumAllBpmE_Cmn)
sumAllBpmE_Cmn <- select(sumAllBpmE_Cmn, 
                         event = samp_ev,
                         group = Group,
                         size,
                         bio_ugC_l_E,
                         Cmn_bio)

### Calculate clearance rates. The resulting data fram includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sumAllBpm_cr <- rowwise(sumAllBpmE_Cmn) %>% 
  mutate(CR = cr_func(controlMnCt = Cmn_bio, expCt = bio_ugC_l_E))
### Replace the NAs with 0 because if not, when R calculates the mean, if one of the reps has NA, but
##  the other one or two have a number, R will not calculate it and will return NA
sumAllBpm_cr <- sumAllBpm_cr %>% 
  mutate(CR = ifelse(is.na(CR), 0, CR))
save(sumAllBpm_cr, file = "data/Clearance Rates/sumAllBpm_cr.Rdata")
write_xlsx(sumAllBpm_cr, "data/Clearance Rates/sumAllBpm_cr.xlsx")
load("data/Clearance Rates/sumAllBpm_cr.Rdata")

###  Take the mean of the CR
crmnsize_sumAllBpm_cr <- sumAllBpm_cr %>% 
  group_by(event, group, size) %>% 
  summarize(crMnBpl = mean(CR))
write_xlsx(crmnsize_sumAllBpm_cr, "data/Clearance Rates/crmnsize_sumAllBpm_cr.xlsx")
save(crmnsize_sumAllBpm_cr, file = "data/Clearance Rates/crmnsize_sumAllBpm_cr.Rdata")


### __________ CLEARANCE RATE CPM AND BPL IN THE SAME DF _________________-
##  BPL is biomass in µgC per Liter
load("data/Clearance Rates/crmnsize_sumAllCpm_cr.Rdata")
load("data/Clearance Rates/crmnsize_sumAllBpm_cr.Rdata")
CRcpmBpl <- left_join(crmnsize_sumAllCpm_cr, crmnsize_sumAllBpm_cr,
                      by = c("event", "group", "size"))

### Add the group_size column
CrGrps <- CRcpmBpl 
## Make a new column named "group_size" that combines the names of the group column
##   and the size column
CrGrps$group_size <- (paste(CrGrps$group, CrGrps$size))

CrGrps["group_size"][CrGrps["group_size"] == "centricDiatom large"] <- "CenDiaLg"
CrGrps["group_size"][CrGrps["group_size"] == "centricDiatom small"] <- "CenDiaSm"
CrGrps["group_size"][CrGrps["group_size"] == "chlorophyte large"] <- "ChlLg"
CrGrps["group_size"][CrGrps["group_size"] == "chlorophyte small"] <- "ChlSm"
CrGrps["group_size"][CrGrps["group_size"] == "ciliate large"] <- "CilLg"
CrGrps["group_size"][CrGrps["group_size"] == "ciliate small"] <- "CilSm"
CrGrps["group_size"][CrGrps["group_size"] == "cyanobacteria large"] <- "CyanoLg"
CrGrps["group_size"][CrGrps["group_size"] == "cyanobacteria small"] <- "CyanoSm"
CrGrps["group_size"][CrGrps["group_size"] == "dinoflagellate large"] <- "DinoLg"
CrGrps["group_size"][CrGrps["group_size"] == "dinoflagellate small"] <- "DinoSm"
CrGrps["group_size"][CrGrps["group_size"] == "flagellate large"] <- "FlagLg"
CrGrps["group_size"][CrGrps["group_size"] == "flagellate small"] <- "FlagSm"
CrGrps["group_size"][CrGrps["group_size"] == "pennateDiatom large"] <- "PenDiaLg"
CrGrps["group_size"][CrGrps["group_size"] == "pennateDiatom small"] <- "PenDiaSm"
CrGrps["group_size"][CrGrps["group_size"] == "unidentified large"] <- "UnidLg"
CrGrps["group_size"][CrGrps["group_size"] == "unidentified small"] <- "UnidSm"

save(CrGrps, file =  "data/Clearance Rates/CrGrps.Rdata")
save(CrGrps, file = "Notes/CR_Groups/CrGrps.Rdata")
write_xlsx(CrGrps, "data/Clearance Rates/CrGrps.xlsx")
