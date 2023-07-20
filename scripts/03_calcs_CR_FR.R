############################################################################
############### CALCULATE CLEARANCE RATE AND INGESTION RATE  ###############
############################################################################

### 4/11/23 After finding an error in the original file (03_calcs_CrFr_bySizeSmLg.R)
##  in summing the cpm per samp_ev per group_size, I corrected and cleaned up the 
##  CR and IR code here. NOTE: 4/12/23, There was no error.
## NOTE: 4/13/23, I found an error in the µg C conversion and updated the df
## NOTE: 4/24/23, After meeting with Wim yesterday, I removed the chain diatoms
##  from the pennate group_size and created a group_size, ChnDiaLg and Sm of their
##  own, and so needed to re-run the CR and IR code with those taxa group changes. 
## NOTE: 4/27/23, at the end, see code for calculating the top 5 taxa groups' IR
##  by replicate

### Notes:
##  CR = V/T * (ln(mean control samples) - ln(experimental samples))/n
##  Clearance rate = (Volume of experimental container--595 ml)/ Time of experiment *
##  (ln of the mean of the control samples cpm) - (ln of the experimental samples cpm)/ 
##  the number of copepods in the experimental bottle. 
##  Units of the result are mL per copepod per day

### Then calculate the initial samples cpm or biomass to be used to calculate ingestion 
##  feeding) rate, I = F x mean IC, Ingestion rate = Clearance rate  * 
##  the mean initial samples (counts or biomass)
##  Then calculate the Ingestion Rate
### Ingestion Rates units are in cells per copepod per day, or pgC  or µg C biomass per copepod per day,
##  I need to also calculate the median clearance rate for putting into the
##  feeding rate equation.


library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
source("scripts/01_function_clearanceRates.R")
source("scripts/01_function_feedingRate.R")

### DFs created here:
load("data/Clearance Rates/sumCpm_cr.Rdata") #Clearance Rates with reps and means
load("data/Clearance Rates/sumCpm_CRmn.Rdata") # Clearance Rates means only
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata") #Ingestion Rates,
### biomass, means only, µg C and pg C
load( "data/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only

### ____________  CREATE THE BASE DATA FRAME ___________________

base <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, group_size, cpm, bio_pgC_ml)
save(base, file = "data/Clearance Rates/base.Rdata")
load(base,)
###### _________________CLEARANCE RATE _________________________###
##################################################################

### Sum up the cpm for the 15 group_size groups, adding all cpm for organisms
##  that fall into those 15 categories, such as, all the small centric diatoms
##  in a sampling event, experimental bottle
sumCpm <- base %>% 
  group_by(samp_ev, group_size, exp, rep) %>% 
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

### Create a df with only initial samples (for the ingestion rates)
sumCpm_I <- sumCpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sumCpm_Cmn <- sumCpm_C %>% 
  group_by(samp_ev, group_size, exp) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sumCpmE_Cmn <- 	left_join(sumCpm_E, sumCpm_Cmn, 
                             by = c("samp_ev", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
##  Do I need the exp column?--Can't keep it since E is one column and C
## is another column
names(sumCpmE_Cmn)
sumCpmE_Cmn <- select(sumCpmE_Cmn, 
                         event = samp_ev,
                         group_size, rep, cpmE,
                         Cmn)

### Calculate clearance rates. The resulting data frame includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sumCpm_cr <- rowwise(sumCpmE_Cmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = Cmn, expCt = cpmE))

### Replace the NAs with 0 because if not, when R calculates the mean, if one of the reps has NA, but
##  the other one or two have a number, R will not calculate it and will return NA
sumCpm_cr <- sumCpm_cr %>% 
  mutate(CRmlcd = ifelse(is.na(CRmlcd), 0, CRmlcd))
save(sumCpm_cr, file = "data/Clearance Rates/sumCpm_cr.Rdata")
write_xlsx(sumCpm_cr, "data/Clearance Rates/sumCpm_cr.xlsx")
load("data/Clearance Rates/sumCpm_cr.Rdata")

###  Take the mean of the CR
sumCpm_CRmn <- sumCpm_cr %>% 
  group_by(event, group_size) %>% 
  summarize(CrMNmlcd = mean(CRmlcd))
write_xlsx(sumCpm_CRmn, "data/Clearance Rates/sumCpm_CRmn.xlsx")
save(sumCpm_CRmn,file =  "data/Clearance Rates/sumCpm_CRmn.Rdata")
load("data/Clearance Rates/sumCpm_CRmn.Rdata")

### Sum the CR means across all sampling events per group_size
sumCRmnAll <- sumCpm_CRmn %>% 
  group_by(group_size) %>% 
  summarise(MeanCR = mean(CrMNmlcd),
            .groups = 'drop') %>% 
  as.data.frame()
write_xlsx(sumCRmnAll, "data/Clearance Rates/sumCRmnAll.xlsx")

### Sum the CR means by sampling event, no taxa groups
sumCRmnEventsOnly <- sumCpm_CRmn %>% 
  group_by(event) %>% 
  summarise(MeanCR = mean(CrMNmlcd),
            .groups = 'drop') %>% 
  as.data.frame()

### _________ Make a df that combines rep CR with mean CR_________________
###_______________________________________________________________________
### Use this to plot the CR means with the reps so I can see if the outliers
##  have one crazy CR that's pulling up or down the means
names(sumCpm_cr)
names(sumCpm_CRmn)
CR_Rep_Mn <- 	left_join(sumCpm_cr, sumCpm_CRmn, 
                        by = c("event", "group_size"))
save(CR_Rep_Mn, file= "data/Clearance Rates/CR_Rep_Mn.Rdata")
load("data/Clearance Rates/CR_Rep_Mn.Rdata")

###### ###### ###### ###### 
###### Clearance Rate with taxa groups ordered in large and small grouping
###### ###### ###### ###### 

load("data/Clearance Rates/sumCpm_CRmn.Rdata")
CR_Size <- sumCpm_CRmn %>% 
  mutate(group_size = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(CR_Size, file = "data/Clearance Rates/CR_Size.Rdata")
write_xlsx(CR_Size, "data/Clearance Rates/CR_Size.xlsx")
### Need to take the mean of the CR per taxon across all events
CR_allEvents <- CR_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalCRmn = mean(CrMNmlcd)) %>% 
  ungroup()
### See 04_plots_IR_Various.R for plotting it.




################################################################################
### _________INGESTION (FEEDING) RATE BY BIOMASS ______________________________
################################################################################

### Sum up the biomass, pgC mL^1 (Bpm) for the 15 group_size groups, adding all bpm for 
##  organisms that fall into those 15 categories, such as all the small centric diatoms
##  in a sampling event, experimental bottle. Need to use pgC mL^1 because CR are in units
## of mL copepod^1 d^1, so the biomass units need to correspond. If I want to plot in 
## terms of ugC L^1, I can do the conversion then.

## Create the base data frame that has only the biomass 
sumBpm <- base %>% 
  group_by(samp_ev, group_size, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sumBpm_I <- sumBpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, group_size, TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumBpm_Imn <- sumBpm_I %>% 
  group_by(samp_ev, exp, group_size) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_Imn)
sumBpm_Imn <- select(sumBpm_Imn, 
                        event = samp_ev, exp, group_size, ImnBpm)

### Join sumBpm_Imn with the df that has clearance rates, sumCpm_cr
load("data/Clearance Rates/sumCpm_cr.Rdata")
sumBpm_cr_Imn <- 	left_join(sumCpm_cr, sumBpm_Imn, 
                              by = c("event", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_cr_Imn)
sumBpm_cr_Imn <- select(sumBpm_cr_Imn, 
                          event, group_size, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumBpm_FR <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sumBpm_FR <- sumBpm_FR %>% 
  rename("FRpgCcd" = "FR")
save(sumBpm_FR, file = "data/Clearance Rates/Feeding Rates/sumBpm_FR.Rdata")
write_xlsx(sumBpm_FR, "data/Clearance Rates/Feeding Rates/sumBpm_FR.xlsx")
load("data/Clearance Rates/Feeding Rates/sumBpm_FR.Rdata")

### Calculate the means across the groups and sizes
BpmFRMn_GrpSz <- sumBpm_FR %>%
  group_by(event, group_size) %>%
  summarise(FRmnpgCcd = mean(FRpgCcd),
            .groups = 'drop') %>% 
  as.data.frame()
save(BpmFRMn_GrpSz, file = "data/Clearance Rates/Feeding Rates/BpmFRMn_GrpSz.Rdata")
write_xlsx(BpmFRMn_GrpSz, "data/Clearance Rates/Feeding Rates/BpmFRMn_GrpSz.xlsx")

## Add a column with the conversion from pg to µg
BugFRMn_GrpSz <- BpmFRMn_GrpSz %>% 
  group_by(event, group_size, FRmnpgCcd) %>%
  summarise(FRmnUgCcd = FRmnpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 
save(BugFRMn_GrpSz, file = "data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata")
write_xlsx(BugFRMn_GrpSz, "data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.xlsx")
load("data/Clearance Rates/Feeding Rates/BugFRMn_GrpSz.Rdata")

### ______ Make a df that combines rep FR (biomass) with mean FR (biomass) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
load("data/Clearance Rates/Feeding Rates/BpmFRMn_GrpSz.Rdata")
load("data/Clearance Rates/Feeding Rates/sumBpm_FR.Rdata")
names(sumBpm_FR)
names(BpmFRMn_GrpSz)
FRbpm_Rep_Mn <- 	left_join(sumBpm_FR, BpmFRMn_GrpSz, 
                           by = c("event", "group_size"))
FRbpm_Rep_Mn <- FRbp
save(FRbpm_Rep_Mn, file= "data/Clearance Rates/Feeding Rates/FRbpm_Rep_Mn.Rdata")

### ______________ADD A COLUMN OF INGESTION RATE IN µg C per copepod per day________________
### for the means and the reps
##  1µg = 1,000,000 pg
##  1 µgC per copepod per day  = 1 pgC per copepod per day/ 1,000,000  (1 pgC)/(1 ml) * (1000 ml)/(1 L) * (0.000001 µg)/(1 pgC) =  0.001 µg L-1

# for Ingestion Rate per rep
FRBio_Rep_Mn <- FRbpm_Rep_Mn %>% 
  group_by(event, group_size, FRpgCcd, FRmnpgCcd) %>%
  reframe(FRUgCcd = FRpgCcd/1000000) 
#For ingestion rate means
FRBio_Rep_Mn <- FRBio_Rep_Mn %>% 
  group_by(event, group_size, FRpgCcd, FRmnpgCcd, FRUgCcd) %>%
  reframe(FRmnUgCcd = FRmnpgCcd/1000000) 
# Group together the means both units and the reps both units
FRBio_Rep_Mn <- FRBio_Rep_Mn %>% 
  select(event, group_size, FRpgCcd, FRUgCcd, FRmnpgCcd, FRmnUgCcd)

save(FRBio_Rep_Mn, file = "data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata")
write_xlsx(FRBio_Rep_Mn, "data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.xlsx")
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata")


################################################################################
#_____________ Make a df of IR bio totals by sampling event ____________________
################################################################################
FRBio_Event <- FRBio_Rep_Mn %>% 
  group_by(event, group_size) %>% 
  summarise(FRmnUgCcd = mean(FRmnUgCcd),
            .groups = 'drop') %>% 
  as.data.frame()

FRBio_Event
  sumCpm_Imn <- sumCpm_I %>% 
  group_by(samp_ev, group_size) %>%
  summarise(Imn = mean(cpmI),
            .groups = 'drop') %>% 
  rename(event = samp_ev) %>% 
  as.data.frame()
  
####################
########## Ingestion rate, cells, combined totals, events only #########
####################
IRbio_eventsOnly <- BugFRMn_GrpSz %>% 
  group_by(event) %>%
  summarise(IRbioMn = mean(FRmnUgCcd),
            .groups = 'drop') %>% 
  as.data.frame()
save(IRbio_eventsOnly, file = "data/Clearance Rates/Feeding Rates/IRbio_eventsOnly.Rdata")
write_xlsx(IRbio_eventsOnly, "data/Clearance Rates/Feeding Rates/IRbio_eventsOnly.xlsx")
load("data/Clearance Rates/Feeding Rates/IRbio_eventsOnly.Rdata")
IRbio_eventsTotals <- BugFRMn_GrpSz %>% 
  group_by(event) %>%
  summarise(IRbioMn = sum(FRmnUgCcd),
            .groups = 'drop') %>% 
  as.data.frame()
save(IRbio_eventsTotals, file = "data/Clearance Rates/Feeding Rates/IRbio_eventsTotals.Rdata")
write_xlsx(IRbio_eventsTotals, "data/Clearance Rates/Feeding Rates/IRbio_eventsTotals.xlsx")
################################################################################
### _________ CLEARANCE RATE WITH INGESTION RATE BY BIOMASS ____________________
################################################################################
CR_IRbio_mn <- 	left_join(sumCpm_CRmn, BugFRMn_GrpSz, 
                            by = c("event", "group_size"))
save(CR_IRbio_mn, file = "data/Clearance Rates/CR_IRbio_mn.Rdata")
write_xlsx(CR_IRbio_mn, "data/Clearance Rates/CR_IRbio_mn.xlsx")
load("data/Clearance Rates/CR_IRbio_mn.Rdata")


################################################################################
####_____________INGESTION (FEEDING) RATE BY CELLS (COUNTS, cpm) __________
################################################################################

### Run the base data frame code (created above, near the top) that has the initial samples
sumCpm_I <- sumCpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumCpm_Imn <- sumCpm_I %>% 
  group_by(samp_ev, group_size) %>%
  summarise(Imn = mean(cpmI),
            .groups = 'drop') %>% 
  rename(event = samp_ev) %>% 
  as.data.frame()

### Join sumCpm_Imn with the df that has clearance rates, sumCpm_cr,
##  reorder columns so CR is the end column
load("data/Clearance Rates/sumCpm_cr.Rdata")
names(sumCpm_cr)
names(sumCpm_Imn)
sumCpm_cr_fr <- 	left_join(sumCpm_cr, sumCpm_Imn, 
                           by = c("event", "group_size"))# %>% 
#select(event, group_size, rep, cpmE, Cmn,Imn, CR)
#  "select" is not needed if I want to include all columns, R automatically adds all unless specified

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumCpm_FR <- rowwise(sumCpm_cr_fr) %>% 
  mutate(FRCellsCd = fr_func(CR=CRmlcd, initialMnCt = Imn))
save(sumCpm_FR, file = "data/Clearance Rates/Feeding Rates/sumCpm_FR.Rdata")
write_xlsx(sumCpm_FR, "data/Clearance Rates/Feeding Rates/sumCpm_FR.xlsx")

load("data/Clearance Rates/Feeding Rates/sumCpm_FR.Rdata")

### Take the mean FR for group and size, across the 3 reps, i.e., CenDiaLg in LSZ2
CpmFRMn_GrpSz <- sumCpm_FR %>%
  group_by(event, group_size) %>%
  summarise(FRmnCellsCd = mean(FRCellsCd),
            .groups = 'drop') %>% 
  as.data.frame()
save(CpmFRMn_GrpSz, file = "data/Clearance Rates/Feeding Rates/CpmFRMn_GrpSz.Rdata")
load("data/Clearance Rates/Feeding Rates/CpmFRMn_GrpSz.Rdata")


### ______ Make a df that combines rep FR (cells) with mean FR (cells) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
### This still needs work as of 4/4/23, because I think I need the rep numbers
names(sumCpm_FR)
names(CpmFRMn_GrpSz)
FRCells_Rep_Mn <- 	left_join(sumCpm_FR, CpmFRMn_GrpSz, 
                             by = c("event", "group_size"))
save(FRCells_Rep_Mn, file= "data/Clearance Rates/Feeding Rates/FRCells_Rep_Mn.Rdata")

####################
########## Ingestion rate, cells, combined totals, events only #########
####################
IRcells_eventsOnly <- CpmFRMn_GrpSz %>% 
  group_by(event) %>% 
  summarise(IRcellsMn = mean(FRmnCellsCd),
            .groups = 'drop') %>% 
  as.data.frame()  
load(IRcells_eventsOnly, file= "data/Clearance Rates/Feeding Rates/IRcells_eventsOnly.Rdata")


### Look at the chain diatom group_size to see what the numbers are like
ChnDiaFRBio <- FRBio_Rep_Mn %>% 
  filter(group_size =="ChnDiaLg" | group_size =="ChnDiaSm")
write_xlsx(ChnDiaFRBio, "data/Clearance Rates/Feeding Rates/ChnDiaFRBio.xlsx")

a <- ggplot(data=ChnDiaFRBio, aes(event, FRmnUgCcd)) +
  geom_hline(yintercept=0, color="gray", linewidth=1) +
  geom_point(aes(color = FRmnUgCcd>0), size = 4)+
  scale_color_manual(values=c("FALSE"="maroon","TRUE"="navy"))+
  #geom_point(data = ChnDiaFRBio,
            # aes(y=FRUgCcd),
             #color="orange", size=2)+
  #xlab("Taxon Group, -centric diatoms") +
  ggtitle("Chain Diatom Biomass Ingestion Rates")+
  scale_y_continuous()+#limits = c(-.05, .09), 
                    # breaks = c(0.3, 0, .01, .06, .07, .08)) +
  ylab("µg C"~copepod^-1~d^-1)+
  wimGraph()+
  theme(plot.title = element_text(face = "bold", size = (12)),
        axis.text.x = element_text(angle = 60, hjust = 0.8, vjust = 0.8, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 11.5),
        axis.title.x = element_text(size = 11.5),
        legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1.5))
a

### Look at pennate diatoms IR bio now that the chain diatoms were removed from 
##  their group and note the change in IR. Chain diatoms only appeared in
##  YBP2 (IRbio 0.07) and LSZ2 (IR bio 0.001)
PenDiaIRbio <- BugFRMn_GrpSz %>% 
  filter(group_size == "PenDiaSm" | group_size == "PenDiaLg")
write_xlsx(PenDiaIRbio, "data/Clearance Rates/Feeding Rates/PenDiaIRbio.xlsx")

### And CR pennates and chains
PenDiaCR <- sumCpm_CRmn %>% 
  filter(group_size == "PenDiaSm" | group_size == "PenDiaLg")

#####################################################################
############### SORT BY SMALL AND LARGE GROUPS #####################
#####################################################################
## Use these dfs:
load("data/Clearance Rates/Feeding Rates/FRBio_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
load( "data/Clearance Rates/CR_IRbio_mn.Rdata") # Clearance Rates and Ingestion

### Ingestion Rates, biomass, Means only
FR_bio_Size <- FRBio_Rep_Mn %>% 
  mutate(group_size = factor(group_size,
         levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
         "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
         "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
          "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(FR_bio_Size, file = "data/Clearance Rates/Feeding Rates/FR_bio_Size.Rdata")
write_xlsx(FR_bio_Size, "data/Clearance Rates/Feeding Rates/FR_bio_Size.xlsx")
### Need to take the mean of the FRbio per taxon across all events
FRbio_allEvents <- FR_bio_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalFRbioMn = mean(FRmnUgCcd)) %>% 
  ungroup()
save(FRbio_allEvents, file = "data/Clearance Rates/Feeding Rates/FRbio_allEvents.Rdata")
write_xlsx(FRbio_allEvents, "data/Clearance Rates/Feeding Rates/FRbio_allEvents.xlsx")
### See 04_plots_IR_Various.R for plotting it.

### Ingestion rates, cells, Means only
load ("data/Clearance Rates/Feeding Rates/CpmFRMn_GrpSz.Rdata")
FR_cell_Size <- CpmFRMn_GrpSz %>% 
  mutate(group_size = factor(group_size,
                             levels=c("CenDiaLg", "ChlLg", "ChnDiaLg" ,"CilLg",
                                      "CyanoLg", "DinoLg", "FlagLg", "PenDiaLg", "UnidLg", 
                                      "CenDiaSm", "ChlSm", "ChnDiaSm", "CilSm", "CyanoSm", 
                                      "FlagSm", "PenDiaSm", "UnidSm" ))) 
save(FR_cell_Size, file = "data/Clearance Rates/Feeding Rates/FR_cell_Size.Rdata")
write_xlsx(FR_cell_Size, "data/Clearance Rates/Feeding Rates/FR_cell_Size.xlsx")
load("data/Clearance Rates/Feeding Rates/FR_cell_Size.Rdata")

### Take the mean of the IR cells per taxon across all events
IRcells_allEvents <- FR_cell_Size %>% 
  group_by(group_size) %>% 
  summarise(TotalIRcells = mean(FRmnCellsCd)) %>% 
  ungroup()
save(IRcells_allEvents, file = "data/Clearance Rates/Feeding Rates/IRcells_allEvents.Rdata")
### See 04_plots_IR_Various.R for plotting it.

#####################################################################
############### TOP 5 TAXA GROUPS IR REPLICATES #####################
#####################################################################
### Using the code from above, but modify so it keeps the replicate numbers

base <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, group_size, cpm, bio_pgC_ml)
save(base, file = "data/Clearance Rates/base.Rdata")

sumBpm <- base %>% 
  group_by(samp_ev, group_size, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sumBpm_I <- sumBpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, group_size, TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sumBpm_Imn <- sumBpm_I %>% 
  group_by(samp_ev, exp, group_size) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_Imn)
sumBpm_Imn <- select(sumBpm_Imn, 
                     event = samp_ev, exp, group_size, ImnBpm)

### Join sumBpm_Imn with the df that has clearance rates, sumCpm_cr
load("data/Clearance Rates/sumCpm_cr.Rdata")
sumBpm_cr_Imn <- 	left_join(sumCpm_cr, sumBpm_Imn, 
                            by = c("event", "group_size"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sumBpm_cr_Imn)
sumBpm_cr_Imn <- select(sumBpm_cr_Imn, 
                        event, group_size, rep, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sumBpm_FR <- rowwise(sumBpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sumBpm_FR <- sumBpm_FR %>% 
  rename("FRpgCcd" = "FR")
### Filter out everything but the top 5 taxa groups:
##  CilLg, CenDiaLg, CilSm, FlagSm, CenDiaSm
IRbioTop5 <- sumBpm_FR %>% 
  filter(group_size %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 
### Add a column in µg C L^1
IRbioTop5 <- IRbioTop5 %>% 
  group_by(event, group_size, rep, CRmlcd,FRpgCcd) %>%
  summarise(FRUgCcd = FRpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 

### Add the mean CR and FR
### Filter to keep the top 5 in the CR IR mean df
load("data/Clearance Rates/CR_IRbio_mn.Rdata")
Top5CR_IRbio_mn <-CR_IRbio_mn %>% 
  filter(group_size %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 

IRbioCRTop5 <- IRbioTop5 %>% 
  left_join(Top5CR_IRbio_mn, IRbioTop5,
            by = c("event", "group_size"))

save(IRbioCRTop5, file = "data/Clearance Rates/Feeding Rates/IRbioCRTop5.Rdata")
write_xlsx(IRbioCRTop5, "data/Clearance Rates/Feeding Rates/IRbioCRTop5.xlsx")
load("data/Clearance Rates/Feeding Rates/IRbioCRTop5.Rdata")


### Means of cells per mL
#FrCpmOverall <- FrGrps %>%
#  group_by(group_size) %>%
#  summarise( FrCpmAllEvents= mean(FRmnCpm))
#save(FrCpmOverall, file = "data/Clearance Rates/Feeding Rates/FrCpmOverall.Rdata")
#write_xlsx(FrCpmOverall, "data/Clearance Rates/Feeding Rates/FrCpmOverall.xlsx")
