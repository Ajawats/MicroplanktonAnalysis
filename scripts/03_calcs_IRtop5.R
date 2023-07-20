############################################################################
######## CLEARANCE RATE AND INGESTION RATES WITH TOP 5 AND OTHER  ##########
############################################################################


### 4/27/23 See 03_calcs_CR_FR.R for various notes, I copied the code from there.

library(tidyverse)
library(writexl)
load("data/FinalAnalysis/baseTop5.Rdata")
source("scripts/01_function_clearanceRates.R")
source("scripts/01_function_feedingRate.R")

### DFs created here:
load("data/Clearance Rates/sum5Cpm_cr.Rdata") #Clearance Rates with reps and means
load("data/Clearance Rates/sum5Cpm_CRmn.Rdata") # Clearance Rates means only
load("data/FinalAnalysis/FRBio5_Rep_Mn.Rdata") #Ingestion Rates,
### biomass, reps and means, µg C and pg C
load("data/FinalAnalysis/BugFRMn5_GrpSz.Rdata") #Ingestion Rates,
### biomass, means only, µg C and pg C
load( "data/Clearance Rates/CR5_IRbio_mn.Rdata") # Clearance Rates and Ingestion
### Rates, biomass, Means only
load("data/FinalAnalysis/CpmFRMn5_txGp.Rdata") # Ingestion Rates, cells means only
load("data/FinalAnalysis/sum5Cpm_FR.Rdata") # Ingestion Rates, cells meand and reps

### ____________  CREATE THE BASE DATA FRAME ___________________

base <- baseTop5 %>% 
  select(samp_ev, exp, rep, taxaGroup, cpm, bio_pgC_ml)
#save(base, file = "data/Clearance Rates/base.Rdata")

###### _________________CLEARANCE RATE _________________________###
##################################################################

### Sum up the cpm for the 15 taxaGroup groups, adding all cpm for organisms
##  that fall into those 15 categories, such as, all the small centric diatoms
##  in a sampling event, experimental bottle
sum5Cpm <- base %>% 
  group_by(samp_ev, taxaGroup, exp, rep) %>% 
  summarise(TotalCpm=sum(cpm),
            .groups = 'drop') %>% 
  as.data.frame()

## Create the base data frame that has only the control and experimental cpm
sum5Cpm_CE <- sum5Cpm %>%
  filter( str_detect(exp, "C|E"))

### Create another df from the above, with only the control samples
sum5Cpm_C <- sum5Cpm_CE%>% 
  filter(exp == "C") %>% 
  rename(cpmC = TotalCpm)

### Create a df with only experimental samples
sum5Cpm_E <- sum5Cpm_CE%>% 
  filter(exp == "E")%>% 
  rename(cpmE = TotalCpm) 

### Create a df with only initial samples (for the ingestion rates)
sum5Cpm_I <- sum5Cpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm) 

### Apply the mean function to the controls df to get control mean counts 
##  per ml across  the three replicates. Leave out the rep column so 
##  that what remains in the df is one row for each individual organism/size 
##  and the mean of the control sample counts per ml or biomass per ml
sum5Cpm_Cmn <- sum5Cpm_C %>% 
  group_by(samp_ev, taxaGroup, exp) %>% 
  summarise(Cmn=mean(cpmC),
            .groups = 'drop') %>% 
  as.data.frame()

### Since CR needs the mean control samples and the three replicates experimental
## samples, join the experimental sample df with the control means df. This will
##  necessarily include the rep column, since we need the experimental samples
## individual replicate counts or biomass for the calculation.
sum5CpmE_Cmn <- 	left_join(sum5Cpm_E, sum5Cpm_Cmn, 
                          by = c("samp_ev", "taxaGroup"))

### Remove unneeded columns, and rename and re-order remaining columns
##  Do I need the exp column?--Can't keep it since E is one column and C
## is another column
names(sum5CpmE_Cmn)
sum5CpmE_Cmn <- select(sum5CpmE_Cmn, 
                      event = samp_ev,
                      taxaGroup, rep, cpmE,
                      Cmn)

### Calculate clearance rates. The resulting data frame includes all the replicates
## since the CR was calculated for each replicate.
source("scripts/01_function_clearanceRates.R")
sum5Cpm_cr <- rowwise(sum5CpmE_Cmn) %>% 
  mutate(CRmlcd = cr_func(controlMnCt = Cmn, expCt = cpmE))

### Replace the NAs with 0 because if not, when R calculates the mean, if one of the reps has NA, but
##  the other one or two have a number, R will not calculate it and will return NA
sum5Cpm_cr <- sum5Cpm_cr %>% 
  mutate(CRmlcd = ifelse(is.na(CRmlcd), 0, CRmlcd))
save(sum5Cpm_cr, file = "data/Clearance Rates/sum5Cpm_cr.Rdata")
write_xlsx(sum5Cpm_cr, "data/Clearance Rates/sum5Cpm_cr.xlsx")
#load("data/Clearance Rates/sum5Cpm_cr.Rdata")

###  Take the mean of the CR
sum5Cpm_CRmn <- sum5Cpm_cr %>% 
  group_by(event, taxaGroup) %>% 
  summarize(CrMNmlcd = mean(CRmlcd))
write_xlsx(sum5Cpm_CRmn, "data/Clearance Rates/sum5Cpm_CRmn.xlsx")
save(sum5Cpm_CRmn,file =  "data/Clearance Rates/sum5Cpm_CRmn.Rdata")
load("data/Clearance Rates/sum5Cpm_CRmn.Rdata")

### _________ Make a df that combines rep CR with mean CR_________________
###_______________________________________________________________________
### Use this to plot the CR means with the reps so I can see if the outliers
##  have one crazy CR that's pulling up or down the means
names(sum5Cpm_cr)
names(sum5Cpm_CRmn)
CR5_Rep_Mn <- 	left_join(sum5Cpm_cr, sum5Cpm_CRmn, 
                        by = c("event", "taxaGroup"))
save(CR5_Rep_Mn, file= "data/Clearance Rates/CR5_Rep_Mn.Rdata")
load("data/Clearance Rates/CR5_Rep_Mn.Rdata")


################################################################################
### _________INGESTION (FEEDING) RATE BY BIOMASS ______________________________
################################################################################

### Sum up the biomass, pgC mL^1 (Bpm) for the 15 taxaGroup groups, adding all bpm for 
##  organisms that fall into those 15 categories, such as all the small centric diatoms
##  in a sampling event, experimental bottle. Need to use pgC mL^1 because CR are in units
## of mL copepod^1 d^1, so the biomass units need to correspond. If I want to plot in 
## terms of ugC L^1, I can do the conversion then.

## Create the base data frame that has only the biomass 
sum5Bpm <- base %>% 
  group_by(samp_ev, taxaGroup, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sum5Bpm_I <- sum5Bpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, taxaGroup, TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sum5Bpm_Imn <- sum5Bpm_I %>% 
  group_by(samp_ev, exp, taxaGroup) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(sum5Bpm_Imn)
sum5Bpm_Imn <- select(sum5Bpm_Imn, 
                     event = samp_ev, exp, taxaGroup, ImnBpm)

### Join sum5Bpm_Imn with the df that has clearance rates, sum5Cpm_cr
load("data/Clearance Rates/sum5Cpm_cr.Rdata")
sum5Bpm_cr_Imn <- 	left_join(sum5Cpm_cr, sum5Bpm_Imn, 
                            by = c("event", "taxaGroup"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sum5Bpm_cr_Imn)
sum5Bpm_cr_Imn <- select(sum5Bpm_cr_Imn, 
                        event, taxaGroup, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sum5Bpm_FR <- rowwise(sum5Bpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sum5Bpm_FR <- sum5Bpm_FR %>% 
  rename("FRpgCcd" = "FR")
save(sum5Bpm_FR, file = "data/FinalAnalysis/sum5Bpm_FR.Rdata")
write_xlsx(sum5Bpm_FR, "data/FinalAnalysis/sum5Bpm_FR.xlsx")
load("data/FinalAnalysis/sum5Bpm_FR.Rdata")

### Calculate the means across the groups and sizes
BpmFRMn5_txGp <- sum5Bpm_FR %>%
  group_by(event, taxaGroup) %>%
  summarise(FRmnpgCcd = mean(FRpgCcd),
            .groups = 'drop') %>% 
  as.data.frame()
save(BpmFRMn5_txGp, file = "data/FinalAnalysis/BpmFRMn5_txGp.Rdata")
write_xlsx(BpmFRMn5_txGp, "data/FinalAnalysis/BpmFRMn5_txGp.xlsx")

## Add a column with the conversion from pg to µg
BugFRMn5_txGp <- BpmFRMn5_txGp %>% 
  group_by(event, taxaGroup, FRmnpgCcd) %>%
  summarise(FRmnUgCcd = FRmnpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 
save(BugFRMn5_txGp, file = "data/FinalAnalysis/BugFRMn5_txGp.Rdata")
write_xlsx(BugFRMn5_txGp, "data/FinalAnalysis/BugFRMn5_txGp.xlsx")
load("data/FinalAnalysis/BugFRMn5_txGp.Rdata")
### ______ Make a df that combines rep FR (biomass) with mean FR (biomass) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
load("data/FinalAnalysis/BugFRMn5_txGp.Rdata")
load("data/FinalAnalysis/sum5Bpm_FR.Rdata")
names(sum5Bpm_FR)
names(BpmFRMn5_txGp)
FRbpm5_Rep_Mn <- 	left_join(sum5Bpm_FR, BpmFRMn5_txGp, 
                           by = c("event", "taxaGroup"))
save(FRbpm5_Rep_Mn, file= "data/FinalAnalysis/FRbpm5_Rep_Mn.Rdata")

### ______________ADD A COLUMN OF INGESTION RATE IN µg C per copepod per day________________
### for the means and the reps
##  1µg = 1,000,000 pg
##  1 µgC per copepod per day  = 1 pgC per copepod per day/ 1,000,000  (1 pgC)/(1 ml) * (1000 ml)/(1 L) * (0.000001 µg)/(1 pgC) =  0.001 µg L-1

# for Ingestion Rate per rep
FRBio5_Rep_Mn <- FRbpm5_Rep_Mn %>% 
  group_by(event, taxaGroup, FRpgCcd, FRmnpgCcd) %>%
  reframe(FRUgCcd = FRpgCcd/1000000) 
#For ingestion rate means
FRBio5_Rep_Mn <- FRBio5_Rep_Mn %>% 
  group_by(event, taxaGroup, FRpgCcd, FRmnpgCcd, FRUgCcd) %>%
  reframe(FRmnUgCcd = FRmnpgCcd/1000000) 
# Group together the means both units and the reps both units
FRBio5_Rep_Mn <- FRBio5_Rep_Mn %>% 
  select(event, taxaGroup, FRpgCcd, FRUgCcd, FRmnpgCcd, FRmnUgCcd)

save(FRBio5_Rep_Mn, file = "data/FinalAnalysis/FRBio5_Rep_Mn.Rdata")
write_xlsx(FRBio5_Rep_Mn, "data/FinalAnalysis/FRBio5_Rep_Mn.xlsx")
load("data/FinalAnalysis/FRBio5_Rep_Mn.Rdata")


################################################################################
### _________ CLEARANCE RATE WITH INGESTION RATE BY BIOMASS ____________________
################################################################################
CR5_IRbio_mn <- 	left_join(sum5Cpm_CRmn, BugFRMn5_txGp, 
                          by = c("event", "taxaGroup"))
save(CR5_IRbio_mn, file = "data/Clearance Rates/CR5_IRbio_mn.Rdata")
write_xlsx(CR5_IRbio_mn, "data/Clearance Rates/CR5_IRbio_mn.xlsx")
load("data/Clearance Rates/CR5_IRbio_mn.Rdata")
### _________INGESTION (FEEDING) RATE CELLS (COUNTS, cpm) ______________________________
###################################################################################

### Run the base data frame code (created above, near the top) that has the initial samples
sum5Cpm_I <- sum5Cpm%>% 
  filter(exp == "I")%>% 
  rename(cpmI = TotalCpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sum5Cpm_Imn <- sum5Cpm_I %>% 
  group_by(samp_ev, taxaGroup) %>%
  summarise(Imn = mean(cpmI),
            .groups = 'drop') %>% 
  rename(event = samp_ev) %>% 
  as.data.frame()

### Join sum5Cpm_Imn with the df that has clearance rates, sum5Cpm_cr,
##  reorder columns so CR is the end column
load("data/Clearance Rates/sum5Cpm_cr.Rdata")
names(sum5Cpm_cr)
names(sum5Cpm_Imn)
sum5Cpm_cr_fr <- 	left_join(sum5Cpm_cr, sum5Cpm_Imn, 
                           by = c("event", "taxaGroup"))# %>% 
#select(event, taxaGroup, rep, cpmE, Cmn,Imn, CR)
#  "select" is not needed if I want to include all columns, R automatically adds all unless specified

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sum5Cpm_FR <- rowwise(sum5Cpm_cr_fr) %>% 
  mutate(FRCellsCd = fr_func(CR=CRmlcd, initialMnCt = Imn))
save(sum5Cpm_FR, file = "data/FinalAnalysis/sum5Cpm_FR.Rdata")
write_xlsx(sum5Cpm_FR, "data/FinalAnalysis/sum5Cpm_FR.xlsx")

load("data/FinalAnalysis/sum5Cpm_FR.Rdata")

### Take the mean FR for group and size, across the 3 reps, i.e., CenDiaLg in LSZ2
CpmFRMn5_txGp <- sum5Cpm_FR %>%
  group_by(event, taxaGroup) %>%
  summarise(FRmnCellsCd = mean(FRCellsCd),
            .groups = 'drop') %>% 
  as.data.frame()
save(CpmFRMn5_txGp, file = "data/FinalAnalysis/CpmFRMn5_txGp.Rdata")


### ______ Make a df that combines rep FR (cells) with mean FR (cells) ___________
###_______________________________________________________________________
### Use this to plot the FR means with the reps so I can see if the outliers
##  have one crazy FR that's pulling up or down the means
### This still needs work as of 4/4/23, because I think I need the rep numbers
names(sum5Cpm_FR)
names(CpmFRMn5_txGp)
FRCells5_Rep_Mn <- 	left_join(sum5Cpm_FR, CpmFRMn5_txGp, 
                             by = c("event", "taxaGroup"))
save(FRCells5_Rep_Mn, file= "data/FinalAnalysis/FRCells5_Rep_Mn.Rdata")



### Look at the chain diatom taxaGroup to see what the numbers are like
ChnDiaFRBio <- FRBio_Rep_Mn %>% 
  filter(taxaGroup =="ChnDiaLg" | taxaGroup =="ChnDiaSm")
write_xlsx(ChnDiaFRBio, "data/FinalAnalysis/ChnDiaFRBio.xlsx")

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
  filter(taxaGroup == "PenDiaSm" | taxaGroup == "PenDiaLg")
write_xlsx(PenDiaIRbio, "data/FinalAnalysis/PenDiaIRbio.xlsx")

### And CR pennates and chains
PenDiaCR <- sum5Cpm_CRmn %>% 
  filter(taxaGroup == "PenDiaSm" | taxaGroup == "PenDiaLg")

#####################################################################
############### TOP 5 TAXA GROUPS IR REPLICATES #####################
#####################################################################
### Using the code from above, but modify so it keeps the replicate numbers

base <- baseTop5 %>% 
  select(samp_ev, exp, rep, taxaGroup, cpm, bio_pgC_ml)
save(base, file = "data/Clearance Rates/base.Rdata")

sum5Bpm <- base %>% 
  group_by(samp_ev, taxaGroup, exp, rep) %>% 
  summarise(TotalBpm=sum(bio_pgC_ml),
            .groups = 'drop') %>% 
  as.data.frame()

###data frame that has only the initial samples
sum5Bpm_I <- sum5Bpm %>%
  filter(exp == "I")%>%
  select(samp_ev, exp,rep, taxaGroup, TotalBpm)

### Apply the mean function to the initial mean counts per ml across
## the three replicates, as done in the CR
sum5Bpm_Imn <- sum5Bpm_I %>% 
  group_by(samp_ev, exp, taxaGroup) %>%
  summarise(ImnBpm = mean(TotalBpm),
            .groups = 'drop') %>% 
  as.data.frame()

### Remove unneeded columns, and rename and re-order remaining columns
names(Bpm_Imn)
sum5Bpm_Imn <- select(sum5Bpm_Imn, 
                     event = samp_ev, exp, taxaGroup, ImnBpm)

### Join sum5Bpm_Imn with the df that has clearance rates, sum5Cpm_cr
load("data/Clearance Rates/sum5Cpm_cr.Rdata")
sum5Bpm_cr_Imn <- 	left_join(sum5Cpm_cr, sum5Bpm_Imn, 
                            by = c("event", "taxaGroup"))

### Remove unneeded columns, and rename and re-order remaining columns
names(sum5Bpm_cr_Imn)
sum5Bpm_cr_Imn <- select(sum5Bpm_cr_Imn, 
                        event, taxaGroup, rep, ImnBpm, CRmlcd)

### Calculate ingestion (feeding) rate
source("scripts/01_function_feedingRate.R")

sum5Bpm_FR <- rowwise(sum5Bpm_cr_Imn) %>% 
  mutate(FR = fr_func(CR=CRmlcd, initialMnCt = ImnBpm))
### Rename the FR column to FRpgCmL so I remember it's in those units
sum5Bpm_FR <- sum5Bpm_FR %>% 
  rename("FRpgCcd" = "FR")
### Filter out everything but the top 5 taxa groups:
##  CilLg, CenDiaLg, CilSm, FlagSm, CenDiaSm
IRbioTop5 <- sum5Bpm_FR %>% 
  filter(taxaGroup %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 
### Add a column in µg C L^1
IRbioTop5 <- IRbioTop5 %>% 
  group_by(event, taxaGroup, rep, CRmlcd,FRpgCcd) %>%
  summarise(FRUgCcd = FRpgCcd/1000000,
            .groups = 'drop') %>% 
  as.data.frame() 

### Add the mean CR and FR
### Filter to keep the top 5 in the CR IR mean df
load("data/Clearance Rates/CR_IRbio_mn.Rdata")
Top5CR_IRbio_mn <-CR_IRbio_mn %>% 
  filter(taxaGroup %in%  
           c("CilLg", "CenDiaLg", "CilSm", "FlagSm", "CenDiaSm")) 

IRbioCRTop5 <- IRbioTop5 %>% 
  left_join(Top5CR_IRbio_mn, IRbioTop5,
            by = c("event", "taxaGroup"))

save(IRbioCRTop5, file = "data/FinalAnalysis/IRbioCRTop5.Rdata")
write_xlsx(IRbioCRTop5, "data/FinalAnalysis/IRbioCRTop5.xlsx")
load("data/FinalAnalysis/IRbioCRTop5.Rdata")


### Means of cells per mL
#FrCpmOverall <- FrGrps %>%
#  group_by(taxaGroup) %>%
#  summarise( FrCpmAllEvents= mean(FRmnCpm))
#save(FrCpmOverall, file = "data/FinalAnalysis/FrCpmOverall.Rdata")
#write_xlsx(FrCpmOverall, "data/FinalAnalysis/FrCpmOverall.xlsx")
