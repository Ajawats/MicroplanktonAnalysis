##############################################################################
############ SITE WATER SAMPLES ABUNDANCE (Counts and Biomass) ###############
##############################################################################

### 8/14/23 See notes about the site water rel biomass plot in Thesis Project and Analysis Notes.docx
##  or in my notebook, p. 85, re: discussion with Wim in meeting 8/12
### Includes code to get cpm totals for initials and experimentals to plot and compare with site water
### 8/11/23 updated and re-ran with updated volbio_all_cr
### 3/8/23

library(tidyverse)
library(writexl)
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")
### When returning to this code to run it again, load:
load("data7_24/SiteWater/siteCntBio.Rdata")

### Make a df with just site water, add columns with cpm and biomass totals per 
##  event per taxaGroup
siteCntBio <- volbio_all_cr %>%
  filter(exp == "S") %>% 
  select(samp_ev, exp, counts, cpm, bio_pgC_ml, bio_ugC_l, 
         taxaGroup=group_size) %>% 
  group_by(samp_ev, taxaGroup) %>% 
  mutate(tot_cpmTaxa = sum(cpm),
         tot_bio_ugTaxa = sum(bio_ugC_l)) %>% 
  ungroup

save(siteCntBio, file =  "data7_24/SiteWater/siteCntBio.Rdata")
write_xlsx(siteCntBio, "data7_24/SiteWater/siteCntBio.xlsx")
load("data7_24/SiteWater/siteCntBio.Rdata")

### Make a separate df with just the cpm totals per event per taxaGroup
## Counts per mL
siteCpmTot <- siteCntBio %>% 
  group_by(samp_ev, taxaGroup) %>% 
  summarise(tot_cpmTaxa = sum(cpm))
min(siteCpmTot$tot_cpm)
max(siteCpmTot$tot_cpm)
save(siteCpmTot, file = "data7_24/SiteWater/siteCpmTot.Rdata")

### Make a separate df with just the biomass µgC per liter totals per event per taxaGroup
### Biomass, ugC per L
siteBioUgLTot  <- siteCntBio %>% 
  group_by(samp_ev, taxaGroup) %>% 
  summarise(tot_bio_ug = sum(bio_ugC_l))
min(siteBioUgLTot$tot_bio_ug)
max(siteBioUgLTot$tot_bio_ug)
save(siteBioUgLTot, file = "data7_24/SiteWater/siteBioUgLTot.Rdata")

### Calculate the proportions of each
siteCntBioProp <- siteCntBio %>% 
  filter(bio_ugC_l !=0)
siteCntBioProp <- siteCntBioProp %>% 
  group_by(samp_ev) %>% 
  mutate(totCpmEv = sum(tot_cpmTaxa)) %>%
           ungroup
siteCntBioProp <- siteCntBioProp %>% 
  group_by(samp_ev) %>% 
  mutate(totBioUgEv = sum(tot_bio_ugTaxa)) %>%
  ungroup

siteCntBioProp <- siteCntBioProp %>% 
  select(samp_ev, taxaGroup, tot_cpmTaxa, tot_cpmTaxa, totCpmEv, 
         tot_bio_ugTaxa, totBioUgEv)
siteCntBioProp <- siteCntBioProp %>%
  distinct()

siteCntBioProp <- siteCntBioProp %>%
  mutate(propCpm = tot_cpmTaxa/totCpmEv,
         propBioUgL = tot_bio_ugTaxa/totBioUgEv)
### Test the numbers
sitePropTest <- siteCntBioProp %>% 
  filter(samp_ev == "LSZ2")
  

################# Plot the Site Water #############################
source("scripts/01_function_wimGraph and Palettes.R")
### CPM, proportion
plot_siteCpmProp <- ggplot(siteCntBioProp, aes(fill=taxaGroup, y=propCpm, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Counts per mL, Site Water Samples")+
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()

### Bio ug L, proportion
plot_siteBioProp <-ggplot(siteCntBioProp, aes(fill=taxaGroup, y=propBioUgL, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Biomass, Site Water Samples")+
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()
plot_siteBioProp


############### INTIALS AND EXPERIMENTALS CPM TOTALS #########################
############################## INITIALS #################
### Make the base df with just initials
load("data7_24/MasterFiles/MasterRFiles/volbio_all_cr.Rdata")
initialsCpm <- volbio_all_cr %>%
  filter(exp == "I") %>% 
  select(samp_ev, exp, rep, Group, type, shp, sa, la, wi, grp_sz, grp_esd, counts, cpm, taxaGroup=group_size)

### take the mean of the 3 replicates of each organism in thr grp_sz column,
##  because that is the most distinct grouping
IcpmTot <- initialsCpm %>% 
  group_by(samp_ev, Group, type, shp, sa, la, wi, grp_sz) %>% 
  mutate(cpmTot = mean(cpm))
### Remove the unneeded columns
IcpmTotno0 <- IcpmTot %>% 
  subset(select = c(samp_ev, grp_sz, taxaGroup, cpmTot)) %>% 
  filter(cpmTot != 0)
### Remove the rows that are "duplicates" because I only want to keep one
##  mean value, not the mean value for all three replicates
IcpmTotno01 <- IcpmTotno0 %>% distinct()
### Now sum all the replicate means in the same taxaGroup
IcpmTot2 <- IcpmTotno01 %>% 
  group_by(samp_ev, taxaGroup) %>% 
  mutate(TotCpmTx = sum(cpmTot))
### Add a column of total cpm, all taxa, per sampling event
IcpmTot2allTxperEvent <- IcpmTot2 %>% 
  group_by(samp_ev) %>% 
  mutate(cpmTotallTxperEv = sum(cpmTot)) %>% 
  ungroup
### test
ybp2TxAllEvTest <- IcpmTot2allTxperEvent %>% 
  filter(samp_ev == "YBP2") %>% 
  summarise(totCpmallTx = sum(cpmTot))
### that's accurate
### Remove the columns again like above
IcpmTot3 <- IcpmTot2allTxperEvent %>% 
  subset(select= c(-grp_sz, -cpmTot))
### Remove the "duplicate" rows like above
IcpmTot4 <- IcpmTot3 %>% distinct()
### Calculate proportions
### Calculate the proportion of the total FR UgC that each group_size contributed to total
IcpmProp <- IcpmTot4 %>% 
  mutate(IcpmProp = TotCpmTx/cpmTotallTxperEv)
### Test IcpmProp
testIcpmProp <- IcpmProp %>% 
  filter(samp_ev =="YBP2")
### summed the prop column, it checks out, sums to 1
save(IcpmProp, file = "Final Final/Abundance/Cells/IcpmProp.Rdata")
write_xlsx(IcpmProp, "Final Final/Abundance/Cells/IcpmProp.xlsx")

### Plot to compare with site water
source("scripts/01_function_wimGraph and Palettes.R")

plot_IcpmProp <-ggplot(IcpmProp, aes(fill=taxaGroup, y=IcpmProp, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Initial Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()


############################## EXPERIMENTALS ##################################
### Make the base df with just initials
expCpm <- volbio_all_cr %>%
  filter(exp == "E") %>% 
  select(samp_ev, exp, rep, Group, type, shp, sa, la, wi, grp_sz, grp_esd, counts, cpm, taxaGroup=group_size)

### take the mean of the 3 replicates of each organism in thr grp_sz column,
##  because that is the most distinct grouping
EcpmTot <- expCpm %>% 
  group_by(samp_ev, Group, type, shp, sa, la, wi, grp_sz) %>% 
  mutate(cpmTot = mean(cpm))
### Remove the unneeded columns
EcpmTotno0 <- EcpmTot %>% 
  subset(select = c(samp_ev, grp_sz, taxaGroup, cpmTot)) %>% 
  filter(cpmTot != 0)
### Remove the rows that are "duplicates" because I only want to keep one
##  mean value, not the mean value for all three replicates
EcpmTotno01 <- EcpmTotno0 %>% distinct()
### Now sum all the replicate means in the same taxaGroup
EcpmTot2 <- EcpmTotno01 %>% 
  group_by(samp_ev, taxaGroup) %>% 
  mutate(TotCpmTx = sum(cpmTot))
### Remove the columns again like above
EcpmTot3 <- EcpmTot2 %>% 
  subset(select= c(-grp_sz, -cpmTot))
### Remove the "duplicate" rows like above
EcpmTot4 <- EcpmTot3 %>% distinct()

### Plot to compare with site water
source("scripts/01_function_wimGraph and Palettes.R")

plot_EcpmProp <-ggplot(EcpmTot4, aes(fill=taxaGroup, y=TotCpmTx, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Experimental Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()

################### PLOT ALL THREE WITH PATCHWORK #######################
library(patchwork)
plot_cpmPropAll <- plot_EcpmProp / plot_IcpmProp / plot_siteCpmProp
+ plot_layout(ncol=2)
plot_cpmPropAll



load("data7_24/SiteWater/siteCpmTot.Rdata")
### Plot site water
source("scripts/01_function_wimGraph and Palettes.R")

ggplot(siteCpmTot, aes(fill=taxaGroup, y=tot_cpm, x=samp_ev)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("CenDiaLg" = "cornflowerblue", "CenDiaSm" = "lightskyblue", "CilLg" = "salmon3", "CilSm" = "salmon1",
                               "FlagLg" = "#85B22C", "FlagSm" = "#c3E57E", "PenDiaLg"= "pink4", "PenDiaSm" = "pink3",
                               "ChlSm" = "#CC8E51", "ChnDiaLg" = "peachpuff", "UnidLg" = "plum4", "UnidSm" = "plum2"),
                    limits = c("CenDiaLg", "CenDiaSm", "CilLg", "CilSm", "FlagSm", "FlagLg", "PenDiaLg", "PenDiaSm",
                               "ChnDiaLg", "ChlSm", "UnidLg", "UnidSm"),
                    name = "Taxa Group")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Taxa Group Relative Counts per mL, Site Water Samples")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 6))+
  wimGraph()




expsCpm <- volbio_all_cr %>%
  filter(exp == "E") %>% 
  select(samp_ev, exp, rep, counts, cpm, taxaGroup=group_size)

EcpmTot <- expsCpm %>% 
  group_by(samp_ev, taxaGroup) %>% 
  summarise(cpmTot = mean(cpm))

### Accuracy check LSZ2 CilLg cpm
lsz2IcilLgcpm <- initialsCpm %>% 
  filter(samp_ev == "LSZ2", taxaGroup == "CilLg") %>% 
  summarise(cpmTot = mean(cpm))

### Make separate plots for each of the sampling events
## Cells
siteCntTotLSZ2 <- siteCntTot %>% 
  filter(samp_ev =="LSZ2")

siteCntTotSJR1 <- siteCntTot %>% 
  filter(samp_ev =="SJR1")

siteCntTotSJR2 <- siteCntTot %>% 
  filter(samp_ev =="SJR2")

siteCntTotWLD2 <- siteCntTot %>% 
  filter(samp_ev =="WLD2")

siteCntTotYBP1 <- siteCntTot %>% 
  filter(samp_ev =="YBP1")

siteCntTotYBP2 <- siteCntTot %>% 
  filter(samp_ev =="YBP2")

## Biomass
siteBioUgLTotLSZ2 <- siteBioUgLTot %>% 
  filter(samp_ev =="LSZ2")

siteBioUgLTotSJR1 <- siteBioUgLTot %>% 
  filter(samp_ev =="SJR1")

siteBioUgLTotSJR2 <- siteBioUgLTot %>% 
  filter(samp_ev =="SJR2")

siteBioUgLTotWLD2 <- siteBioUgLTot %>% 
  filter(samp_ev =="WLD2")

siteBioUgLTotYBP1 <- siteBioUgLTot %>% 
  filter(samp_ev =="YBP1")

siteBioUgLTotYBP2 <- siteBioUgLTot %>% 
  filter(samp_ev =="YBP2")
