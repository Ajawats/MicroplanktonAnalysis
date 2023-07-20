############################################################################
############ CODE FOR OTHER EXPLORATORY INVESTIATIONS #####################
############################################################################

### 2/22/23

library(tidyverse)
library(writexl)
load("data/Clearance Rates/volbio_all_cr.Rdata")
load("data/Clearance Rates/cr_cpm_CR.Rdata")

### What are all the group and type names?
grp_typ_names<- volbio_all_cr %>% 
  select(grp_typ)
duplicated(grp_typ_names)
grptypnames_unique <- grp_typ_names[!duplicated(grp_typ_names$grp_typ),]
write_xlsx(grptypnames_unique, "Groups Collapse/grp_typ_names.xlsx")

### Where does ochrophyte occur in the samples?
ochro <- volbio_all_cr %>% 
  filter(Group == "ochrophyte") %>% 
  select(samp_ev, exp, rep, Group, type,  
         counts, cpm, size) %>% 
subset(cpm !=0)
write_xlsx(ochro, "Groups Collapse/ochro.xlsx")

### Answer to above: they only occurr in SJR1 site, C and I, and YBP2 I samples,
## so they have no CR.
##  Add them to flagellates


### Where does dinoflagellate occur in the samples?
dino <- volbio_all_cr %>% 
  filter(Group == "dinoflagellate") %>% 
  select(samp_ev, exp, rep, Group, type,  
         counts, cpm, size) %>% 
  subset(cpm !=0)
write_xlsx(dino, "Groups Collapse/dino.xlsx")

dinoCR <- cr_cpm_CR %>% 
  filter(group == "dinoflagellate") %>% 
  select(event, sample, rep, group, type, size, cpmE=cpm, Cmn, CR) %>% 
  subset(cpmE | Cmn !=0)
write_xlsx(dinoCR, "data/Clearance Rates/Groups/dinoCR.xlsx")

### Find out what proportion the tintinnids are of the ciliates
tinCil <- volbio_all_cr %>% 
  filter(Group %in% c("ciliate", "tintinnid")) %>% 
  select(samp_ev, exp, rep, Group, type, counts, tot_vol_um3, tot_biomass_pgC) %>% 
  subset(counts !=0)

table(tinCil$Group)

### Find out what proportion the flagchloro are of the unidentified
flagchl <- volbio_all_cr %>% 
  filter(Group %in% c("unidentified", "flagchloro")) %>% 
  select(samp_ev, exp, rep, Group, type, counts, cpm, tot_vol_um3, tot_biomass_pgC) %>% 
  subset(counts !=0)

table(flagchl$type)
###  flagchloro      other      round 
#           25         78        221  

flagchloro <- flagchl %>% 
  filter(type == "flagchloro")
write_xlsx(flagchloro, "data/Clearance Rates/Groups/flagchloro.xlsx")

mean(flagchloro$cpm)

### Get the mean biomass and cpm for each existing group

### CPM
cpm_mean_groups <- volbio_all_cr %>% 
  aggregate(cpm ~ grp_typ, FUN = mean)
write_xlsx(cpm_mean_groups, "data/Clearance Rates/Groups/cpm_mean_groups.xlsx")

### BPM
bpm_mean_groups <- volbio_all_cr %>% 
  aggregate(bio_pgC_ml ~ grp_typ, FUN = mean)
write_xlsx(bpm_mean_groups, "data/Clearance Rates/Groups/bpm_mean_groups.xlsx")

# mean(df[df$team == 'A', 'points'])

## Error code with this: argument is not numeric or logical: returning NA
## mean(volbio_all_cr[volbio_all_cr$type == "actinastrum", "cpm"])

### Try this: df %>% filter(Gender == "M", Job == "E") %>% summarize(Avg = mean(Age))

mn_test <- volbio_all_cr %>% filter (Group == "unidentified", type == "flagchloro") %>%
  select(samp_ev, Avg) %>% 
  summarize(Avg = mean(cpm)) %>% 
  subset(Avg !=0)

### Check the cpm_mean_groups results by calculating just chlor actin cpm
chlact <- volbio_all_cr %>% 
  select(grp_typ, counts, cpm) %>% 
  filter(grp_typ == "chlorophyte actinastrum") %>% 
  subset(counts !=0)
mean(chlact$cpm)
### result is 2.28672, but in cpm_mean_groups it's 0.304895960

### Mean counts per ml by group-type
grpTyp <- volbio_all_cr %>% 
  select(grp_typ, counts, cpm) %>% 
  subset(counts !=0)
grpTypMn <- grpTyp %>% 
  group_by(grp_typ) %>% 
  summarise_at(vars(cpm), list(Mncpm = mean))
### This one worked, don't know why the other didn't
write_xlsx(grpTypMn, "data/Clearance Rates/Groups/grpTypMn.xlsx")

### Mean biomass pgC per ml by group-type
grpTyp <- volbio_all_cr %>% 
  select(grp_typ, counts, cpm, bio_pgC_ml) %>% 
  subset(counts !=0)
grpTypMn_bpm <- grpTyp %>% 
  group_by(grp_typ) %>% 
  summarise_at(vars(bio_pgC_ml), list(Mnbpm = mean))
### This one worked, don't know why the other didn't
write_xlsx(grpTypMn_bpm, "data/Clearance Rates/Groups/grpTypMn_bpm.xlsx")

### Check tintinnid agglutinated biomass
tinagg <- volbio_all_cr %>% 
  select(samp_ev, exp, rep, grp_typ, counts, bio_pgC_ml) %>% 
  filter(grp_typ == "tintinnid agglutinated") %>% 
  subset(counts !=0)
mean(chlact$cpm)
### result is 2.28672, but in cpm_mean_groups it's 0.304895960




