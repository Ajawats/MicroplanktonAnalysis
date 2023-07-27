############################################################################
################### BIOMASS MEANS AND COUNTS TOTALS ########################
############################################################################

### 11/09/22, 10/25/22 (originally created 9/2/22, duplicated and edited from
##  script, Biomass_Means_Final.R)

###  This code is for calculating the total counts of each
##   organism and the mean biomass per organism/sample.
### Also includes the creation of the "category" column, which combines
##  group, type, sa, la, wi, into one column.

##  I'm taking this code below and using the Rdata file, 
##  volbio_all.Rdata (from the script, 03_calcs_volbio_100400.R)
##  as the source file, because it was recently 
##  updated and includes all the necesarry columns. Also trying
##  to streamline all the crazy amounts of scripts I have

### 9/2/22
##   Uses bmass_final_exp  (biomass for everything EXCEPT site water samples)
##   from Biomass_All_Final.R. It contains all the columns needed for
##  nearly all the calculations (18). This is the cleaned up version 
##   originally written in Bmass_Mn_Ct.R, which contains lots of notes.


library(tidyverse)
library(readxl)
library(writexl)


load("data/MasterFiles/MasterRFiles/volbio_all.Rdata")
names(volbio_all)

### Calculate total counts of specific organism per sample, sums up the total 
##  counts of the reps of that organism per samp_ev
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5)

### 11/9/22 I re-coded volbio_all so that I could have a column that
## puts together the group-type name with the short axis and long axis
## measurements.
### __________ I added the code below to 03_calcs_volbio_100400.R so don't need to here
### Add several columns to volbio_all, that put group categories together
## sa and la, so that the dimensions are together
## grp-typ so that the organism group and type are together
## grp_sz, so that the organism group and type and size are together
## grp_esd, so tht the organsim group and type and esd are together

#volbio_all$size <- (paste(volbio_all$sa, volbio_all$la)) 

#volbio_all$grp_typ <- (paste(volbio_all$Group, 
#                                  volbio_all$type))
#volbio_all$grp_sz <- (paste(volbio_all$grp_typ, 
#                                 volbio_all$size))
#volbio_all$grp_esd <- (paste(volbio_all$grp_typ,
 #                            volbio_all$esd))

#save(volbio_all, file = "data/MasterFiles/MasterRFiles/volbio_all.Rdata")
#write_csv(volbio_all, "data/MasterFiles/volbio_all.csv")

### This file below might need to be re-coded for other analyses, since
## above, I changed the volbio_all file to include a group/size column
### 11/17/22 I changed the summarize to sum the counts per mL instead of total
## counts, since after a discussion with Wim yesterday, I realized that the
## volumes settled varied too much to be able to compare the raw counts
names(volbio_all)

count_sum_rep_fin_sz <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp, rep, grp_sz, mag)%>% 
  summarize(tot_ct_ml = sum(counts_per_ml)) %>%
  #summarize(tot_ct = sum(counts)) %>% 
  ungroup()

### Make another data frame that includes the grp_typ instead of the grp_sz
count_sum_rep_fin_grptyp <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp,rep, grp_typ, mag)%>% 
  summarize(tot_ct_ml = sum(counts_per_ml)) %>%
  #summarize(tot_ct = sum(counts)) %>% 
  ungroup()

### Make another data frame that includes the esd column and the grpesd
## Updated 12/22 to group by the szeds column, that has the group, type, sz and esd
count_sum_rep_fin_grpesd <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp, rep, grp_sz, esd, grp_esd, szesd, mag)%>% 
  summarize(tot_ct_ml = sum(counts_per_ml)) %>%
  #summarize(tot_ct = sum(counts)) %>% 
  ungroup()
save(count_sum_rep_fin_grpesd, file ="data/MasterFiles/MasterRFiles/count_sum_rep_fin_grpesd.Rdata")
## Use count_sum_rep_fin_grpesd to calculate the means of the totalCPM across
## the replicates, using this file because later in the code, I used a file
## from which I removed the IC and site water samples
ctsum_rep_mn_esd <- count_sum_rep_fin_grpesd %>%
  group_by(samp_ev, exp, grp_esd, )%>%
  summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))
save(ctsum_rep_mn_esd, file = "data/MasterFiles/MasterRFiles/ctsum_rep_mn_esd.Rdata")
### Remove all rows that have 0 means b/c if the mean is 0, then there
##  were none in the replicates either
ctsum_rep_mnesd_no0 = filter(ctsum_rep_mn_esd, mn_ct_ml !=0)
save(ctsum_rep_mnesd_no0, file = "data/MasterFiles/MasterRFiles/ctsum_rep_mnesd_no0.Rdata")

### Make a file that removes IC and site
ctsum_rep_sz_fct24 <- subset(count_sum_rep_fin_sz, exp!= "site") %>% 
  subset(exp!="IC")
### And another one using count_sum_rep_fin_grpesd
ctsum_rep_esd_fct24 <- subset(count_sum_rep_fin_grpesd, exp!= "site") %>% 
  subset(exp!="IC")

### Calculate the mean counts across the replicates--by keeping all the rows
##  that need to match, and by removing the "rep"
##  column from the group_by function, it averages the counts across the
##  replicates for the sampling event, FC or T24 and the grp_sz organism
#ctsum_rep_mn <- ctsum_rep_sz_fct24
#ctsum_rep_mn <- ctsum_rep_sz_fct24 %>%
  #group_by(samp_ev, exp, grp_sz, )%>%
  #summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))
# this is the example of how to do the above line of code
# summarise_at(vars(tot_ct), list(average = mean))

### And do this with count_sum_rep_fin_grpesd: This is done above
#ctsum_rep_mn_esd <- count_sum_rep_fin_grpesd
#ctsum_rep_mn_esd <- count_sum_rep_fin_grpesd %>%
 #group_by(samp_ev, exp, grp_esd, )%>%
 #summarise_at(vars(tot_ct_ml), list(mn_ct_ml = mean))


### Remove all rows that have 0 means b/c if the mean is 0, then there
##  were none in the replicates either

### When re-running this code, check code above for ctsum_rep_mn, as I had
##  previously removed the IC and site water samples
#ctsum_rep_mn_no0 = filter(ctsum_rep_mn, mn_ct_ml !=0)
#save(ctsum_rep_mn_no0, file = "data/ctsum_rep_mn_no0.Rdata")
#write_csv(ctsum_rep_mn_no0, "data/ctsum_rep_mn_no0.csv")

### And do this for ctsum_rep_mn_esd
### But use  as source, since it includes all samples
### This is done above around line 93
#ctsum_rep_mnesd_no0 = filter(ctsum_rep_mn_esd, mn_ct_ml !=0)
#save(ctsum_rep_mnesd_no0, file = "data/ctsum_rep_mnesd_no0.Rdata")
#write_csv(ctsum_rep_mnesd_no0, "data/ctsum_rep_mnesd_no0.csv")
                                   
#ctsum_rep_mn %>%     # Specify data frame
  #group_by(grp_sz) %>%                         # Specify group indicator
  #summarise_at(vars(tot_ct),              # Specify column
             #  list(name = mean))

#sapply(split.default(df1, sub("\\..*", "", names(df1))), rowMeans)
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

## Calculate the mean biomass of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5)

bmean_rep_fin_sz <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp, Group, type, sa, la, wi, mag)%>% 
  summarize(bmn_pgml = mean(bio_per_vol_pgc_ml)) %>% 
  ungroup()

## Format the biomass column so that it has no decimal places

library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
bmean_rep_fin_sz$bmn_pgml<-formattable(bmean_rep_fin_sz$bmn_pgml,format="f",digits=0)
#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Left_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin <- left_join(count_sum_rep_fin, bmean_rep_fin_sz)
save(ct_bmn_fin, file = "data/ct_bmn_fin.Rdata")
write_xlsx(ct_bmn_fin, "ct_bmn_fin.xlsx")

## Format the biomass column so that it has no decimal places
## Note: this seemed to change the format it was shown in, but when
## saving as an excel doc, it showed many decimal places, so I
## just changed the decimal places on that column. So I'm not sure
## this is the best function for this.


## Combine group, type, sa, la, wi, into one column and call it "category"
ct_bmn_fin$category <- paste(ct_bmn_fin$Group, ct_bmn_fin$type,
                             ct_bmn_fin$sa, ct_bmn_fin$la)

### Get rid of the columns I no longer need and order the columns
all_sta_categories <- select(ct_bmn_fin, -Group, -type, -sa, -la, -wi) %>% 
  select(samp_ev, exp, category, tot_ct, bmn_pgml)
save(all_sta_categories,  file = "all_sta_categories")
load("all_sta_categories")
write_xlsx(all_sta_categories, "all_sta_categories.xlsx")

## Make a file with just one station
#sjr1_categories <- all_sta_categories %>% 
  #filter(samp_ev == "SJR1")
## Get a list of all the unique organism categories
#unique(sjr1_categories$category)

## Ciliates only, just for fun
#sjr1_cil <- sjr1_categories %>% 
  #filter(grepl("ciliate", category))

#save(sjr1_categories, file = "sjr1_categories")
#write_xlsx(sjr1_categories, "sjr1_categories.xlsx")

############################################################################
## SAME CALCULATIONS BUT THE THE COMPLETE DATA CONTAINING THE ZERO COUNTS ##
############################################################################

### Do everything but using the Rdata file, 
##  volbio_all.Rdata (from the script, 03_calcs_volbio_100400.R)
##  as the source file

load("data/volbio_all.Rdata")
names(volbio_all)

## Calculate total counts of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5)
count_sum_rep_fin_all <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp, Group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

## Calculate the mean biomass of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5)

bmean_rep_fin_all <- volbio_all %>% 
  group_by(samp_date, samp_ev, exp, Group, type, sa, la, wi)%>% 
  summarize(bmn_pgml = mean(bio_per_vol_pgc_ml)) %>% 
  ungroup()

## Format the biomass column so that it has no decimal places

library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
bmean_rep_fin_all$bmn_pgml<-formattable(bmean_rep_fin_all$bmn_pgml,format="f",digits=0)
#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Left_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin_all <- left_join(count_sum_rep_fin_all, bmean_rep_fin_all)
save(ct_bmn_fin_all, file = "data/ct_bmn_fin_all.Rdata")
write_xlsx(ct_bmn_fin_all, "ct_bmn_fin_all.xlsx")

## Format the biomass column so that it has no decimal places
## Note: this seemed to change the format it was shown in, but when
## saving as an excel doc, it showed many decimal places, so I
## just changed the decimal places on that column. So I'm not sure
## this is the best function for this.


## Combine group, type, sa, la, wi, into one column and call it "category"
ct_bmn_fin_all$category <- paste(ct_bmn_fin_all$Group, ct_bmn_fin_all$type,
                             ct_bmn_fin_all$sa, ct_bmn_fin_all$la)

### Get rid of the columns I no longer need and order the columns
all_sta_categories_all <- select(ct_bmn_fin_all, -Group, -type, -sa, -la, -wi) %>% 
  select(samp_ev, exp, category, tot_ct, bmn_pgml)
save(all_sta_categories_all,  file = "results/all_sta_categories_all.Rdata")
load("results/all_sta_categories_all.Rdata")
write_xlsx(all_sta_categories_all, "results/all_sta_categories.xlsx")


