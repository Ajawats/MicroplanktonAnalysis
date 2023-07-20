### 8/5/22
##  This file is the cleaned version of MeansTests.R, that takes the excel data that has
##   the biomass calculations, for calculating the mean biomass per organism/sample.
##   Uses bmass_final as the foundational data frame, created in. It contains all the 
##   columns needed for nearly all the calculations (18)
##   First, I made a data frame that sums up the total counts of organism/sample,count_sum_rep_fin
##   Then I made a data frame that calculates the mean biomass of organism/sample,
##   Then I did a left_join to combine them.

## Extra note: I created the carbio.Rdata file in the MeansTests.R script,
##   but it is commented out right now, along with the Bquick function
##  carbio was made so that I could have all the raw data, excluding the
##   site water samples. After creating carbio, I realized I needed to make
##  separate columns for the sampling event names, experimental sample names,
##   and replicates, such as SJR1 IC-1, needing to be separated into
##   SJR1, IC, 1, and so on. I made this change in the carbio excel document.
##   However, on 8/8/22 or so, I realized it was not practical to make those changes
##   manually on the excel doc, for the entire data set, 1905 observations!
##   So I worked on it for a whole day, and did it in the file, 100x_BioPerVol.R,
##   and named it bmass_fin

### 8/9/22, At the end of this code, I will apply these counts sums and biomass means 
##   to the bmass_fin file

library(tidyverse)
library(readxl)
library(writexl)

#carbio <- read_excel("carbio.xlsx")
 
## Calculate total counts of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5) You have to leave out the rep
##  column, so that it won't keep it in the output, since what I want to do
##  is group all the reps together instead of separately
# count_sum_rep <- carbio %>% 
# group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
# summarize(tot_ct = sum(counts)) %>% 
# ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

## Calculate the mean biomass of specific organism per sample 
##  (i.e., SJR1 FC centri diatom 24.5 x 24.5)

#bmean_rep <- carbio %>% 
# group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
# summarize(bmn = mean(bmass_ugCl)) %>% 
# ungroup()
#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Right_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

#ct_bmn <- left_join(count_sum_rep, bmean_rep)
#write_xlsx(ct_bmn, "ct_bmn.xlsx")

## Make separate data frames for sampling events

#sjr1_ctbmn <- filter(ct_bmn, "SJR1" == samp_ev) 
#write_xlsx(sjr1_ctbmn, "SJR1_Ct_Bmn.xlsx")

### This below is the counts sums and mean biomass using bmass_fin,
##   which contains all the data

## Calculate total counts of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5)

#count_sum_rep_fin <- bmass_final %>% 
#  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi, pres_fact, vol_set_ml, n_bugs, bio_per_vol_pgc_ml)%>% 
 # summarize(tot_ct = sum(counts)) %>% 
  #ungroup()
load("data/bmass_final.Rdata")
names(bmass_final)
load("bmass_final_exp")
count_sum_rep_fin <- bmass_final %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

## Calculate the mean biomass of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5), being sure to keep
##  all the columns necessary for doing the clearance rates--!!! NO!!
## This didn't output the means, maybe because it was trying to still
##  keep the specific data in the other columns.

bmean_rep_fin <- bmass_final %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(bmn = mean(bio_per_vol_pgc_ml)) %>% 
  ungroup()
#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Right_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin <- left_join(count_sum_rep_fin, bmean_rep_fin)
write_xlsx(ct_bmn_fin, "ct_bmn_fin.xlsx")

## Do all this below using bmass_final_exp, for the data frame that
##  doesn't have site water samples
load("bmass_final_exp")
count_sum_rep_fin_exp <- bmass_final_exp %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

bmean_rep_fin_exp <- bmass_final_exp %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(bmn_pgcml = mean(bio_per_vol_pgc_ml)) %>%
  ungroup()

## Format the biomass column so that it has no decimal places
## Note: this seemed to change the format it was shown in, but when
## saving as an excel doc, it showed many decimal places, so I
## just changed the decimal places on that column. So I'm not sure
## this is the best function for this.

library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
bmean_rep_fin_exp$bmn_pgcml<-formattable(bmean_rep_fin_exp$bmn_pgcml,format="f",digits=0)
bmean_rep_fin_exp


#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Right_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin_exp <- left_join(count_sum_rep_fin_exp, bmean_rep_fin_exp)

save(ct_bmn_fin_exp, file = "ct_bmn_fin_exp")
write_xlsx(ct_bmn_fin_exp, "ct_bmn_fin_exp.xlsx")


## 8/29/22, Now combine group, type, sa, la, wi, into one column and call it "category"
ct_bmn_fin_exp$category <- paste(ct_bmn_fin_exp$group, ct_bmn_fin_exp$type,
                                 ct_bmn_fin_exp$sa, ct_bmn_fin_exp$la)
### Get rid of the columns I no longer need
all_sta_categories <- select(ct_bmn_fin_exp, -group, -type, -sa, -la, -wi) %>% 
  select(samp_ev, exp, category, tot_ct)
save(all_sta_categories,  file = "all_sta_categories")

## Make a file with just one station, ciliates only
sjr1_categories <- all_sta_categories %>% 
  filter(samp_ev == "SJR1") %>% 
  filter(grepl("ciliate", category)
         )
save(sjr1_categories, file = "sjr1_categories")
write_xlsx(sjr1_categories, "sjr1_categories.xlsx")

