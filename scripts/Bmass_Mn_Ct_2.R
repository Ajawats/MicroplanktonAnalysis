### 9/1/22 Using the code from 100x_BioPerVol_2_with_zeros.R to rerun everything 
##  with zeros to see if it will then include in later analyses the zero counts 
##  for organisms that weren't found in the T24 samples.

### 8/5/22
##  This file is the cleaned version of MeansTests.R, that takes the excel data that has
##   the biomass calculations, for calculating the mean biomass per organism/sample.
##   First, I made a data frame that sums up the total counts of organism/sample,
##   Then I made a data frame that calculates the mean biomass of organism/sample,
##   Then I did a left_join to combine them.



library(tidyverse)
library(readxl)
library(writexl)


load("bmass_fin_exp_v2")
count_sum_rep_fin_v2 <- bmass_fin_exp_v2 %>% 
  group_by(samp_date, samp_ev, exp, rep, group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

## Calculate the mean biomass of specific organism per sample 
##  (i.e., SJR1 FC centric diatom 24.5 x 24.5), being sure to keep
##  all the columns necessary for doing the clearance rates--!!! NO!!
## This didn't output the means, maybe because it was trying to still
##  keep the specific data in the other columns.

bmean_rep_fin_v2 <- bmass_fin_exp_v2 %>% 
  group_by(samp_date, samp_ev, exp, rep, group, type, sa, la, wi)%>% 
  summarize(bmn = mean(bio_per_vol_pgc_ml)) %>% 
  ungroup()
#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Right_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin_v2 <- left_join(count_sum_rep_fin_v2, bmean_rep_fin_v2)
write_xlsx(ct_bmn_fin, "ct_bmn_fin.xlsx")

## Do all this below using bmass_fin_exp, for the data frame that
##  doesn't have site water samples
load("bmass_fin_exp_v2")
count_sum_rep_fin_exp_v2 <- bmass_fin_exp_v2 %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(tot_ct = sum(counts)) %>% 
  ungroup()
#write_xlsx(count_sum_rep, "count_sum_rep.xlsx")

bmean_rep_fin_exp_v2 <- bmass_fin_exp_v2 %>% 
  group_by(samp_date, samp_ev, exp, group, type, sa, la, wi)%>% 
  summarize(bmn_pgcml = mean(bio_per_vol_pgc_ml)) %>%
  ungroup()

## Format the biomass column so that it has no decimal places
## Note: this seemed to change the format it was shown in, but when
## saving as an excel doc, it showed many decimal places, so I
## just changed the decimal places on that column. So I'm not sure
## this is the best function for this.

library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
bmean_rep_fin_exp_v2$bmn_pgcml<-formattable(bmean_rep_fin_exp_v2$bmn_pgcml,format="f",digits=0)
bmean_rep_fin_exp_v2


#write_xlsx(bmean_rep, "bmean_rep.xlsx")

##  Right_join to join the two data frames so that both total
##   counts and mean biomass are on the same data frame

ct_bmn_fin_exp_v2 <- left_join(count_sum_rep_fin_exp_v2, bmean_rep_fin_exp_v2)

save(ct_bmn_fin_exp, file = "ct_bmn_fin_exp")
write_xlsx(ct_bmn_fin_exp, "ct_bmn_fin_exp.xlsx")


## 8/29/22, Now combine group, type, sa, la, wi, into one column and call it "category"
ct_bmn_fin_exp_v2$category <- paste(ct_bmn_fin_exp_v2$group, ct_bmn_fin_exp_v2$type,
                                 ct_bmn_fin_exp_v2$sa, ct_bmn_fin_exp_v2$la)
### Get rid of the columns I no longer need
all_sta_categories_v2 <- select(ct_bmn_fin_exp_v2, -group, -type, -sa, -la, -wi) %>% 
  select(samp_ev, exp, category, tot_ct)
#save(all_sta_categories,  file = "all_sta_categories")

## Make a file with just one station, ciliates only
sjr1_categories_v2<- all_sta_categories_v2 %>% 
  filter(samp_ev == "SJR1") %>% 
  filter(grepl("ciliate", category)
  )
## order this file by category so the specific organisms are together,
## among all the experiments
sjr1_categories_v2_cat_ord <- sjr1_categories_v2[order(sjr1_categories_v2$category),]

library(ggtext)
library(RColorBrewer)
library(ggbreak)
library(ggrepel)

### Plot it to see the zero count entries (copied from SJR1_Plot_test
ggplot(sjr1_categories_v2_cat_ord)+
  (aes(fill = exp, y= tot_ct, x = category)) +
  geom_col(position = position_dodge())+
  ylim(0, 300) +
  #geom_label_repel(aes(label = tot_ct), size = 3, vjust = 1.5) +
  # to add a black outline where the counts are zero
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(y = "Total Counts",
       x = "Organism Category",
       title = "SJR1 Ciliates",
       fill = "Experimental Sample") 




