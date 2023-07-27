############################################################################
#### ORGANISMS GROUPED BY SAMP_EV AND EXP WITH MEAN COUNTS AND BIOMASS ####
############################################################################
### Updated 10/31/22

### 9/3/22
##  This file uses ct_bmn_fin, made in Biomass_Means_Final.R to look at 
##  organisms by group and see the different sizes and their counts 
##  and biomass, to help me determine where to consolidate/aggregate 
##  the organisms

library(tidyverse)
library(readxl)
library(writexl)

load("data/ct_bmn_fin.Rdata")
load("data/ctbmn_grptyp.Rdata")
write.csv(ctbmn_grptyp, "data/ctbmn_grptyp.csv")
# ct_bmn_fin and ct_bmn_fin_exp are found in Bmass_Mn_Ct.R
# ct_bmn_fin includes all samples, including site water
# ct_bmn_fin_exp included only the experimentals samples, not site water.

### Combine group and type as one column because that's the level at which
##    I will group the different sizes

## First make a copy of the ct_bmn_fin data file to use for combining group and type
ct_bmn_grp_typ <- ct_bmn_fin
#  Combine group and type into one column and call it "grp_typ"
ct_bmn_grp_typ$grp_typ <- (paste(ct_bmn_grp_typ$Group, ct_bmn_grp_typ$type))
## Remove the columns I don't want and reorder the ones I keep
##  And then arrange the data frame alphabetically by grp_typ
##  so that all the same organisms are together and I can see the list
##  of them by their sizes
names(ct_bmn_grp_typ)
ctbmn_grptyp <- select(ct_bmn_grp_typ, samp_ev, exp, grp_typ, category, sa, la, wi, tot_ct, bmn_pgml) %>% 
  arrange(grp_typ)
save(ctbmn_grptyp, file = "data/ctbmn_grptyp.Rdata")
write_xlsx(ctbmn_grptyp, "ctbmn_grptyp.xlsx")
#  Ha ha! This added up all the organisms! 
#ctbmn_grptyp_orgct <- summarise(ctbmn_grptyp,(org_ct = sum(tot_ct)))

#unique(ctbmn_grptyp$grp_typ)
## Above shows 34 different grp_typ's

### Add a column that calculates the mean count across the three replicates
ctbmn_grptyp_mnct <- ctbmn_grptyp %>% 
  group_by(samp_ev, exp, grp_typ, category,sa, la, wi, tot_ct, bmn_pgml) %>%
  summarise(mn_ct = tot_ct/3) %>% 
  ungroup
## Format so that it has no decimal places
library(formattable) #found here https://www.tutorialspoint.com/how-to-format-all-decimal-places-in-an-r-vector-and-data-frame
ctbmn_grptyp_mnct$bmn_pgml<-formattable(ctbmn_grptyp_mnct$bmn_pgml,
                                        format="f",digits=0)
ctbmn_grptyp_mnct$mn_ct <- formattable(ctbmn_grptyp_mnct$mn_ct,
                                       format="f",digits=0) 

save(ctbmn_grptyp_mnct, file = "data/ctbmn_grptyp_mnct.Rdata")
write_xlsx(ctbmn_grptyp_mnct, "ctbmn_grptyp_mnct.xlsx")




# Get a sum of the total counts of the same organisms of the same size
ctbmn_grptyp_sumct <- ctbmn_grptyp %>%
  group_by(grp_typ, sa, la, wi) %>%
  summarise(tot_ct = sum(tot_ct))
# Get a sum of the mean biomass of the same organisms of the same size
ctbmn_grptyp_sumbmn <- ctbmn_grptyp %>%
  group_by(grp_typ, sa, la, wi) %>%
  summarise(tot_bmn = mean(bmn_pgml))

# Join the above two together so I can see the total counts and total
## mean biomass r
ctbm_grptyp_grp <- left_join(ctbmn_grptyp_sumct, ctbmn_grptyp_sumbmn)


### Make a file with one station to see what the mean counts are for the
## grp_typ column
sjr2_categories_grp_typ <- ctbmn_grptyp %>% 
  filter(samp_ev == "SJR1")
sjr2_categories_grp_typ$grp_typ
## Get a list of all the unique organism categories
unique(sjr1_categories$grp_typ)


## Make a file with just one station, using ctbmn_grptyp_mnct 
##  for mean counts
sjr1_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "SJR1")
## Get a list of all the unique organism categories
unique(sjr1_categories$grp_typ)

ybp1_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "YBP1")
unique(ybp1_categories$grp_typ)

wld2_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "WLD2")
unique(wld2_categories$grp_typ)

lsz2_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "LSZ2")
unique(lsz2_categories$grp_typ)

sjr2_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "SJR2")
unique(sjr2_categories$grp_typ)

ybp2_categories <- ctbmn_grptyp_mnct %>% 
  filter(samp_ev == "YBP2")
unique(ybp2_categories$grp_typ)

## Filter for each of the distinct grp-typ's so I can compare the sizes

grptyp_chl_act <- ctbmn_grptyp_mnct %>% 
  filter(grp_typ == "chlorophyte actinastrum")

#colSums (grptyp_chl_act$tot_ct)
write_xlsx(grptyp_chl_act, "results/Grp_typ_individuals/grptyp_chl_act.xlsx") %>% 
rowSums(grptyp_chl_act$tot_ct)
#data$new <- rowSums( data[,43:167] )
#mutate(ct_tot = sum(tot_ct, na.rm= F))#mutate(total = sum(1:3, ))#summarize(total = sum(tot_ct))

grptyp_chl_oth <- ctbmn_grptyp_mnct %>% 
  filter(grp_typ == "chlorophyte other")
write_xlsx(grptyp_chl_oth, "results/Grp_typ_individuals/grptyp_chl_oth.xlsx")

### Edit all the following to use ctbmn_grptyp_mnct as the source file,
##   since it includes the mean counts

grptyp_chl_scen <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte scenedesmus")
write_xlsx(grptyp_chl_scen, "results/Grp_typ_individuals/grptyp_chl_scen.xlsx")

grptyp_cil_cn <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate cone")
write_xlsx(grptyp_cil_cn, "results/Grp_typ_individuals/grptyp_cil_cn.xlsx")

grptyp_cil_odd <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate other odd shape")
write_xlsx(grptyp_cil_odd, "results/Grp_typ_individuals/grptyp_cil_odd.xlsx")

grptyp_cil_rnd <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate round")
write_xlsx(grptyp_cil_rnd, "results/Grp_typ_individuals/grptyp_cil_rnd.xlsx")

grptyp_cyan_aph <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria aphanizomenon")
write_xlsx(grptyp_cyan_aph, "results/Grp_typ_individuals/grptyp_cyan_aph.xlsx")

grptyp_cyan_dol <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria dolichospermum")
write_xlsx(grptyp_cyan_dol, "results/Grp_typ_individuals/grptyp_cyan_dol.xlsx")

grptyp_dia_cen <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom centric")
write_xlsx(grptyp_dia_cen, "results/Grp_typ_individuals/grptyp_dia_cen.xlsx")

grptyp_dia_chn <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom chain")
write_xlsx(grptyp_dia_chn, "results/Grp_typ_individuals/grptyp_dia_chn.xlsx")

grptyp_dia_pen <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate")
write_xlsx(grptyp_dia_pen, "results/Grp_typ_individuals/grptyp_dia_pen.xlsx")

grptyp_dia_pen_pleuro <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate pleurosigma")
write_xlsx(grptyp_dia_pen_pleuro, "results/Grp_typ_individuals/grptyp_dia_pen_pleuro.xlsx")

grptyp_fla_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate other")
write_xlsx(grptyp_fla_oth, "results/Grp_typ_individuals/grptyp_fla_oth.xlsx")

grptyp_fla_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate NA")
write_xlsx(grptyp_fla_na, "results/Grp_typ_individuals/grptyp_fla_na.xlsx")

grptyp_och_syn <- ctbmn_grptyp %>% 
  filter(grp_typ == "ochrophyte synura")
write_xlsx(grptyp_och_syn, "results/Grp_typ_individuals/grptyp_och_syn.xlsx")

grptyp_unid_flachl <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified flagchloro")
write_xlsx(grptyp_unid_flachl, "results/Grp_typ_individuals/grptyp_unid_flachl.xlsx")

grptyp_unid_rot <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified rotifer")
write_xlsx(grptyp_unid_rot, "results/Grp_typ_individuals/grptyp_unid_rot.xlsx")

grptyp_unid_rnd <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified round")
write_xlsx(grptyp_unid_rnd, "results/Grp_typ_individuals/grptyp_unid_rnd.xlsx")

grptyp_unid_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "unidentified NA")
write_xlsx(grptyp_unid_na, "results/Grp_typ_individuals/grptyp_unid_na.xlsx")

grptyp_chl_col <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte colonial")
write_xlsx(grptyp_chl_col, "results/Grp_typ_individuals/grptyp_chl_col.xlsx")

grptyp_cil_obl <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate oblong")
write_xlsx(grptyp_cil_obl, "results/Grp_typ_individuals/grptyp_cil_obl.xlsx")

grptyp_fla_col <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate colonial")
write_xlsx(grptyp_fla_col, "results/Grp_typ_individuals/grptyp_fla_col.xlsx")

grptyp_fla_cryp <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate cryptomonas")
write_xlsx(grptyp_fla_cryp, "results/Grp_typ_individuals/grptyp_fla_cryp.xlsx")

grptyp_fla_cn <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate in a cone")
write_xlsx(grptyp_fla_cn, "results/Grp_typ_individuals/grptyp_fla_cn.xlsx")

grptyp_tin_ag <- ctbmn_grptyp %>% 
  filter(grp_typ == "tintinnid agglutinated")
write_xlsx(grptyp_tin_ag, "results/Grp_typ_individuals/grptyp_tin_ag.xlsx")

grptyp_cil_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "ciliate other")
write_xlsx(grptyp_cil_oth, "results/Grp_typ_individuals/grptyp_cil_oth.xlsx")

grptyp_cyan_na <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria NA")
write_xlsx(grptyp_cyan_na, "results/Grp_typ_individuals/grptyp_cyan_na.xlsx")

grptyp_dia_egg <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom egg shaped")
write_xlsx(grptyp_dia_egg, "results/Grp_typ_individuals/grptyp_dia_egg.xlsx")

grptyp_dia_pen_cyl <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom cylindrotheca pennate")
write_xlsx(grptyp_dia_pen_cyl, "results/Grp_typ_individuals/grptyp_dia_pen_cyl.xlsx")

grptyp_dino_per <- ctbmn_grptyp %>% 
  filter(grp_typ == "dinoflagellate peridinium")
write_xlsx(grptyp_dino_per, "results/Grp_typ_individuals/grptyp_dino_per.xlsx")

grptyp_tin_hya <- ctbmn_grptyp %>% 
  filter(grp_typ == "tintinnid hyaline")
write_xlsx(grptyp_tin_hya, "results/Grp_typ_individuals/grptyp_tin_hya.xlsx")

grptyp_dia_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom other")
write_xlsx(grptyp_dia_oth, "results/Grp_typ_individuals/grptyp_dia_oth.xlsx")

grptyp_cyan_oth <- ctbmn_grptyp %>% 
  filter(grp_typ == "cyanobacteria other")
write_xlsx(grptyp_cyan_oth, "results/Grp_typ_individuals/grptyp_cyan_oth.xlsx")

grptyp_flag_eug <- ctbmn_grptyp %>% 
  filter(grp_typ == "flagellate euglenid")
write_xlsx(grptyp_flag_eug, "results/Grp_typ_individuals/grptyp_flag_eug.xlsx")

grptyp_dia_pen_frag <- ctbmn_grptyp %>% 
  filter(grp_typ == "diatom pennate fragillaria")
write_xlsx(grptyp_dia_pen_frag, "results/Grp_typ_individuals/grptyp_dia_pen_frag.xlsx")

grptyp_chl_ped <- ctbmn_grptyp %>% 
  filter(grp_typ == "chlorophyte pediastrum")
write_xlsx(grptyp_chl_ped, "results/Grp_typ_individuals/grptyp_chl_ped.xlsx")
           
summarise(grptyp_chl_col, sum(tot_ct))         
summarise(grptyp_chl_col, mean(filter(sa, la, bmn_pgml)))       


unique(volbio_all_no0$Group)
unique(ctbmn_grptyp$grp_typ)
          